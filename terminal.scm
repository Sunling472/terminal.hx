;; terminal.scm -- floating terminal for helix + steel.
;;
;; Two backends:
;;
;;   ghostty (default): bin/hx-term (built from hx-term.c in this package),
;;     a small helper linked against libghostty-vt.
;;     It owns a real pty and the VT state (grid, alt screen, modes, answers to
;;     terminal queries) and streams the visible grid to this plugin.
;;       => fish/zsh/bash are fully interactive, vim/htop/curses work,
;;          resize and Ctrl-C are handled through the pty.
;;     Colors: SGR/palette is resolved to RGB by the helper and sent as style
;;     runs; default-styled text follows the hx theme.
;;     Keys are forwarded as-is; Ctrl-4 (Ctrl-\\) hides the popup and keeps the
;;     session running, Ctrl-D kills it. Esc goes to the child.
;;
;;   pipe (fallback, pure steel, no pty): shell with piped stdio plus a local
;;     line editor and scrollback. No isatty, no full-screen apps, no job
;;     control. Esc hides (the shell keeps running), Ctrl-D kills, Enter
;;     submits, Up/Down history, Tab inserts two spaces, PgUp/PgDn scroll,
;;     Ctrl-L clears the output, Ctrl-U/Ctrl-C clear the input line.
;;
;; The backend is picked automatically: ghostty when bin/hx-term exists,
;; pipe otherwise.
;;
;; Building the helper (inside this package directory):
;;   make            # gcc -O2 -o bin/hx-term hx-term.c -lghostty-vt -lutil
;;   (headers and the .so ship with the ghostty package; libghostty-vt 1.0+
;;    has a stable C ABI)
;;
;; Configuration (init.scm, before opening):
;;   (terminal-config #:shell "fish")                     ; default: $SHELL, else bash
;;   (terminal-config #:shell "bash" #:args '("--noprofile" "--norc"))
;;   (terminal-config #:backend 'ghostty)                 ; 'auto | 'ghostty | 'pipe
;;   (terminal-config #:helper "/path/to/hx-term")
;;   (terminal-config #:scrollback 8000 #:tick-fast 16 #:tick-slow 120)
;;
;; fish note (pipe backend only): fish reads stdin in blocks, so over a plain
;; pipe it would only run commands once stdin hits EOF. In pipe mode fish is
;; therefore started as
;;     fish -c 'while read -l l; eval $l; end'
;; The ghostty backend runs fish normally, interactively.
;;
;; Known cosmetic limit: rows are truncated by character count, so very wide
;; glyphs (CJK, some emoji) can overrun the right border by a column or two.
;;
;; Bind it from init.scm:
;;   (require "terminal.scm")
;;   (keymap (global) (normal (space (t ":terminal-open"))))

(require-builtin steel/process)
(require-builtin steel/threads)
(require-builtin steel/ports)

(require "helix/misc.scm")
(require "helix/components.scm")

(provide terminal-open terminal-config)

;; ---------------------------------------------------------------------------
;; state
;; ---------------------------------------------------------------------------

(define *term-name* "helix-terminal")

(define *term-active?* #f)
(define *term-started?* #f)     ; child spawned at least once
(define *term-exited?* #f)      ; child hit EOF
(define *term-child* #f)
(define *term-stdin* #f)
(define *term-channel* #f)

(define *term-lines* '())       ; newest first, finished lines
(define *term-line-count* 0)
(define *term-cur* "")          ; partial line still being written
(define *term-max-lines* 4000)
(define *term-trim-to* 3000)

(define *term-input* "")
(define *term-caret* 0)
(define *term-history* '())     ; newest first
(define *term-hist-idx* 0)      ; 0 = editing a fresh line

(define *term-scroll* 0)        ; lines scrolled up from the bottom
(define *term-out-h* 10)        ; visible output rows, set while rendering

(define *term-cols* 80)
(define *term-rows* 24)

(define *term-status* "starting...")
(define *term-cwd* "")

;; tick backoff: render fast while the child is chatty, slow down when idle
(define *term-got-data?* #f)
(define *term-idle-ticks* 0)

;; tiny ANSI state machine for the pipe backend: text | esc | csi | osc | osc-esc
(define *term-parse* 'text)

;; ghostty backend (hx-term helper): grid is maintained by libghostty-vt
(define *term-gg?* #f)          ; currently running the hx-term helper
(define *term-gg-rows* '())     ; committed visible rows, top to bottom
(define *term-gg-pending* '())  ; rows of the frame being received (reversed)
(define *term-gg-cols* 0)
(define *term-gg-nrows* 0)
(define *term-gg-cx* 0)
(define *term-gg-cy* 0)
(define *term-gg-cursor?* #f)
(define *term-gg-alt?* #f)
(define *term-gg-size* #f)      ; last (cols . rows) sent to the helper
;; style table: id -> (id fgR fgG fgB bgR bgG bgB attrs underline)
(define *term-gg-style-defs* (hash))
(define *term-gg-style-cache* (hash))

(define *term-timer?* #f)       ; a tick chain is scheduled

;; ---------------------------------------------------------------------------
;; configuration
;; ---------------------------------------------------------------------------

;; shell       : string? or #f   (#f -> $SHELL, else "bash")
;; args        : list?  or #f    (#f -> per-shell default: fish gets an eval loop)
;; scrollback  : int?            lines kept in the buffer
;; tick-fast   : int?            ms between frames while output is flowing
;; tick-slow   : int?            max ms between frames when idle
(define *term-config*
  (hash 'shell #f
        'args #f
        'scrollback 4000
        'tick-fast 16
        'tick-slow 120
        'backend 'auto
        'helper #f))

;; steel has no getenv; parse /proc/self/environ (linux)
(define (term-env-var name)
  (with-handler
    (lambda (_) "")
    (let* ([p (open-input-file "/proc/self/environ")]
           [raw (read-port-to-string p)])
      (close-input-port p)
      (define prefix (string-append name "="))
      (define plen (string-length prefix))
      (define hit
        (filter (lambda (entry)
                  (and (>= (string-length entry) plen)
                       (equal? (substring entry 0 plen) prefix)))
                (split-many raw (string (integer->char 0)))))
      (if (null? hit)
          ""
          (substring (car hit) plen (string-length (car hit)))))))

(define (term-shell)
  (define configured (hash-ref *term-config* 'shell))
  (cond
    [(and (string? configured) (> (string-length configured) 0)) configured]
    [else
     (define from-env (term-env-var "SHELL"))
     (if (> (string-length from-env) 0) from-env "bash")]))

(define (term-helper-path)
  (define configured (hash-ref *term-config* 'helper))
  (if (and (string? configured) (> (string-length configured) 0))
      configured
      (string-append (term-env-var "HOME") "/.config/helix/terminal/bin/hx-term")))

(define (term-backend)
  (define configured (hash-ref *term-config* 'backend))
  (cond
    [(equal? configured 'pipe) 'pipe]
    [(equal? configured 'ghostty) 'ghostty]
    [else (if (is-file? (term-helper-path)) 'ghostty 'pipe)]))

(define (term-fish? shell)
  (equal? (file-name shell) "fish"))

;; fish buffers its stdin when it is a pipe, so drive it with a read/eval loop
;; to keep state and get output streamed as commands are entered.
(define (term-args-for shell)
  (define configured (hash-ref *term-config* 'args))
  (cond
    [(list? configured) configured]
    ;; only the pipe backend needs it: without a tty fish waits for EOF
    [(and (term-fish? shell) (equal? (term-backend) 'pipe))
     '("-c" "while read -l l; eval $l; end")]
    [else '()]))

;; ---------------------------------------------------------------------------
;; small list helpers (steel has take/drop, but keep it self contained)
;; ---------------------------------------------------------------------------

(define (term-take lst n)
  (if (or (null? lst) (<= n 0)) '() (cons (car lst) (term-take (cdr lst) (- n 1)))))

(define (term-drop lst n)
  (if (or (null? lst) (<= n 0)) lst (term-drop (cdr lst) (- n 1))))

(define (term-truncate s max-w)
  (if (<= (string-length s) max-w)
      s
      (string-append (substring s 0 (max 0 (- max-w 1))) "…")))

(define (term-repeat s n)
  (if (<= n 0) "" (string-append s (term-repeat s (- n 1)))))

;; ---------------------------------------------------------------------------
;; output buffer
;; ---------------------------------------------------------------------------

(define (term-push-line! line)
  (set! *term-lines* (cons line *term-lines*))
  (set! *term-line-count* (+ *term-line-count* 1))
  (when (> *term-line-count* *term-max-lines*)
    (set! *term-lines* (term-take *term-lines* *term-trim-to*))
    (set! *term-line-count* *term-trim-to*)))

(define (term-flush-partial!)
  (term-push-line! *term-cur*)
  (set! *term-cur* ""))

(define (term-clear-output!)
  (set! *term-lines* '())
  (set! *term-line-count* 0)
  (set! *term-cur* "")
  (set! *term-scroll* 0))

;; ---------------------------------------------------------------------------
;; ANSI parser (strip control sequences, honour \r \n \t \b)
;; ---------------------------------------------------------------------------

(define (term-feed! chunk)
  (for-each term-feed-char! (string->list chunk)))

(define (term-feed-char! c)
  (define code (char->integer c))
  (cond
    [(equal? *term-parse* 'text)
     (cond
       [(= code 10) (term-flush-partial!)]
       [(= code 13) (set! *term-cur* "")]
       [(= code 9) (set! *term-cur* (string-append *term-cur* "    "))]
       [(= code 8) (term-cur-backspace!)]
       [(= code 27) (set! *term-parse* 'esc)]
       [(< code 32) #f]
       [else
        (set! *term-cur* (string-append *term-cur* (string c)))
        ;; crude wrap: don't let one endless line eat all the memory
        (when (> (string-length *term-cur*) 4000) (term-flush-partial!))])]
    [(equal? *term-parse* 'esc)
     (cond
       [(= code 91) (set! *term-parse* 'csi)]
       [(= code 93) (set! *term-parse* 'osc)]
       [else (set! *term-parse* 'text)])]
    ;; CSI: skip until a final byte in 0x40..0x7e
    [(equal? *term-parse* 'csi)
     (when (and (>= code 64) (<= code 126)) (set! *term-parse* 'text))]
    [(equal? *term-parse* 'osc)
     (cond
       [(= code 7) (set! *term-parse* 'text)]
       [(= code 27) (set! *term-parse* 'osc-esc)]
       [else #f])]
    [(equal? *term-parse* 'osc-esc)
     (set! *term-parse* (if (= code 92) 'text 'osc))]
    [else #f]))

(define (term-cur-backspace!)
  (define n (string-length *term-cur*))
  (when (> n 0)
    (set! *term-cur* (substring *term-cur* 0 (- n 1)))))

;; ---------------------------------------------------------------------------
;; child process
;; ---------------------------------------------------------------------------

(define (term-reader-loop port sender)
  (let loop ()
    (define b (with-handler (lambda (_) 'eof) (read-byte port)))
    (cond
      [(equal? b 'eof) (channel/send sender 'eof)]
      [(eof-object? b) (channel/send sender 'eof)]
      [else
       (channel/send sender (string (integer->char b)))
       (loop)])))

(define (term-chomp s)
  (define n (string-length s))
  (cond
    [(and (> n 0) (= (char->integer (string-ref s (- n 1))) 10))
     (term-chomp (substring s 0 (- n 1)))]
    [(and (> n 0) (= (char->integer (string-ref s (- n 1))) 13))
     (term-chomp (substring s 0 (- n 1)))]
    [else s]))

(define (term-line-reader-loop port sender)
  (let loop ()
    (define line (with-handler (lambda (_) 'eof) (read-line-from-port port)))
    (cond
      [(string? line) (channel/send sender (list 'line (term-chomp line))) (loop)]
      [(equal? line 'eof) (channel/send sender 'eof)]
      [else (channel/send sender 'eof)])))

(define (term-spawn!)
  (set! *term-status* "spawning...")
  (set! *term-exited?* #f)
  (set! *term-gg-rows* '())
  (set! *term-gg-pending* '())
  (set! *term-gg-cursor?* #f)
  (set! *term-gg-size* #f)
  (set! *term-gg-style-defs* (hash))
  (set! *term-gg-style-cache* (hash))

  (define cwd (with-handler (lambda (_) "") (helix-find-workspace)))
  (set! *term-cwd* cwd)

  (define shell (term-shell))
  (define args (term-args-for shell))

  (if (equal? (term-backend) 'ghostty)
      (term-spawn-ghostty! shell args cwd)
      (term-spawn-pipe! shell args cwd)))

(define (term-spawn-pipe! shell args cwd)
  (set! *term-gg?* #f)
  (define builder
    (~> (command shell args)
        (with-env-var "TERM" "xterm-256color")
        (with-env-var "COLORTERM" "truecolor")
        (with-env-var "COLUMNS" (number->string *term-cols*))
        (with-env-var "LINES" (number->string *term-rows*))
        with-stdin-piped
        with-stdout-piped
        with-stderr-piped))

  (define builder-2
    (if (and (string? cwd) (> (string-length cwd) 0))
        (with-handler (lambda (_) builder) (with-current-dir builder cwd))
        builder))

  (define res (spawn-process builder-2))
  (cond
    [(Ok? res)
     (define child (Ok->value res))
     (define ch (channels/new))
     (set! *term-child* child)
     (set! *term-stdin* (child-stdin child))
     (set! *term-channel* ch)
     (set! *term-started?* #t)
     (set! *term-status* (string-append (file-name shell) " · " (if (string? cwd) cwd "")))
     (spawn-native-thread
      (lambda () (term-reader-loop (child-stdout child) (channels-sender ch))))
     (spawn-native-thread
      (lambda () (term-reader-loop (child-stderr child) (channels-sender ch))))]
    [else
     (set! *term-started?* #t)
     (set! *term-status* (string-append "failed to spawn " shell))
     (set! *term-exited?* #t)]))

(define (term-spawn-ghostty! shell args cwd)
  (define helper (term-helper-path))
  (define argv (append (list "--cols" (number->string *term-cols*)
                             "--rows" (number->string *term-rows*)
                             "--" shell)
                       args))
  (define builder
    (~> (command helper argv)
        with-stdin-piped
        with-stdout-piped
        with-stderr-piped))

  (define builder-2
    (if (and (string? cwd) (> (string-length cwd) 0))
        (with-handler (lambda (_) builder) (with-current-dir builder cwd))
        builder))

  (define res (spawn-process builder-2))
  (cond
    [(Ok? res)
     (define child (Ok->value res))
     (define ch (channels/new))
     (set! *term-child* child)
     (set! *term-stdin* (child-stdin child))
     (set! *term-channel* ch)
     (set! *term-gg?* #t)
     (set! *term-gg-size* (cons *term-cols* *term-rows*))
     (set! *term-started?* #t)
     (set! *term-status* (string-append (file-name shell) " · " (if (string? cwd) cwd "")))
     (spawn-native-thread
      (lambda () (term-line-reader-loop (child-stdout child) (channels-sender ch))))
     (spawn-native-thread
      (lambda () (term-line-reader-loop (child-stderr child) (channels-sender ch))))]
    [else
     (set! *term-started?* #t)
     (set! *term-status* (string-append "failed to spawn hx-term for " shell))
     (set! *term-exited?* #t)]))

(define (term-kill!)
  (when *term-child*
    (with-handler (lambda (_) #f) (subprocess-kill *term-child*)))
  (set! *term-child* #f)
  (set! *term-stdin* #f))

;; ---------------------------------------------------------------------------
;; timer: drain the channel, re-arm itself
;; ---------------------------------------------------------------------------

(define (term-starts-with? s prefix)
  (and (>= (string-length s) (string-length prefix))
       (equal? (substring s 0 (string-length prefix)) prefix)))

(define (term-space-index s start)
  (let loop ([i start])
    (cond
      [(>= i (string-length s)) #f]
      [(char=? (string-ref s i) #\space) i]
      [else (loop (+ i 1))])))

(define (term-char-index s ch)
  (let loop ([i 0])
    (cond
      [(>= i (string-length s)) #f]
      [(char=? (string-ref s i) ch) i]
      [else (loop (+ i 1))])))

;; a run looks like "<style-id>\x02<text>"
(define (term-parse-run piece)
  (define i (term-char-index piece (integer->char 2)))
  (if i
      (let ([id (string->number (substring piece 0 i))])
        (cons (if (number? id) id 0)
              (substring piece (+ i 1) (string-length piece))))
      (cons 0 piece)))

(define (term-bit? n k)
  (not (= 0 (bitwise-and n (expt 2 k)))))

(define (term-build-style def)
  (define st (theme-scope-ref "ui.text"))
  (define fg-r (list-ref def 1))
  (define fg-g (list-ref def 2))
  (define fg-b (list-ref def 3))
  (define bg-r (list-ref def 4))
  (define bg-g (list-ref def 5))
  (define bg-b (list-ref def 6))
  (define attrs (list-ref def 7))
  (define ul (list-ref def 8))
  (when (>= fg-r 0) (set! st (style-fg st (Color/rgb fg-r fg-g fg-b))))
  (when (>= bg-r 0) (set! st (style-bg st (Color/rgb bg-r bg-g bg-b))))
  (when (term-bit? attrs 0) (set! st (style-with-bold st)))
  (when (term-bit? attrs 1) (set! st (style-with-italics st)))
  (when (term-bit? attrs 2) (set! st (style-with-dim st)))
  (when (term-bit? attrs 3) (set! st (style-with-slow-blink st)))
  (when (term-bit? attrs 4) (set! st (style-with-reversed st)))
  (when (term-bit? attrs 5) (set! st (style-with-hidden st)))
  (when (term-bit? attrs 6) (set! st (style-with-crossed-out st)))
  (cond
    [(= ul 1) (set! st (style-underline-style st Underline/Line))]
    [(= ul 2) (set! st (style-underline-style st Underline/DoubleLine))]
    [(= ul 3) (set! st (style-underline-style st Underline/Curl))]
    [(= ul 4) (set! st (style-underline-style st Underline/Dotted))]
    [(= ul 5) (set! st (style-underline-style st Underline/Dashed))]
    [else #f])
  st)

(define (term-style-for id)
  (cond
    [(equal? id 0) (theme-scope-ref "ui.text")]
    [(hash-contains? *term-gg-style-cache* id)
     (hash-ref *term-gg-style-cache* id)]
    [else
     (define def (hash-ref *term-gg-style-defs* id))
     (define st (if (list? def) (term-build-style def) (theme-scope-ref "ui.text")))
     (set! *term-gg-style-cache* (hash-insert *term-gg-style-cache* id st))
     st]))

;; hx-term line protocol: T style / F header / R row / E end / X exit
(define (term-gg-handle-line! line)
  (cond
    [(term-starts-with? line "T ")
     (define parts (split-many line " "))
     (when (>= (length parts) 10)
       (define nums (filter number? (map string->number (cdr parts))))
       (when (>= (length nums) 9)
         (set! *term-gg-style-defs*
               (hash-insert *term-gg-style-defs* (list-ref nums 0) nums))
         (set! *term-gg-style-cache* (hash))))]
    [(term-starts-with? line "F ")
     (define parts (split-many line " "))
     (when (>= (length parts) 7)
       (set! *term-gg-cols* (string->number (list-ref parts 1)))
       (set! *term-gg-nrows* (string->number (list-ref parts 2)))
       (set! *term-gg-cx* (string->number (list-ref parts 3)))
       (set! *term-gg-cy* (string->number (list-ref parts 4)))
       (set! *term-gg-cursor?* (equal? (list-ref parts 5) "1"))
       (set! *term-gg-alt?* (equal? (list-ref parts 6) "1"))
       (set! *term-gg-pending* '()))]
    [(term-starts-with? line "R ")
     (define sp (term-space-index line 2))
     (when sp
       (define rest (substring line (+ sp 1) (string-length line)))
       (define pieces
         (filter (lambda (p) (> (string-length p) 0))
                 (split-many rest (string (integer->char 1)))))
       (define runs (map term-parse-run pieces))
       (set! *term-gg-pending* (cons runs *term-gg-pending*)))]
    [(equal? line "E")
     (set! *term-gg-rows* (reverse *term-gg-pending*))
     (set! *term-gg-pending* '())
     (set! *term-got-data?* #t)]
    [(term-starts-with? line "X ")
     (set! *term-exited?* #t)
     (set! *term-status* "process exited -- Enter restarts, Ctrl-D kills")]
    [else
     (when (> (string-length line) 0)
       (set! *term-status* (string-append "hx-term: " line)))]))

(define (term-drain!)
  (define ch *term-channel*)
  (when ch
    (let loop ()
      (define v (channel/try-recv (channels-receiver ch)))
      (cond
        [(empty-channel-object? v) #f]
        [(disconnected-channel-object? v) #f]
        [(equal? v 'eof)
         (unless *term-exited?*
           (unless *term-gg?* (term-flush-partial!))
           (set! *term-exited?* #t)
           (set! *term-status* "process exited -- Enter restarts, Ctrl-D kills"))]
        [(and (list? v) (equal? (car v) 'line))
         (term-gg-handle-line! (list-ref v 1))
         (loop)]
        [(string? v)
         (set! *term-got-data?* #t)
         (term-feed! v)
         (loop)]
        [else (loop)]))))

(define (term-next-delay)
  (define fast (hash-ref *term-config* 'tick-fast))
  (define slow (hash-ref *term-config* 'tick-slow))
  (if *term-got-data?*
      (begin (set! *term-got-data?* #f)
             (set! *term-idle-ticks* 0)
             fast)
      (begin (set! *term-idle-ticks* (+ *term-idle-ticks* 1))
             (min slow (+ fast (* 8 *term-idle-ticks*))))))

(define (term-tick)
  (term-drain!)
  ;; keep draining while the popup is hidden so a running session does not
  ;; pile up frames in the channel; stop once it is killed/exited
  (if (or *term-active?* (and *term-started?* (not *term-exited?*)))
      (enqueue-thread-local-callback-with-delay (term-next-delay) term-tick)
      (set! *term-timer?* #f)))

;; ---------------------------------------------------------------------------
;; input line editing
;; ---------------------------------------------------------------------------

(define (term-insert-string! s)
  (define n (string-length *term-input*))
  (define i *term-caret*)
  (set! *term-input*
        (string-append (substring *term-input* 0 i) s (substring *term-input* i n)))
  (set! *term-caret* (+ i (string-length s))))

(define (term-insert-char! c)
  (term-insert-string! (string c)))

(define (term-delete-before!)
  (define n (string-length *term-input*))
  (define i *term-caret*)
  (when (> i 0)
    (set! *term-input*
          (string-append (substring *term-input* 0 (- i 1)) (substring *term-input* i n)))
    (set! *term-caret* (- i 1))))

(define (term-delete-at!)
  (define n (string-length *term-input*))
  (define i *term-caret*)
  (when (< i n)
    (set! *term-input*
          (string-append (substring *term-input* 0 i) (substring *term-input* (+ i 1) n)))))

(define (term-caret-left!)
  (when (> *term-caret* 0) (set! *term-caret* (- *term-caret* 1))))

(define (term-caret-right!)
  (when (< *term-caret* (string-length *term-input*))
    (set! *term-caret* (+ *term-caret* 1))))

(define (term-history-prev!)
  (define n (length *term-history*))
  (when (< *term-hist-idx* n)
    (set! *term-hist-idx* (+ *term-hist-idx* 1))
    (set! *term-input* (list-ref *term-history* (- *term-hist-idx* 1)))
    (set! *term-caret* (string-length *term-input*))))

(define (term-history-next!)
  (when (> *term-hist-idx* 0)
    (set! *term-hist-idx* (- *term-hist-idx* 1))
    (if (= *term-hist-idx* 0)
        (begin (set! *term-input* "") (set! *term-caret* 0))
        (begin
          (set! *term-input* (list-ref *term-history* (- *term-hist-idx* 1)))
          (set! *term-caret* (string-length *term-input*))))))

;; ---------------------------------------------------------------------------
;; sending input to the child
;; ---------------------------------------------------------------------------

(define (term-write! s)
  (when *term-stdin*
    (with-handler (lambda (_) #f)
                  (begin (write-string s *term-stdin*) (flush-output-port *term-stdin*)))))

(define (term-submit!)
  (define line *term-input*)
  (term-write! (string-append line "\n"))
  (when (> (string-length line) 0)
    (set! *term-history* (cons line *term-history*)))
  (set! *term-hist-idx* 0)
  (when (> (string-length *term-cur*) 0) (term-flush-partial!))
  (term-push-line! (string-append "❯ " line))
  (set! *term-input* "")
  (set! *term-caret* 0)
  (set! *term-scroll* 0)
  (set! *term-idle-ticks* 0))

(define (term-enter!)
  (cond
    ;; dead shell: start a fresh one and run whatever was typed
    [*term-exited?*
     (term-kill!)
     (term-clear-output!)
     (set! *term-started?* #f)
     (term-spawn!)
     (term-submit!)]
    [else (term-submit!)]))

(define (term-scroll-by! delta max-scroll)
  (set! *term-scroll* (max 0 (min max-scroll (+ *term-scroll* delta)))))

;; ---------------------------------------------------------------------------
;; component
;; ---------------------------------------------------------------------------

(define (term-input-window avail)
  ;; returns (list caret-col-in-window shown-string)
  (define len (string-length *term-input*))
  (define start (if (> *term-caret* avail) (- *term-caret* avail) 0))
  (define start (min start len))
  (define shown (substring *term-input* start len))
  (list (min *term-caret* (- len start))
        (term-truncate shown avail)))

(define (term-metrics rect)
  (define tw (area-width rect))
  (define th (area-height rect))
  (define w (min tw (max 30 (quotient (* tw 9) 10))))
  (define h (min th (max 8 (quotient (* th 8) 10))))
  (list (max 0 (quotient (- tw w) 2)) (max 0 (quotient (- th h) 3)) w h))

(define (term-draw-header! frame x y w)
  (define right (string-append (number->string *term-cols*) "×"
                               (number->string *term-rows*)))
  (frame-set-string! frame (+ x 2) y "terminal" (theme-scope-ref "ui.text"))
  (frame-set-string! frame (+ x 11) y
                     (term-truncate *term-status*
                                    (max 0 (- w 12 (string-length right))))
                     (style-with-dim (theme-scope-ref "ui.text")))
  (frame-set-string! frame (max (+ x 2) (- (+ x w -2) (string-length right))) y
                     right
                     (style-with-dim (theme-scope-ref "ui.text"))))

(define (term-render _state rect frame)
  (define m (term-metrics rect))
  (define x (list-ref m 0))
  (define y (list-ref m 1))
  (define w (list-ref m 2))
  (define h (list-ref m 3))

  (define bg (theme-scope-ref "ui.background"))
  (define text-style (theme-scope-ref "ui.text"))
  (define dim-style (style-with-dim (theme-scope-ref "ui.text")))

  (define box (area x y w h))
  (buffer/clear-with frame box bg)
  (block/render frame box (make-block bg text-style "all" "plain"))

  ;; size the grid before the first spawn so the helper starts correctly
  (unless *term-started?*
    (if (equal? (term-backend) 'ghostty)
        (begin (set! *term-cols* (max 1 (- w 2)))
               (set! *term-rows* (max 1 (- h 3))))
        (begin (set! *term-cols* (max 1 (- w 4)))
               (set! *term-rows* (max 1 (- h 5))))))

  (unless *term-started?* (term-spawn!))
  (when *term-exited?* (term-drain!))

  (if *term-gg?*
      (term-draw-grid! frame x y w h text-style)
      (term-draw-pipe! frame x y w h text-style dim-style))

  (term-draw-header! frame x (+ y 1) w))

;; libghostty backend: a plain grid, no local input line (the shell draws it)
(define (term-draw-grid! frame x y w h text-style)
  (define content-x (+ x 1))
  (define content-w (max 1 (- w 2)))
  (define out-h (max 1 (- h 3)))
  (set! *term-cols* content-w)
  (set! *term-rows* out-h)
  (set! *term-out-h* out-h)

  ;; keep the helper's grid and the pty window in sync with the popup
  (when *term-started?*
    (define sz (cons content-w out-h))
    (unless (equal? *term-gg-size* sz)
      (set! *term-gg-size* sz)
      (term-send-line! (string-append "S "
                                      (number->string content-w) " "
                                      (number->string out-h)))))

  (let loop ([lst *term-gg-rows*] [row 0])
    (unless (or (null? lst) (>= row out-h))
      (term-draw-runs! frame content-x (+ y 2 row) (car lst) content-w)
      (loop (cdr lst) (+ row 1)))))

;; paint one row made of (style-id . text) runs, clipped to width
(define (term-draw-runs! frame x y runs width)
  (let loop ([rs runs] [col 0])
    (unless (or (null? rs) (>= col width))
      (define run (car rs))
      (define text (cdr run))
      (define avail (- width col))
      (define shown (if (> (string-length text) avail)
                        (substring text 0 avail)
                        text))
      (unless (= (string-length shown) 0)
        (frame-set-string! frame (+ x col) y shown
                           (term-style-for (car run))))
      (loop (cdr rs) (+ col (string-length text))))))

;; pipe backend: local buffer + local input line
(define (term-draw-pipe! frame x y w h text-style dim-style)
  (define content-x (+ x 2))
  (define content-w (max 1 (- w 4)))
  (define out-h (max 1 (- h 5)))
  (set! *term-cols* content-w)
  (set! *term-rows* out-h)
  (set! *term-out-h* out-h)

  (frame-set-string! frame content-x (+ y 2) (term-repeat "─" content-w) dim-style)

  ;; output: newest at the bottom, offset by *term-scroll*
  (define all (cons *term-cur* *term-lines*))
  (define visible (term-take (term-drop all *term-scroll*) out-h))
  (let loop ([lst (reverse visible)] [row 0])
    (unless (null? lst)
      (frame-set-string! frame content-x (+ y 3 row)
                         (term-truncate (car lst) content-w)
                         text-style)
      (loop (cdr lst) (+ row 1))))

  ;; input line, scrolled horizontally so the caret stays visible
  (define in-y (+ y (- h 2)))
  (define avail (max 0 (- content-w 2)))
  (define win (term-input-window avail))
  (frame-set-string! frame content-x in-y "❯ " text-style)
  (frame-set-string! frame (+ content-x 2) in-y (list-ref win 1) text-style))

(define (term-cursor _state rect)
  (define m (term-metrics rect))
  (define x (list-ref m 0))
  (define y (list-ref m 1))
  (define w (list-ref m 2))
  (define h (list-ref m 3))
  (if *term-gg?*
      (position (+ y 2 (min *term-gg-cy* (max 0 (- h 4))))
                (+ x 1 (min *term-gg-cx* (max 0 (- w 3)))))
      (let* ([avail (max 0 (- w 6))]
             [win (term-input-window avail)])
        (position (+ y (- h 2)) (+ x 4 (list-ref win 0))))))

;; ---------------------------------------------------------------------------
;; ghostty backend: keys are forwarded as bytes, no local editing
;; ---------------------------------------------------------------------------

(define (term-send-line! s)
  (term-write! (string-append s "\n")))

(define (term-send-bytes! bytes)
  (term-send-line!
   (string-append "K "
                  (if (null? bytes)
                      ""
                      (string-join (map number->string bytes) " ")))))

(define (term-ascii-bytes s)
  (map char->integer (string->list s)))

(define (term-char-bytes ch)
  (define c (char->integer ch))
  (cond
    [(< c 128) (list c)]
    [(< c 2048)
     (list (+ 192 (quotient c 64)) (+ 128 (remainder c 64)))]
    [(< c 65536)
     (list (+ 224 (quotient c 4096))
           (+ 128 (remainder (quotient c 64) 64))
           (+ 128 (remainder c 64)))]
    [else
     (list (+ 240 (quotient c 262144))
           (+ 128 (remainder (quotient c 4096) 64))
           (+ 128 (remainder (quotient c 64) 64))
           (+ 128 (remainder c 64)))]))

(define (term-ctrl-byte ch)
  (define c (char->integer ch))
  (cond
    [(and (>= c 97) (<= c 122)) (- c 96)]
    [(and (>= c 65) (<= c 90)) (- c 64)]
    [(char=? ch #\space) 0]
    [(char=? ch #\@) 0]
    [(char=? ch #\[) 27]
    [(char=? ch #\\) 28]
    [(char=? ch #\]) 29]
    [(char=? ch #\^) 30]
    [(char=? ch #\_) 31]
    [else #f]))

(define (term-gg-key-bytes event)
  (define ch (key-event-char event))
  (define mod (key-event-modifier event))
  (cond
    [(key-event-enter? event) (list 13)]
    [(key-event-tab? event) (list 9)]
    [(key-event-backspace? event) (list 127)]
    [(key-event-escape? event) (list 27)]
    [(key-event-up? event) (cons 27 (term-ascii-bytes "[A"))]
    [(key-event-down? event) (cons 27 (term-ascii-bytes "[B"))]
    [(key-event-right? event) (cons 27 (term-ascii-bytes "[C"))]
    [(key-event-left? event) (cons 27 (term-ascii-bytes "[D"))]
    [(key-event-home? event) (cons 27 (term-ascii-bytes "[H"))]
    [(key-event-end? event) (cons 27 (term-ascii-bytes "[F"))]
    [(key-event-page-up? event) (cons 27 (term-ascii-bytes "[5~"))]
    [(key-event-page-down? event) (cons 27 (term-ascii-bytes "[6~"))]
    [(key-event-delete? event) (cons 27 (term-ascii-bytes "[3~"))]
    [(key-event-insert? event) (cons 27 (term-ascii-bytes "[2~"))]
    [(not (char? ch)) '()]
    [(equal? mod key-modifier-ctrl)
     (define b (term-ctrl-byte ch))
     (if b (list b) '())]
    [(equal? mod key-modifier-alt)
     (append (list 27) (term-char-bytes ch))]
    [else (term-char-bytes ch)]))

(define (term-gg-handle-event event)
  (define ch (key-event-char event))
  (define mod (key-event-modifier event))
  (cond
    [*term-exited?*
     (cond
       [(key-event-escape? event) (term-destroy!) event-result/close]
       [(key-event-enter? event)
        (term-kill!)
        (set! *term-started?* #f)
        (term-spawn!)
        event-result/consume]
       [else event-result/consume])]
    ;; a terminal sends Ctrl-\ as Ctrl-4 (0x1c); either spelling hides the popup
    [(and (char? ch)
          (equal? mod key-modifier-ctrl)
          (or (char=? ch #\\) (char=? ch #\4)))
     (term-hide!)
     event-result/close]
    ;; Ctrl-D kills the session
    [(and (char? ch) (char=? ch #\d) (equal? mod key-modifier-ctrl))
     (term-destroy!)
     event-result/close]
    [else
     (define bytes (term-gg-key-bytes event))
     (unless (null? bytes) (term-send-bytes! bytes))
     event-result/consume]))

(define (term-handle-event _state event)
  (cond
    [(paste-event? event)
     (define s (paste-event-string event))
     (when (string? s)
       (if *term-gg?*
           (term-send-bytes! (apply append (map term-char-bytes (string->list s))))
           (term-insert-string! s)))
     event-result/consume]
    [(not (key-event? event)) event-result/consume]
    [*term-gg?* (term-gg-handle-event event)]
    [else (term-pipe-handle-event event)]))

(define (term-pipe-handle-event event)
  (cond
    [(key-event-escape? event) (term-hide!) event-result/close]
    [(key-event-enter? event) (term-enter!) event-result/consume]
    [(key-event-backspace? event) (term-delete-before!) event-result/consume]
    [(key-event-delete? event) (term-delete-at!) event-result/consume]
    [(key-event-left? event) (term-caret-left!) event-result/consume]
    [(key-event-right? event) (term-caret-right!) event-result/consume]
    [(key-event-home? event) (set! *term-caret* 0) event-result/consume]
    [(key-event-end? event) (set! *term-caret* (string-length *term-input*)) event-result/consume]
    [(key-event-up? event) (term-history-prev!) event-result/consume]
    [(key-event-down? event) (term-history-next!) event-result/consume]
    [(key-event-tab? event) (term-insert-string! "  ") event-result/consume]
    [(key-event-page-up? event)
     (term-scroll-by! *term-out-h* (max 0 (- *term-line-count* *term-out-h*)))
     event-result/consume]
    [(key-event-page-down? event)
     (term-scroll-by! (- *term-out-h*) (max 0 (- *term-line-count* *term-out-h*)))
     event-result/consume]
    [else
     (define ch (key-event-char event))
     (define mod (key-event-modifier event))
     (cond
       [(not (char? ch)) event-result/consume]
       [(equal? mod key-modifier-ctrl)
        (cond
          [(char=? ch #\l) (term-clear-output!) event-result/consume]
          [(char=? ch #\u) (set! *term-input* "") (set! *term-caret* 0) event-result/consume]
          [(char=? ch #\c) (set! *term-input* "") (set! *term-caret* 0) event-result/consume]
          [(char=? ch #\d) (term-destroy!) event-result/close]
          [else event-result/consume])]
       [(equal? mod key-modifier-alt) (term-insert-char! ch) event-result/consume]
       [else (term-insert-char! ch) event-result/consume])]))

;; hide: close the popup, keep the shell (and its pty) running
(define (term-hide!)
  (set! *term-active?* #f)
  (when *term-gg?* (term-send-line! "P 1")))

;; destroy: kill the shell and forget the session
(define (term-destroy!)
  (set! *term-active?* #f)
  (term-kill!)
  (set! *term-started?* #f)
  (set! *term-exited?* #f)
  (set! *term-gg-rows* '())
  (set! *term-gg-pending* '())
  (set! *term-gg-cursor?* #f)
  (set! *term-gg-size* #f)
  (set! *term-gg-style-defs* (hash))
  (set! *term-gg-style-cache* (hash))
  (term-clear-output!))

(define (term-restart!)
  (when *term-started?*
    (term-kill!)
    (set! *term-channel* #f)
    (set! *term-started?* #f)
    (set! *term-exited?* #f)
    (set! *term-status* "restarting...")))

;;@doc
;; Configure the terminal. Call it from init.scm before opening; calling it
;; while a terminal is open restarts the shell with the new settings.
;;
;; (terminal-config #:shell "fish")
;; (terminal-config #:shell "bash" #:args '("--noprofile" "--norc"))
;; (terminal-config #:scrollback 8000 #:tick-fast 16 #:tick-slow 120)
;;
;; With no arguments it just reports the current settings in the status line.
(define (terminal-config #:shell [shell #f]
                         #:args [args #f]
                         #:scrollback [scrollback #f]
                         #:tick-fast [fast #f]
                         #:tick-slow [slow #f]
                         #:backend [backend #f]
                         #:helper [helper #f])
  (define changed #f)
  (when shell
    (set! *term-config* (hash-insert *term-config* 'shell shell))
    (set! changed #t))
  (when args
    (set! *term-config* (hash-insert *term-config* 'args args))
    (set! changed #t))
  (when scrollback
    (set! *term-config* (hash-insert *term-config* 'scrollback scrollback))
    (set! *term-max-lines* scrollback)
    (set! *term-trim-to* (max 1 (- scrollback (min 1000 scrollback))))
    (set! *term-lines* (term-take *term-lines* *term-max-lines*))
    (set! *term-line-count* (length *term-lines*)))
  (when fast
    (set! *term-config* (hash-insert *term-config* 'tick-fast fast)))
  (when slow
    (set! *term-config* (hash-insert *term-config* 'tick-slow slow)))
  (when backend
    (set! *term-config* (hash-insert *term-config* 'backend backend))
    (set! changed #t))
  (when helper
    (set! *term-config* (hash-insert *term-config* 'helper helper))
    (set! changed #t))
  (cond
    [changed (term-restart!)]
    [else
     (set-status! (string-append "terminal: shell=" (term-shell)
                                 " backend=" (to-string (term-backend))
                                 " scrollback=" (number->string *term-max-lines*)))]))

(struct TermState ())

(define (term-make-component)
  (new-component! *term-name*
                  (TermState)
                  term-render
                  (hash "handle_event" term-handle-event
                        "cursor" term-cursor)))

;;@doc
;; Open the floating terminal
(define (terminal-open)
  (unless *term-active?*
    (set! *term-active?* #t)
    (set! *term-scroll* 0)
    (set! *term-parse* 'text)
    (when *term-gg?* (term-send-line! "P 0"))
    (unless *term-started?* (set! *term-status* "starting..."))
    (set! *term-input* "")
    (set! *term-caret* 0)
    (push-component! (term-make-component))
    (unless *term-timer?*
      (set! *term-timer?* #t)
      (enqueue-thread-local-callback-with-delay (hash-ref *term-config* 'tick-fast) term-tick))))
