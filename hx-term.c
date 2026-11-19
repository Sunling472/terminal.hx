/*
 * hx-term -- terminal backend for the helix + steel terminal plugin.
 *
 * Owns the pty and the VT state (via libghostty-vt), and speaks a tiny
 * line protocol with the plugin:
 *
 *   stdout (to plugin):
 *     F <cols> <rows> <cx> <cy> <cursor_visible> <alt>    frame header
 *     R <row> <text>                                       one row of text
 *     E                                                    end of frame
 *     X <status>                                           child exited
 *
 *   stdin (from plugin), one command per line:
 *     K <byte> [<byte> ...]     bytes to write to the pty (0-255)
 *     S <cols> <rows>           resize
 *     Q                         quit
 */

#define _GNU_SOURCE
#include <ghostty/vt.h>

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pty.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#define MAX_COLS 512
#define MAX_ROWS 256

static GhosttyTerminal term = NULL;
static int pty_fd = -1;
static int child_pid = -1;
static volatile int dirty = 1;

/* ---- pty replies (DA1, OSC 11, kitty keyboard, ...) ------------------- */

static void on_write_pty(GhosttyTerminal t, void *userdata,
                         const uint8_t *data, size_t len) {
  (void)t; (void)userdata;
  if (pty_fd < 0) return;
  size_t off = 0;
  while (off < len) {
    ssize_t n = write(pty_fd, data + off, len - off);
    if (n > 0) {
      off += (size_t)n;
      continue;
    }
    if (n < 0 && errno == EINTR) continue;
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
      struct pollfd pfd = {.fd = pty_fd, .events = POLLOUT};
      if (poll(&pfd, 1, 50) > 0) continue;
    }
    break;
  }
}

/* ---- utf8 ------------------------------------------------------------ */

static size_t encode_utf8(uint32_t cp, char *out) {
  if (cp < 0x80) { out[0] = (char)cp; return 1; }
  if (cp < 0x800) {
    out[0] = (char)(0xC0 | (cp >> 6));
    out[1] = (char)(0x80 | (cp & 0x3F));
    return 2;
  }
  if (cp < 0x10000) {
    out[0] = (char)(0xE0 | (cp >> 12));
    out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[2] = (char)(0x80 | (cp & 0x3F));
    return 3;
  }
  out[0] = (char)(0xF0 | (cp >> 18));
  out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
  out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
  out[3] = (char)(0x80 | (cp & 0x3F));
  return 4;
}

/* ---- frame emission -------------------------------------------------- */

struct outbuf {
  char *data;
  size_t len;
  size_t cap;
};

static void ob_reserve(struct outbuf *ob, size_t extra) {
  if (ob->len + extra <= ob->cap) return;
  size_t cap = ob->cap ? ob->cap : 8192;
  while (cap < ob->len + extra) cap *= 2;
  char *p = realloc(ob->data, cap);
  if (!p) { perror("realloc"); exit(1); }
  ob->data = p;
  ob->cap = cap;
}

static void ob_puts(struct outbuf *ob, const char *s) {
  size_t n = strlen(s);
  ob_reserve(ob, n);
  memcpy(ob->data + ob->len, s, n);
  ob->len += n;
}

static void ob_printf(struct outbuf *ob, const char *fmt, ...) {
  char tmp[512];
  va_list ap;
  va_start(ap, fmt);
  int n = vsnprintf(tmp, sizeof(tmp), fmt, ap);
  va_end(ap);
  if (n < 0) return;
  ob_reserve(ob, (size_t)n);
  memcpy(ob->data + ob->len, tmp, (size_t)n);
  ob->len += (size_t)n;
}

static void emit_frame(void) {
  uint16_t cols = 0, rows = 0, cx = 0, cy = 0;
  int cursor_visible = 1;
  uint32_t alt = 0;

  ghostty_terminal_get(term, GHOSTTY_TERMINAL_DATA_COLS, &cols);
  ghostty_terminal_get(term, GHOSTTY_TERMINAL_DATA_ROWS, &rows);
  ghostty_terminal_get(term, GHOSTTY_TERMINAL_DATA_CURSOR_X, &cx);
  ghostty_terminal_get(term, GHOSTTY_TERMINAL_DATA_CURSOR_Y, &cy);
  ghostty_terminal_get(term, GHOSTTY_TERMINAL_DATA_CURSOR_VISIBLE,
                       &cursor_visible);
  ghostty_terminal_get(term, GHOSTTY_TERMINAL_DATA_ACTIVE_SCREEN, &alt);

  struct outbuf ob = {0};
  ob_printf(&ob, "F %u %u %u %u %d %u\n", cols, rows, cx, cy,
            cursor_visible ? 1 : 0,
            alt == GHOSTTY_TERMINAL_SCREEN_ALTERNATE ? 1 : 0);

  for (uint16_t y = 0; y < rows; y++) {
    char row[MAX_COLS * 4 + 1];
    size_t rlen = 0;
    char utf8[4];
    uint32_t graphemes[16];

    for (uint16_t x = 0; x < cols; x++) {
      GhosttyPoint pt;
      pt.tag = GHOSTTY_POINT_TAG_VIEWPORT;
      pt.value.coordinate.x = x;
      pt.value.coordinate.y = y;

      GhosttyGridRef ref;
      memset(&ref, 0, sizeof(ref));
      ref.size = sizeof(ref);

      size_t gn = 0;
      if (ghostty_terminal_grid_ref(term, pt, &ref) == GHOSTTY_SUCCESS &&
          ghostty_grid_ref_graphemes(&ref, graphemes,
                                     sizeof(graphemes) / sizeof(graphemes[0]),
                                     &gn) == GHOSTTY_SUCCESS &&
          gn > 0) {
        size_t n = encode_utf8(graphemes[0], utf8);
        if (rlen + n < sizeof(row)) {
          memcpy(row + rlen, utf8, n);
          rlen += n;
        }
      } else {
        row[rlen++] = ' ';
      }
    }

    while (rlen > 0 && row[rlen - 1] == ' ') rlen--;
    row[rlen] = '\0';

    ob_printf(&ob, "R %u ", y);
    ob_reserve(&ob, rlen + 2);
    memcpy(ob.data + ob.len, row, rlen);
    ob.len += rlen;
    ob_puts(&ob, "\n");
  }
  ob_puts(&ob, "E\n");

  size_t off = 0;
  while (off < ob.len) {
    ssize_t n = write(STDOUT_FILENO, ob.data + off, ob.len - off);
    if (n <= 0) {
      if (n < 0 && (errno == EAGAIN || errno == EINTR)) continue;
      break;
    }
    off += (size_t)n;
  }
  free(ob.data);
}

/* ---- stdin command handling ------------------------------------------ */

static void set_winsize(uint16_t cols, uint16_t rows) {
  struct winsize ws;
  memset(&ws, 0, sizeof(ws));
  ws.ws_col = cols;
  ws.ws_row = rows;
  if (pty_fd >= 0) ioctl(pty_fd, TIOCSWINSZ, &ws);
}

static void handle_line(char *line) {
  if (line[0] == 'K') {
    /* "K <byte> <byte> ..." */
    char *p = line + 1;
    while (*p) {
      while (*p == ' ') p++;
      if (!*p) break;
      char *end = NULL;
      long v = strtol(p, &end, 10);
      if (end == p) break;
      p = end;
      if (v < 0 || v > 255) continue;
      uint8_t b = (uint8_t)v;
      if (pty_fd >= 0) {
        ssize_t n = write(pty_fd, &b, 1);
        if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
          struct pollfd pfd = {.fd = pty_fd, .events = POLLOUT};
          if (poll(&pfd, 1, 50) > 0) {
            n = write(pty_fd, &b, 1);
            (void)n;
          }
        }
      }
    }
  } else if (line[0] == 'S') {
    unsigned c = 0, r = 0;
    if (sscanf(line + 1, "%u %u", &c, &r) == 2 && c > 0 && r > 0) {
      if (c > MAX_COLS) c = MAX_COLS;
      if (r > MAX_ROWS) r = MAX_ROWS;
      ghostty_terminal_resize(term, (uint16_t)c, (uint16_t)r, 0, 0);
      set_winsize((uint16_t)c, (uint16_t)r);
      dirty = 1;
    }
  } else if (line[0] == 'Q') {
    if (child_pid > 0) kill(child_pid, SIGHUP);
    exit(0);
  }
}

/* ---- main ------------------------------------------------------------ */

static void usage(const char *argv0) {
  fprintf(stderr,
          "usage: %s [--cols N] [--rows N] -- <command> [args...]\n",
          argv0);
  exit(2);
}

int main(int argc, char **argv) {
  int cols = 80, rows = 24;
  int i = 1;
  for (; i < argc; i++) {
    if (!strcmp(argv[i], "--cols") && i + 1 < argc) {
      cols = atoi(argv[++i]);
    } else if (!strcmp(argv[i], "--rows") && i + 1 < argc) {
      rows = atoi(argv[++i]);
    } else if (!strcmp(argv[i], "--")) {
      i++;
      break;
    } else {
      break;
    }
  }
  if (i >= argc) usage(argv[0]);
  if (cols < 2) cols = 2;
  if (rows < 2) rows = 2;
  if (cols > MAX_COLS) cols = MAX_COLS;
  if (rows > MAX_ROWS) rows = MAX_ROWS;

  if (ghostty_terminal_new(NULL, &term, (uint16_t)cols, (uint16_t)rows) !=
      GHOSTTY_SUCCESS) {
    fprintf(stderr, "hx-term: failed to create terminal\n");
    return 1;
  }
  ghostty_terminal_set(term, GHOSTTY_TERMINAL_OPT_WRITE_PTY,
                       (const void *)on_write_pty);

  struct winsize ws;
  memset(&ws, 0, sizeof(ws));
  ws.ws_col = (unsigned short)cols;
  ws.ws_row = (unsigned short)rows;

  child_pid = forkpty(&pty_fd, NULL, NULL, &ws);
  if (child_pid < 0) {
    perror("forkpty");
    return 1;
  }
  if (child_pid == 0) {
    setenv("TERM", "xterm-256color", 1);
    setenv("COLORTERM", "truecolor", 1);
    char cols_buf[16], rows_buf[16];
    snprintf(cols_buf, sizeof(cols_buf), "%d", cols);
    snprintf(rows_buf, sizeof(rows_buf), "%d", rows);
    setenv("COLUMNS", cols_buf, 1);
    setenv("LINES", rows_buf, 1);
    execvp(argv[i], &argv[i]);
    perror("execvp");
    _exit(127);
  }

  signal(SIGPIPE, SIG_IGN);
  fcntl(STDIN_FILENO, F_SETFL, fcntl(STDIN_FILENO, F_GETFL, 0) | O_NONBLOCK);
  /* the master side must not block: we drain it in a loop after poll */
  fcntl(pty_fd, F_SETFL, fcntl(pty_fd, F_GETFL, 0) | O_NONBLOCK);

  int stdin_open = 1;
  char inbuf[8192];
  size_t inlen = 0;

  for (;;) {
    struct pollfd fds[2];
    int nfds = 0;
    fds[nfds].fd = pty_fd;
    fds[nfds].events = POLLIN;
    nfds++;
    if (stdin_open) {
      fds[nfds].fd = STDIN_FILENO;
      fds[nfds].events = POLLIN;
      nfds++;
    }

    int ready = poll(fds, (nfds_t)nfds, 16);
    if (ready < 0) {
      if (errno == EINTR) continue;
      perror("poll");
      break;
    }

    /* pty -> terminal */
    if (fds[0].revents & (POLLIN | POLLHUP | POLLERR)) {
      uint8_t buf[16384];
      for (;;) {
        ssize_t n = read(pty_fd, buf, sizeof(buf));
        if (n > 0) {
          ghostty_terminal_vt_write(term, buf, (size_t)n);
          dirty = 1;
          continue;
        }
        if (n < 0 && (errno == EAGAIN || errno == EINTR)) break;
        break;
      }
    }

    /* plugin -> pty */
    if (stdin_open && (fds[1].revents & (POLLIN | POLLHUP))) {
      ssize_t n = read(STDIN_FILENO, inbuf + inlen, sizeof(inbuf) - inlen);
      if (n > 0) {
        inlen += (size_t)n;
        size_t start = 0;
        for (size_t k = 0; k < inlen; k++) {
          if (inbuf[k] == '\n') {
            inbuf[k] = '\0';
            handle_line(inbuf + start);
            start = k + 1;
          }
        }
        if (start > 0) {
          memmove(inbuf, inbuf + start, inlen - start);
          inlen -= start;
        }
        if (inlen == sizeof(inbuf)) inlen = 0; /* drop oversized */
      } else if (n == 0) {
        stdin_open = 0;
      } else if (errno != EAGAIN && errno != EINTR) {
        stdin_open = 0;
      }
    }

    /* child exited? */
    int status = 0;
    pid_t wp = waitpid(child_pid, &status, WNOHANG);
    if (wp == child_pid) {
      /* flush whatever the child left behind */
      uint8_t buf[16384];
      for (;;) {
        ssize_t n = read(pty_fd, buf, sizeof(buf));
        if (n <= 0) break;
        ghostty_terminal_vt_write(term, buf, (size_t)n);
        dirty = 1;
      }
      emit_frame();
      char tail[64];
      int code = WIFEXITED(status) ? WEXITSTATUS(status)
                                   : (WIFSIGNALED(status) ? -WTERMSIG(status) : -1);
      int n = snprintf(tail, sizeof(tail), "X %d\n", code);
      ssize_t w = write(STDOUT_FILENO, tail, (size_t)n);
      (void)w;
      break;
    }

    if (dirty) {
      emit_frame();
      dirty = 0;
    }
  }

  ghostty_terminal_free(term);
  return 0;
}
