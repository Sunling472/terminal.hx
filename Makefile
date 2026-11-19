CC      ?= gcc
CFLAGS  ?= -O2 -Wall -Wextra

bin/hx-term: hx-term.c
	@mkdir -p bin
	$(CC) $(CFLAGS) -o $@ $< -lghostty-vt -lutil

clean:
	rm -f bin/hx-term

.PHONY: clean
