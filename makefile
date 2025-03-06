LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS = --dynamic-space-size 4096 --no-userinit --non-interactive
endif
LISP_FLAGS ?= $(SBCL_FLAGS)
DESTDIR ?= /usr/bin

.PHONY: all install clean

all: lamber

clean:
	rm lamber

lamber:
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' --load lamber.asd --eval '(asdf:load-system :lamber)' --eval '(asdf:make :lamber)' --eval '(quit)'

install: lamber
	cp lamber $(DESTDIR)/
