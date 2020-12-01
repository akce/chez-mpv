# chez-mpv Makefile.
# Copyright (c) 2019 Akce. License: GPLv3, see COPYING for details.

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib/csv$(shell $(SCHEME) --version 2>&1)

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

## Should be no need to edit anything below here.

# Source.
SUBS = mpv/ftypes-util.sls mpv/ev.sls
MAIN = mpv.sls

# Objects.
SUBSO = mpv/ftypes-util.so mpv/ev.so
MAINO = mpv.so

all: $(MAINO) $(SUBSO)

install: install-src install-obj

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

install-src:
	$(INSTALL) -D -p -t $(LIBDIR)/mpv $(SUBS)
	$(INSTALL) -D -p -t $(LIBDIR) $(MAIN)

install-obj:
	$(INSTALL) -D -p -t $(LIBDIR)/mpv $(SUBSO)
	$(INSTALL) -D -p -t $(LIBDIR) $(MAINO)

# Only removes the local builds for now.
clean:
	rm -f $(SUBSO) $(MAINO)
