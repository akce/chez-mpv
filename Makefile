# chez-mpv Makefile.
# Copyright (c) 2019 Akce. License: GPLv3, see COPYING for details.

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
DEST = ~/lib

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

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
	$(INSTALL) -D -p -t $(DEST)/mpv $(SUBS)
	$(INSTALL) -D -p -t $(DEST) $(MAIN)

install-obj:
	$(INSTALL) -D -p -t $(DEST)/mpv $(SUBSO)
	$(INSTALL) -D -p -t $(DEST) $(MAINO)

# Only removes the local builds for now.
clean:
	rm -f $(SUBSO) $(MAINO)
