# chez-mpv GNUmakefile.
# Written by Jerry 2019-2021.
# SPDX-License-Identifier: Unlicense

# Path to chez scheme executable.
SCHEME = /usr/bin/chez-scheme

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib/csv$(shell $(SCHEME) --version 2>&1)

# (mpv ev) requires chez-libev.
LIBEVDIR = $(LIBDIR)

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

## Should be no need to edit anything below here.

# This makefile assumes a library layout as follows:
# TOP
# PROJDIR/
#   FFI
#   SUBSRC ..
#   SUBOBJ ..
#   SUBWPO ..
#
# Where TOP is the high level library definition that imports all sub libs within PROJDIR.
# FFI (if needed) is a C compilable lib.
# The rest are scheme libraries.
# Scheme compilation is handled by building TOP and letting Chez scheme automatically manage dependants.

PROJDIR = mpv

# Source files, shared objects, and whole program optimisations for the library subdirectory.
SUBSRC = $(PROJDIR)/ev.sls $(PROJDIR)/ftypes-util.sls
SUBOBJ = $(SUBSRC:.sls=.so)
SUBWPO = $(SUBSRC:.sls=.wpo)

# Top level (ie, root) library source, .. etc.
TOPSRC = mpv.sls
TOPOBJ = $(TOPSRC:.sls=.so)
TOPWPO = $(TOPSRC:.sls=.wpo)

# Installed versions of all the above.
ISUBSRC = $(addprefix $(LIBDIR)/,$(SUBSRC))
ISUBOBJ = $(addprefix $(LIBDIR)/,$(SUBOBJ))
ISUBWPO = $(addprefix $(LIBDIR)/,$(SUBWPO))
ITOPSRC = $(addprefix $(LIBDIR)/,$(TOPSRC))
ITOPOBJ = $(addprefix $(LIBDIR)/,$(TOPOBJ))
ITOPWPO = $(addprefix $(LIBDIR)/,$(TOPWPO))

# Tell GNU make about the files generated as a "side-effect" of building TOPWPO,
# otherwise make will raise an error that it doesn't know how to build these.
.SECONDARY: $(SUBWPO) $(SUBOBJ) $(TOPOBJ) $(TOPWPO)

# Default to just a local build.
all: build

# Normally the build target is structured so that the top wpo file is the central build point, but since
# (mpv ev) imports (mpv), we need to use that as the springboard.
# That affects contents of .SECONDARY and dependency order in install-so.
# This might indicate a bad project layout and something to consider in the future..
$(PROJDIR)/ev.wpo: $(TOPSRC) $(SUBSRC)
	echo '(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories (list "." "'$(LIBEVDIR)'")) (compile-library "$(PROJDIR)/ev.sls")' | $(SCHEME) $(SFLAGS)

$(LIBDIR)/%: %
	$(INSTALL) -p -D "$<" "$@"

build: $(PROJDIR)/ev.wpo

# Default install target is for everything.
install: install-so install-src

install-so: $(ISUBWPO) $(ISUBOBJ) $(ITOPWPO) $(ITOPOBJ)

install-src: $(ITOPSRC) $(ISUBSRC)

clean:
	$(RM) $(TOPOBJ) $(TOPWPO) $(SUBOBJ) $(SUBWPO)

clean-install:
	$(RM) $(ITOPOBJ) $(ITOPWPO) $(ISUBOBJ) $(ISUBWPO) $(ITOPSRC) $(ISUBSRC)

clean-all: clean clean-install
