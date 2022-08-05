CABAL_OPTIONS += --install-method=copy --installdir=$(srcdir) --overwrite-policy=always

bindir = /bin
infodir = /info
datadir = /share

unexport GHC_PACKAGE_PATH

all: info build

info: hscheme.info
html: hscheme.html

hscheme.info: doc/hscheme.texi
	$(MAKEINFO) $?

hscheme.html: doc/hscheme.texi doc/manual.css
	$(MAKEINFO) $? --html --css-ref=https://www.gnu.org/software/gnulib/manual.css -o hscheme.html

clean: mostlyclean
	rm -rf dist;
	rm -rf dist-newstyle

mostlyclean:
	rm hscheme;
	rm hscheme.info;
	rm -rf hscheme.html;

# You will need to extend this if your cabal build depends on non
# haskell files (here '.lhs' and '.hs' files).
SOURCE = $(shell find src -name '*.lhs' -o -name '*.hs')

# If 'cabal install' fails in building or installing, the
# timestamp on the build dir -- 'build', stored in
# the make target variable '$@' here -- may still be updated.  So,
# we set the timestamp on the build dir to a long time in the past
# with 'touch --date "@0" $@' in case cabal fails.
CABAL_INSTALL = \
	cabal install $(CABAL_OPTIONS) \
	|| { touch --date "@0" $@ ; \
	exit 42 ; }

build: $(SOURCE)
	$(CABAL_INSTALL)

install: installdirs
	cp hscheme $(DESTDIR)$(bindir)
	cp hscheme.info $(DESTDIR)$(infodir)
	cp lib -r $(DESTDIR)$(datadir)

installdirs:
	mkdir -p $(DESTDIR)$(bindir)
	mkdir $(DESTDIR)$(infodir)

run: build
	./hscheme
