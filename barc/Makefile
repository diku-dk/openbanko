.PHONY: install clean

dist/build/barc/barc: barc.cabal barc.hs Barc/*.hs
	cabal configure
	cabal build

install: dist/build/barc/barc
	cabal install

clean:
	cabal clean
