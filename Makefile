
build: YorkScripts

YorkScripts: cabal.sandbox.config YorkScripts.hs
	cabal exec -- ghc --make YorkScripts.hs

init: cabal.sandbox.config deps

deps:

cabal.sandbox.config:
	cabal sandbox init

clean:
	rm -f *.o *.hi *.html

distclean: clean
	rm -f YorkScripts
	cabal sandbox delete

%.html: *.lhs
	pandoc --from markdown+lhs --to html5 --smart --standalone --output=$@ $<

.PHONY: init deps build clean distclean
