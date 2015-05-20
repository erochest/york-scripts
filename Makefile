
build: YorkScripts

YorkScripts: cabal.sandbox.config YorkScripts.hs
	cabal exec -- ghc --make YorkScripts.hs

init: cabal.sandbox.config deps

deps:
	cabal install cassava text text-format conduit conduit-combinators conduit-extra cassava-conduit bytestring

data/bill-id.csv: YorkScripts data/chisqr.csv
	./YorkScripts < data/chisqr.csv > data/bill-id.csv

cabal.sandbox.config:
	cabal sandbox init

clean:
	rm -f *.o *.hi *.html
	rm -f data/bill-id.csv

distclean: clean
	rm -f YorkScripts
	cabal sandbox delete

%.html: *.lhs
	pandoc --from markdown+lhs --to html5 --smart --standalone --output=$@ $<

.PHONY: init deps build clean distclean
