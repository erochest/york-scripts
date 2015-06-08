
INPUT=data/Congressional_Bills.csv
OUTPUT=data/Congressional_Bills_2.csv

build: $(OUTPUT)

YorkScripts: cabal.sandbox.config YorkScripts.hs
	cabal exec -- ghc --make YorkScripts.hs

init: cabal.sandbox.config deps

deps:
	cabal install cassava text text-format conduit conduit-combinators conduit-extra cassava-conduit bytestring

$(OUTPUT): YorkScripts $(INPUT)
	./YorkScripts < $(INPUT) > $(OUTPUT)

cabal.sandbox.config:
	cabal sandbox init
	make deps

clean:
	rm -f *.o *.hi *.html
	rm -f $(OUTPUT)

distclean: clean
	rm -f YorkScripts
	cabal sandbox delete

%.html: *.lhs
	pandoc --from markdown+lhs --to html5 --smart --standalone --output=$@ $<

.PHONY: init deps build clean distclean
