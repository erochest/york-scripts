
INPUT=data/Congressional_Bills.csv
OUTPUT=data/Congressional_Bills_2.csv

build: $(OUTPUT)

YorkScripts: stack.yaml YorkScripts.hs
	stack ghc -- --make YorkScripts.hs

init: stack.yaml deps

deps:
	stack install cassava text text-format conduit conduit-combinators conduit-extra cassava-conduit bytestring

$(OUTPUT): YorkScripts $(INPUT)
	./YorkScripts < $(INPUT) > $(OUTPUT)

stack.yaml:
	stack new --prefer-nightly

clean:
	rm -f *.o *.hi *.html
	rm -f $(OUTPUT)

distclean: clean
	rm -f YorkScripts
	stack clean

%.html: *.lhs
	pandoc --from markdown+lhs --to html5 --smart --standalone --output=$@ $<

.PHONY: init deps build clean distclean
