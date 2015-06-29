
INPUT=data/Congressional_Bills.csv
OUTPUT=data/Congressional_Bills_2.csv

build: $(OUTPUT)

YorkScripts: stack.yaml YorkScripts.hs
	stack ghc -- --make YorkScripts.hs

roll-calls:
	mkdir roll-calls
	mkdir roll-calls/108
	mkdir roll-calls/109
	mkdir roll-calls/110
	mkdir roll-calls/111
	mkdir roll-calls/112
	mkdir roll-calls/113
	rsync -avz --delete --delete-excluded govtrack.us::govtrackdata/congress/108/votes roll-calls/108
	rsync -avz --delete --delete-excluded govtrack.us::govtrackdata/congress/109/votes roll-calls/109
	rsync -avz --delete --delete-excluded govtrack.us::govtrackdata/congress/110/votes roll-calls/110
	rsync -avz --delete --delete-excluded govtrack.us::govtrackdata/congress/111/votes roll-calls/111
	rsync -avz --delete --delete-excluded govtrack.us::govtrackdata/congress/112/votes roll-calls/112
	rsync -avz --delete --delete-excluded govtrack.us::govtrackdata/congress/113/votes roll-calls/113

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
	rm -rf roll-calls
	stack clean

%.html: *.lhs
	pandoc --from markdown+lhs --to html5 --smart --standalone --output=$@ $<

.PHONY: init deps build clean distclean
