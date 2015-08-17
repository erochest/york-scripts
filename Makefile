
SRC=$(shell find src -name '*.hs')
BILLS_INPUT=data/Congressional_Bills.csv
BILLS_OUTPUT=data/Congressional_Bills_2.csv
ROLL_OUTPUT=data/roll-calls.csv

QDIR=tmp-data
QSESSION=108
QBILL=1

all: init data test output

data: roll-calls $(BILLS_INPUT)

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

init: stack.yaml

build:
	stack build --pedantic

test:
	stack test

quarantine: build
	stack install
	-rm -rf tmp-data
	for dn in `find roll-calls/$(QSESSION) -name [hs]$(QBILL)`; \
		do \
		echo $$dn; \
		dest=$(QDIR)/$$dn; \
		mkdir -p $$dest; \
		cp -r $$dn $$dest; \
		done
	stack exec -- roll-calls --verbose $(QDIR)/roll-calls $(QDIR)/q.csv
	tree $(QDIR)

$(BILLS_OUTPUT): build $(BILLS_INPUT)
	stack exec -- york-scripts bill-id < $(BILLS_INPUT) > $(BILLS_OUTPUT)

$(ROLL_OUTPUT): build roll-calls
	stack exec -- roll-calls roll-calls/ $(ROLL_OUTPUT)

data/hr743.csv: build roll-calls
	stack exec -- roll-calls --verbose hr743/ data/hr743.csv

output: $(BILLS_OUTPUT) $(ROLL_OUTPUT)

stack.yaml:
	stack init --prefer-nightly

tags: ${SRC}
	codex update

hlint:
	hlint *.hs src specs

watch:
	ghcid --command="stack ghci"

clean:
	rm -f *.o *.hi *.html
	rm -f $(BILLS_OUTPUT)
	codex cache clean

distclean: clean
	rm -rf roll-calls
	stack clean

.PHONY: init build clean distclean hlint output
