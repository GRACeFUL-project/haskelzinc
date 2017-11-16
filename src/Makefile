
export SHELL := /bin/bash

all:
	runhaskell ActionSequences.hs > out.dot
	dot -Tpng out.dot -o out.png

second:
	runhaskell ActionSequences.hs > out2.dot
	dot -Tpng out2.dot -o out2.png

third:
	runhaskell ActionSequences.hs > out3.dot
	dot -Tpng out3.dot -o out3.png

clean:
	$(RM) out.dot out.png

