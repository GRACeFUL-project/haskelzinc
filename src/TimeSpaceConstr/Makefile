
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

fourth:
	runhaskell ActionSequences.hs > out4.dot
	dot -Tpng out4.dot -o out4.png

clean:
	$(RM) out.dot out.png out2.dot out2.png out3.dot out3.png out4.dot out4.png

