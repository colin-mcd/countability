all: count

count: main.hs cantor.hs
	ghc main.hs cantor.hs -o count

.PHONY: clean
clean:
	rm -f *.hi *.o count
