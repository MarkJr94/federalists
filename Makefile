CC=ghc
HFLAGS=-O2

all: analysis

analysis: Estimator.hs Preprocess.hs Main.hs
	$(CC) $(HFLAGS) -o $@ $^

clean:
	rm *.o *.hi analysis
