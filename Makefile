SRC=$(shell find src -name *.hs)

build/cogo: $(SRC)
	@mkdir -p build intf
	@ghc -O11 --make -isrc -iintf -hidir intf -odir build -o $@ src/Main.hs

clean:
	rm -rf build/* intf/*
