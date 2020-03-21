#! make

MAKEFLAGS += --silent

compile:
	ghc Main.hs -o main.app
.PHONY: compile

clean:
	rm *.{o,hi,app}
.PHONY: clean
