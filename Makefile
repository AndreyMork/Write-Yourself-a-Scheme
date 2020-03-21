#! make

MAKEFLAGS += --silent

compile:
	ghc -j -outputdir dist -o main.app app/*.hs
.PHONY: compile

clean:
	rm -rf dist
	rm main.app
.PHONY: clean
