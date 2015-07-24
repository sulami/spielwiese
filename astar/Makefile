all:
	# let's hope we have cabal
	cabal sandbox init
	cabal install parallel
	cabal build
	mv dist/build/run/run ./runhs

.PHONY: all
