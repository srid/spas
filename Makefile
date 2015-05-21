all:
	cabal sandbox init
	cabal install --only-dependencies
	cabal build
