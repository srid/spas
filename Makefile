EXE := ./dist/build/spas/spas

all:	${EXE}
	@true

${EXE}:	cabal.sandbox.config src/*.hs
	cabal build

run:
	PORT=8000 \
		SPA_USERNAME=user \
		SPA_PASSWORD=password \
		DATABASE_URL=postgres://postgres@localhost/test \
		${EXE}

clean:
	cabal sandbox delete

# Sandbox management
cabal.sandbox.config:	*.cabal
	cabal sandbox init
	cabal install --only-dependencies
