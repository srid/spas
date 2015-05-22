EXE := ./dist/build/spas/spas
PORT ?= 4000

all:	${EXE}
	@true

${EXE}:	cabal.sandbox.config src/*.hs
	cabal build

run:	${EXE}
	PORT=8000 \
		SPA_USERNAME=user \
		SPA_PASSWORD=password \
		DATABASE_URL=postgres://postgres@localhost/test \
		${EXE}

run-legacy:	${EXE}
	${EXE} -p ${PORT} -d srid -U srid --db-pass password \
	 	--db-host localhost -a ${USER} --v1schema public

clean:
	cabal sandbox delete

# Sandbox management
cabal.sandbox.config:	*.cabal
	cabal sandbox init
	cabal install --only-dependencies
