EXE := ./dist/build/spas/spas
PORT ?= 4000

# Dev configuration for the spas app
SPAS_USERNAME := user
SPAS_PASSWORD := password
# END

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
	PORT=${PORT} SPAS_USERNAME=${SPAS_USERNAME} SPAS_PASSWORD=${SPAS_PASSWORD} \
	DATABASE_URL=postgres://srid:password@localhost:5432/srid \
		${EXE} -p ${PORT} -a srid --v1schema public

clean:
	cabal sandbox delete

# Sandbox management
cabal.sandbox.config:	*.cabal
	cabal sandbox init
	cabal install --only-dependencies
