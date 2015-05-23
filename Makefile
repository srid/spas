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
	PORT=${PORT} SPAS_USERNAME=${SPAS_USERNAME} SPAS_PASSWORD=${SPAS_PASSWORD} \
	SPAS_V1SCHEMA=public \
	DATABASE_URL=postgres://srid:password@localhost:5432/srid \
		${EXE} -p ${PORT} -a srid --v1schema public

clean:
	cabal sandbox delete

# Sandbox management
cabal.sandbox.config:	*.cabal
	cabal sandbox init
	cabal install --only-dependencies
