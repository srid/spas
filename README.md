# spa

`spa` is a single-page application server for drop-in use. It provides an auto-generated REST API for the PostgreSQL data using PostgREST.

## Basic idea

* Create a thin server wrapping [PostgREST](https://github.com/begriffs/postgrest) with these additional features:
* Static file serving
* Authentication without basic auth

And then release it as a Haskell executable for direct use without having to compile it on the user side (or CI, deployment side).

## Background

Mailing discussion: https://groups.google.com/forum/#!topic/elm-discuss/6kyabQgUMJk
