# spas

`spas` is a single-page application server for drop-in use. It provides an auto-generated REST API for the PostgreSQL data using PostgREST.

![](https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/SPAS-12_stock_folded.jpg/450px-SPAS-12_stock_folded.jpg)

## Basic idea

* Create a thin server wrapping [PostgREST](https://github.com/begriffs/postgrest) with these additional features:
* Static file serving
* Authentication without basic auth

And then release it as a Haskell executable for direct use without having to compile it on the user side (or CI, deployment side).

## Getting started

```
$ export SPAS_USERNAME=myname
$ export SPAS_PASSWORD=mypassword
$ export PORT=4000
$ spas
...
```


## Background

Mailing discussion: https://groups.google.com/forum/#!topic/elm-discuss/6kyabQgUMJk
