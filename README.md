# spas

`spas` is a server sitting in between your SPA (single-page application) and PostgreSQL database. It provides an auto-generated REST API for the PostgreSQL data using PostgREST.

![](https://upload.wikimedia.org/wikipedia/commons/thumb/0/09/SPAS-12_stock_folded.jpg/450px-SPAS-12_stock_folded.jpg)

## Basic idea

* Create a thin server wrapping [PostgREST](https://github.com/begriffs/postgrest) with these additional features:
* Static file serving
* Authentication (using basic auth for now)

## Getting started

```
$ export SPAS_USERNAME=myname
$ export SPAS_PASSWORD=mypassword
$ export DATABASE_URL=postgres://...  # also provided by Heroku
$ export PORT=4000
$ cd /path/to/static/files && spas
...
```

See https://github.com/srid/chronicle for an Elm application that uses spas.

## Background

Mailing discussion: https://groups.google.com/forum/#!topic/elm-discuss/6kyabQgUMJk
