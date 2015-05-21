# elm-spa

Single-page applications in Elm, using PostgREST and more.

## Basic idea

* Create a thin server wrapping [PostgREST](https://github.com/begriffs/postgrest) with these additional features:
  * Static file serving
  * Authentication without basic auth
* Elm application scaffold using components 
* Dockerfile

And then release it as a Haskell executable for direct use without having to compile it on the user side (or CI, deployment side).
