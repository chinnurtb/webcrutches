webcrutches
===========

A simple web framework with auth, user roles, sessions and templates.

Embeds cowboy web server. Contains mix of helpers and wrappers to define
low level cowboy handlers (with full control over request, response and
headers) or high level web function calls.

## What does?

Webcrutches wraps Cowboy and provides basic request and response handling,
JSON input and output processing, error handling, login and web sessions,
user access control etc. All that you have to reinvent every time when using
Cowboy.

## Can I use in production?

Please don't. Maybe later. Instead have a look at
[Axiom](https://github.com/tsujigiri/axiom) and
[Giallo](https://github.com/kivra/giallo)
