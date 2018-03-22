# CrossDB
[![Build Status](https://travis-ci.org/cabol/cross_db.svg?branch=master)](https://travis-ci.org/cabol/cross_db)
> ## Simple and flexible database wrapper for Erlang
> **Still in development stage!**

**CrossDB** is highly inspired by [Ecto](https://github.com/elixir-ecto/ecto)
and also [SumoDB](https://github.com/inaka/sumo_db).

See the [getting started guide](guides/getting-started.md).

## Testing

Before to run the tests you have run dialyzer first:

```
$ rebar3 dialyzer
```

Then run the tests like so:

```
$ rebar3 ct
```

And if you want to check the coverage:

```
$ rebar3 do ct, cover
```

## Copyright and License

Copyright (c) 2018 Carlos Bolanos

CrossDB source code is licensed under the [MIT License](LICENSE).
