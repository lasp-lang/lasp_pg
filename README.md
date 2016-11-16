Lasp PG
=======================================================

[![Build Status](https://travis-ci.org/lasp-lang/lasp_pg.svg?branch=master)](https://travis-ci.org/lasp-lang/lasp_pg)

Eventually consistent process registry, the spirtual successor to [Riak PG](https://github.com/cmeiklejohn/riak_pg).

## Usage

Join a process to a group (no need to pre-declare)

```erlang
join(term(), pid()) -> ok.
```

What about removing?

```erlang
leave(term(), pid()) -> ok.
```

You can also return the members.

```erlang
members(term()) -> {ok, sets:set(pid())}.
```
