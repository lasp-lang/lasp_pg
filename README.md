Lasp PG
=======================================================

[![Build Status](https://travis-ci.org/lasp-lang/lasp_pg.svg?branch=master)](https://travis-ci.org/lasp-lang/lasp_pg)

Eventually consistent process registry, the spirtual successor to [Riak PG](https://github.com/cmeiklejohn/riak_pg).

## Usage

What are groups?

```erlang
-type group() :: term().
```

Join a process to a group (no need to pre-declare)

```erlang
lasp_pg:join(group(), pid()) -> ok.
```

What about removing?

```erlang
lasp_pg:leave(group(), pid()) -> ok.
```

You can also return the members.

```erlang
lasp_pg:members(group()) -> {ok, sets:set(pid())}.
```

## Clustering

Use the ```lasp_peer_service``` API to join and leave nodes:

```erlang
lasp_peer_service:join({Name, IP, Port}).
```

## Partisan

Our [Partisan](http://github.com/lasp-lang/partisan) library drives what
distribution facility you use: you can use the custom HyParView inspired
backend, which scales to 512+ node Erlang clusters, or fall back to
client/server or full-mesh with distributed Erlang.

This can be configured using the following command, before the Partisan
application is started:

```erlang
partisan_config:set(partisan_peer_service_manager, partisan_hyparview_peer_service_manager).
```

Available options are: ```partisan_client_server_peer_service_manager```, ```partisan_default_peer_service_manager``` or ```partisan_hyparview_peer_service_manager```.

### Configuration

In necessary configure the `partisan` with `pid_encoding` to `false` for working
with the `pid()`.

#### erlang conf:

```erlang
{partisan, [                                                                                                                                                                                                                      
  {pid_encoding, false},
]},
```

#### elixir conf:

```elixir
config :partisan, 
  pid_encoding: false
```

## Replication

Lasp PG uses the underlying Lasp KV store, which only has in-memory
persistence currently, but a configurable API so that can be changed to
use either RocksDB or LevelDB.  Lasp KV is fully replicated across all
nodes, but partial replication is coming soon.
