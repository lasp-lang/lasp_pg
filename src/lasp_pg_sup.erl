%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Christopher Meiklejohn.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(lasp_pg_sup).

-behaviour(supervisor).

-include("lasp_pg.hrl").

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type, Timeout),
        {I, {I, start_link, []}, permanent, Timeout, Type, [I]}).
-define(CHILD(I, Type), ?CHILD(I, Type, 5000)).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Manually configure the peer service to use full mesh.
    partisan_config:set(partisan_peer_service_manager,
                        partisan_default_peer_service_manager),

    Partisan = {partisan_sup,
                {partisan_sup, start_link, []},
                 permanent, infinity, supervisor, [partisan_sup]},

    Lasp = {lasp_sup,
                {lasp_sup, start_link, []},
                 permanent, infinity, supervisor, [lasp_sup]},

    Children = [?CHILD(lasp_pg_monitor, worker), Partisan, Lasp],

    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.
