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

-module(lasp_pg).

-export([start/0,
         stop/0]).

-export([members/1,
         join/2,
         join/3,
         leave/2]).

-include("lasp_pg.hrl").

%% @doc Start the application.
start() ->
    application:ensure_all_started(lasp_pg).

%% @doc Stop the application.
stop() ->
    application:stop(lasp_pg).

%% @doc Return members of the process group.
members(Group) ->
    GroupName = term_to_binary(Group),
    lasp:query({GroupName, ?SET}).

%% @doc Remove a member to the process group.
leave(Group, Pid) ->
    GroupName = term_to_binary(Group),
    {ok, {_, _, _, Value}} = lasp:update({GroupName, ?SET}, {rmv, Pid}, actor()),
    {ok, Value}.

%% @doc Add a member to the process group.
join(Group, Pid) ->
    GroupName = term_to_binary(Group),
    {ok, {_, _, _, Value}} = lasp:update({GroupName, ?SET}, {add, Pid}, actor()),
    {ok, Value}.

%% @doc Add a member to the process group and maybe monitor it.
-spec join(Group :: term(), Pid :: pid(), Monitor :: boolean()) -> {ok, term()}.
join(Group, Pid, false) ->
    join(Group, Pid);
join(Group, Pid, true) ->
    {ok, Value} = join(Group, Pid),
    lasp_pg_monitor:monitor_me(Group, Pid),
    {ok, Value}.

%% @private
actor() ->
    term_to_binary(node()).
