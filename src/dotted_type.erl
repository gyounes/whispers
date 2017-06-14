%%
%% Copyright (c) 2017 Georges Younes.  All Rights Reserved.
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

-module(dotted_type).
-author("Georges Younes <georges.r.younes@gmail.com>").

-include("dotted_type.hrl").

-export_type([dotted_type/0,
              crdt/0,
              polog/0,
              id/0,
              element/0]).

-export([reset/2]).
-export([crdt_size/1]).

%% Define some initial types.
-type dotted_type() :: ?DOTTED_AWSET_TYPE |
                     ?DOTTED_DWFLAG_TYPE |
                     ?DOTTED_EWFLAG_TYPE |
                     ?DOTTED_GCOUNTER_TYPE |
                     ?DOTTED_GSET_TYPE |
                     ?DOTTED_MVREGISTER_TYPE |
                     ?DOTTED_PNCOUNTER_TYPE |
                     ?DOTTED_RWSET_TYPE |
                     ?DOTTED_TWOPSET_TYPE.
-type crdt() :: {dotted_type(), payload()}.
-type payload() :: {polog(), term()}.
-type polog() :: orddict:orddict().
-type id() :: orddict:orddict().
-type element() :: term().

%% Reset the data type
-callback reset(dotted_type:id(), crdt()) -> crdt().

%% @doc Clear/reset the state to initial state.
-spec reset(dotted_type:id(), crdt()) -> crdt().
reset(VV, {Type, {POLog, _Crystal}}) ->
    {Type, {_POLog, Crystal}} = Type:new(),
    POLog1 = orddict:filter(
        fun(Dot, _Op) ->
            not vclock:descends(VV, [Dot])
        end,
        POLog
    ),
    {Type, {POLog1, Crystal}}.

%% @doc Term size.
crdt_size({?DOTTED_AWSET_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?DOTTED_DWFLAG_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?DOTTED_EWFLAG_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?DOTTED_GCOUNTER_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?DOTTED_GSET_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?DOTTED_MVREGISTER_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?DOTTED_RWSET_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?DOTTED_PNCOUNTER_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?DOTTED_TWOPSET_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size(T) ->
    erts_debug:flat_size(T).
