%%
%% Copyright (c) 2015-2016 Christopher Meiklejohn.  All Rights Reserved.
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

-module(pure_type).
-author("Georges Younes <georges.r.younes@gmail.com>").

-include("pure_type.hrl").

-export_type([pure_type/0,
              crdt/0,
              polog/0,
              id/0,
              element/0]).

-export([crdt_size/1]).

%% Define some initial types.
-type pure_type() :: ?PURE_AWSET_TYPE |
                     ?PURE_DWFLAG_TYPE |
                     ?PURE_EWFLAG_TYPE |
                     ?PURE_GCOUNTER_TYPE |
                     ?PURE_GSET_TYPE |
                     ?PURE_MVREGISTER_TYPE |
                     ?PURE_PNCOUNTER_TYPE |
                     ?PURE_RWSET_TYPE |
                     ?PURE_TWOPSET_TYPE.
-type crdt() :: {pure_type(), payload()}.
-type payload() :: {polog(), term()} | term().
-type polog() :: orddict:orddict().
-type id() :: orddict:orddict() | term().
-type element() :: term().


%% check if dt is commutative.
-callback is_commutative() -> boolean().

%% @doc Term size.
crdt_size({?PURE_AWSET_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?PURE_DWFLAG_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?PURE_EWFLAG_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?PURE_GCOUNTER_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?PURE_GSET_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?PURE_MVREGISTER_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?PURE_RWSET_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?PURE_PNCOUNTER_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size({?PURE_TWOPSET_TYPE, CRDT}) -> crdt_size(CRDT);
crdt_size(T) ->
    erts_debug:flat_size(T).
