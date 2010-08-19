%%%
%%%    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@medra.com.tr>
%%%
%%%--------------------------------------------------------------------------
%%%    This file is part of bayeserl.
%%%
%%%    MyDLP is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU General Public License as published by
%%%    the Free Software Foundation, either version 3 of the License, or
%%%    (at your option) any later version.
%%%
%%%    MyDLP is distributed in the hope that it will be useful,
%%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%    GNU General Public License for more details.
%%%
%%%    You should have received a copy of the GNU General Public License
%%%    along with MyDLP.  If not, see <http://www.gnu.org/licenses/>.
%%%--------------------------------------------------------------------------

-module(bayeserl).

-author('kerem@medratech.com').

-behaviour(application).

-export([start/2,
	train_positive/1,
        train_negative/1,
        score/1,
        forget_everything/0,
        register_store/1,
	shutdown/0,
	stop/1]).

-include("bayeserl.hrl").


%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
	case bayeserl_sup:start_link(StartArgs) of
		{ok, Pid} -> {ok, Pid};
		Error -> Error end.

%%--------------------------------------------------------------------
%% @doc Called to shudown the bayeserl application.
%% @spec shutdown() -> ok
%% @end
%%--------------------------------------------------------------------
shutdown() ->
	application:stop(bayeserl).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Called upon the termination of an application.
%%--------------------------------------------------------------------
stop(_State) ->
	ok.

train_positive(S) -> bayeserl_server:train_positive(S).

train_negative(S) -> bayeserl_server:train_negative(S).

score(S) -> bayeserl_server:score(S).

forget_everything() -> bayeserl_server:forget_everything().

register_store(Store) -> bayeserl_server:register_store(Store).

