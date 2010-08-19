%%
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

%%%-------------------------------------------------------------------
%%% @author H. Kerem Cevahir <kerem@medratech.com>
%%% @copyright 2009, H. Kerem Cevahir
%%% @doc Worker for mydlp.
%%% @end
%%%-------------------------------------------------------------------
-module(bayeserl_memory_store).

-author("kerem@medra.com.tr").

-behaviour(gen_server).

-include("bayeserl.hrl").

%% API
-export([start_link/0,
	i_p_c/0,
	i_n_c/0,
	zero/0,
	start/0,
	i_p_wc/2,
	i_n_wc/2,
	g_p_c/0,
	g_n_c/0,
	g_p_wc/1,
	g_n_wc/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).


-record(state, {
		positive_count=0,
		negative_count=0,
		positive_word_map,
		negative_word_map
	}).


i_p_c() -> gen_server:call(?MODULE, i_p_c).

i_n_c() -> gen_server:call(?MODULE, i_n_c).

zero() -> gen_server:call(?MODULE, zero).

i_p_wc(Word, Count) -> gen_server:call(?MODULE, {i_p_wc, Word, Count}).

i_n_wc(Word, Count) -> gen_server:call(?MODULE, {i_n_wc, Word, Count}).

g_p_c() -> gen_server:call(?MODULE, g_p_c).

g_n_c() -> gen_server:call(?MODULE, g_n_c).

g_p_wc(Word) -> gen_server:call(?MODULE, {g_p_wc, Word}).

g_n_wc(Word) -> gen_server:call(?MODULE, {g_n_wc, Word}).


%%%%%%%%%%%%% MyDLP Thrift RPC API

handle_call(i_p_c, _From, #state{positive_count=PC} = State) ->
	{reply, ok, State#state{positive_count=(PC+1)}, 5000};

handle_call(i_n_c, _From, #state{negative_count=NC} = State) ->
	{reply, ok, State#state{negative_count=(NC+1)}, 5000};

handle_call(zero, _From, _State) ->
	{reply, ok, #state{positive_word_map=gb_trees:empty(), 
		negative_word_map=gb_trees:empty()}, 5000};

handle_call(g_p_c, _From, #state{positive_count=PC} = State) ->
	{reply, PC, State, 5000};

handle_call(g_n_c, _From, #state{negative_count=NC} = State) ->
	{reply, NC, State, 5000};

handle_call({i_p_wc, W, C}, _From, #state{positive_word_map=Tree} = State) ->
	NewTree = case gb_trees:is_defined(W, Tree) of
		true ->
			Count = gb_trees:get(W, Tree),
			gb_trees:update(W, Count + C, Tree);
		false ->
			gb_trees:insert(W, C, Tree) end,
	{reply, ok, State#state{positive_word_map=NewTree}, 5000};

handle_call({i_n_wc, W, C}, _From, #state{negative_word_map=Tree} = State) ->
	NewTree = case gb_trees:is_defined(W, Tree) of
		true ->
			Count = gb_trees:get(W, Tree),
			gb_trees:update(W, Count + C, Tree);
		false ->
			gb_trees:insert(W, C, Tree) end,
	{reply, ok, State#state{negative_word_map=NewTree}, 5000};

handle_call({g_p_wc, W}, _From, #state{positive_word_map=Tree} = State) ->
	Reply = case gb_trees:is_defined(W, Tree) of
		true ->	gb_trees:get(W, Tree);
		false -> 0 end,
	{reply, Reply, State, 5000};

handle_call({g_n_wc, W}, _From, #state{negative_word_map=Tree} = State) ->
	Reply = case gb_trees:is_defined(W, Tree) of
		true ->	gb_trees:get(W, Tree);
		false -> 0 end,
	{reply, Reply, State, 5000};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start() -> start_link().

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	{ok, #state{positive_word_map=gb_trees:empty(), 
		negative_word_map=gb_trees:empty()}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

