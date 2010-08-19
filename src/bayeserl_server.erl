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
-module(bayeserl_server).

-author("kerem@medra.com.tr").

-behaviour(gen_server).

-include("bayeserl.hrl").

%% API
-export([start_link/0,
	train_positive/1,
	train_negative/1,
	score/1,
	forget_everything/0,
	register_store/1,
	stop/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).


-record(state, {
		store,
		regexes
	}).

train_positive(Subject) -> async_call({tp, Subject}).

train_negative(Subject) -> async_call({tn, Subject}).

score(Subject) -> async_call({s, Subject}).

forget_everything() -> async_call(fe).

register_store(Store) -> gen_server:call(?MODULE, {register_store, Store}).

%%%%%%%%%%%%% MyDLP Thrift RPC API

handle_async_call({tp, Subject}, #state{store=Store} = State) ->
	WT = get_word_tree(Subject, State),
	Store:i_p_c(),
	push_word_tree_to_store(WT, Store, positive), ok;

handle_async_call({tn, Subject}, #state{store=Store} = State) ->
	WT = get_word_tree(Subject, State),
	Store:i_n_c(),
	push_word_tree_to_store(WT, Store, negative), ok;

handle_async_call({s, Subject}, #state{store=Store} = State) ->
	WT = get_word_tree(Subject, State),

	RatingList = word_tree_to_rating_list(WT, Store),
	RLLength = length(RatingList),
	FinalList = case RLLength > 20 of
		true -> SL = lists:sort(RatingList),
			L1 = lists:sublist(SL, 10),
			L2 = lists:sublist(SL, RLLength - 9, 10),
			lists:append(L1, L2);
		false -> RatingList end,
	
	P = lists:foldl(fun(X, Prod) -> X * Prod end, 1, FinalList),
	OMP = lists:foldl(fun(X, Prod) -> (1.0 - X) * Prod end, 1, FinalList),

	P / (P + OMP);

handle_async_call(fe, #state{store=Store}) -> Store:zero(), ok;

handle_async_call(_Call, _Store) -> ok.

handle_call({async_call, Call}, From, State) ->
	Worker = self(),
	spawn_link(fun() ->
		Result = handle_async_call(Call, State),
		Worker ! {async_reply, Result, From}
	end),
	{noreply, State, 15000};

handle_call({register_store, NewStore}, _From, #state{store=ExStore} = State) ->
	ExStore:stop(),
	NewStore:start(),
	{reply, ok, State#state{store=NewStore}, 15000};

handle_call(stop, _From, State) ->
	{stop, normalStop, State};

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_info({async_reply, Reply, From}, State) ->
	gen_server:reply(From, Reply),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%% Implicit functions

start_link() ->
	case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	end.

stop() ->
	gen_server:call(?MODULE, stop).

init([]) ->
	Store = bayeserl_memory_store,
	Store:start(),
	RE1=rec("\\d*(?:[.,]\\d+)+|[\\w\\p{L}]+-[\\w\\p{L}]+|[\\w\\p{L}]+"),
	{ok, #state{store=Store, regexes={RE1}}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, #state{store=Store}) ->
	Store:stop(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

async_call(Call) -> gen_server:call(?MODULE, {async_call, Call}).

rec(Regex) -> {ok, Ret} = re:compile(Regex, [unicode]), Ret.

build_word_tree([], Tree) -> Tree;
build_word_tree([Word|R], Tree) ->
	W = erlang:phash2(Word),
	case gb_trees:is_defined(W, Tree) of
		true ->
			Count = gb_trees:get(W, Tree),
			NewTree = gb_trees:update(W, Count + 1, Tree),
			build_word_tree(R, NewTree);
		false ->
			NewTree = gb_trees:insert(W, 1, Tree),
			build_word_tree(R, NewTree)
	end.

get_word_tree(Subject, #state{regexes={RE1}} = State) -> 
	WordList = case re:run(Subject, RE1, [global, {capture, all, list}]) of
		nomatch -> [];
		{match, Captured} -> lists:append(Captured) end,
	WordList1 = lists:filter(fun(W) -> string:len(W) > 2 end, WordList),
	WordList2 = lists:map(fun(W) -> normalize_word(W, State) end, WordList1),

	_WordTree = build_word_tree(WordList2, gb_trees:empty()).

normalize_word(Word, _State) ->
	string:to_lower(Word).

push_word_tree_to_store(Tree, Store, Type) ->
	push_word_tree_to_store1(gb_trees:iterator(Tree), Store, Type).

push_word_tree_to_store1(Iter, Store, Type) ->
	case gb_trees:next(Iter) of
		none -> ok;
		{Word, Count, NewIter} ->
			case Type of
				positive -> Store:i_p_wc(Word, Count);
				negative -> Store:i_n_wc(Word, Count) end,
			push_word_tree_to_store1(NewIter, Store, Type) end.

add_repeating_item_to_list(_Item, 0, List) -> List;
add_repeating_item_to_list(Item, Count, List) ->
	add_repeating_item_to_list(Item, Count - 1, [Item|List]).

rate_word(Store, WordHash) ->
	TPC = Store:g_p_c(),
	TNC = Store:g_n_c(),
	PWC = Store:g_p_wc(WordHash),
	NWC = Store:g_n_wc(WordHash),

	if 	( PWC > 0 ) and ( NWC == 0 ) -> 0.99;
		( PWC == 0 ) and ( NWC > 0 ) -> 0.01;
		%( PWC == 0 ) and ( NWC == 0 ) -> 0.4;
		( PWC == 0 ) and ( NWC == 0 ) -> drop;
		( TPC > 0 ) and ( TNC > 0 ) ->
			PProb = PWC / TPC,
			NProb = NWC / TNC,
			Rating = PProb / (PProb + NProb),
			if	Rating < 0.01 -> 0.01;
				true -> Rating end;
		%true -> 0.4 end.
		true -> drop end.

word_tree_to_rating_list(WT, Store) -> 
	word_tree_to_rating_list(gb_trees:iterator(WT), Store, []).

word_tree_to_rating_list(Iter, Store, Ratings) ->
	case gb_trees:next(Iter) of
		none -> Ratings;
		{WordHash, Count, NewIter} ->
			case rate_word(Store, WordHash) of
				drop -> word_tree_to_rating_list(NewIter, Store, Ratings);
				Rating -> word_tree_to_rating_list(NewIter, Store, 
						add_repeating_item_to_list(Rating, Count, Ratings)) end end.

