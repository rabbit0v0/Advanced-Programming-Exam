-module(mailfilter).
-behaviour(gen_server).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called mailfilter.

% Export at least the API:

-export(
  [ start/1
  , stop/1
  , default/4
  , add_mail/2
  , get_config/1
  , enough/1
  , add_filter/4
  ]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% You may have other exports as well
-export([test1/2, test2/2, test3/2]).

-type mail() :: any().
-type data() :: any().
-type label() :: any().
-type result() :: {done, data()} | inprogress .
-type labelled_result() :: {label(), result()}.
-type filter_result() :: {just, data()}
                       | {transformed, mail()}
                       | unchanged
                       | {both, mail(), data()}.
-type filter_fun() :: fun( (mail(), data()) -> filter_result() ).

-type filter() :: {simple, filter_fun()}
                | {chain, list(filter())}
                | {group, list(filter()), merge_fun() }
                | {timelimit, timeout(), filter()}.
-type merge_fun() :: fun( (list(filter_result() | inprogress)) -> filter_result() | continue ).

% API :

start(_Cap) ->
  gen_server:start({local, mailfilter}, ?MODULE, [], []).

stop(MS) ->
  Ret = gen_server:call(MS, get_state, infinity),
  gen_server:cast(MS, stop_all_filters),
  gen_server:stop(MS),
  Ret.

% stop_filter(PID) ->
%   exit(PID, kill).

add_mail(MS, Mail) ->
  Res = gen_server:call(MS, {add_mail, Mail}, infinity),
  {_, MR} = Res,
  {_, {_, L}} = get_config(MR),
  cast_filter_by_list(L, MR),
  Res.

get_config(MR) ->
  gen_server:call(?MODULE, {get_config, MR}, infinity).

default(MS, Label, Filt, Data) ->
  Flag = gen_server:call(?MODULE, {already_in, Label}, infinity),
  if Flag ->
    ok;
  true ->
    gen_server:cast(MS, {default, Label, Filt, Data})
  end.

enough(MR) ->
  gen_server:cast(?MODULE, {enough, MR}).

add_filter(MR, Label, Filt, Data) ->
  Flag = gen_server:call(?MODULE, {already_in, Label}, infinity),
  MSt = gen_server:call(?MODULE, {mail_in, MR}, infinity),
  if Flag ->
    {info, "Label has been registered."};
  true ->
    if MSt ->
      gen_server:cast(?MODULE, {add_filter, MR, Label, Filt, Data}),
      gen_server:cast(?MODULE, {cast_filters, Label, MR});
    true ->
      {info, "no such MR."}
    end
  end.

cast_filter_by_list([], _MR) -> ok;

cast_filter_by_list([{Label, _}|T], MR) ->
  gen_server:cast(?MODULE, {cast_filters, Label, MR}),
  cast_filter_by_list(T, MR).

get_filters([]) -> [];

get_filters([Key|T]) ->
    [{Key, inprogress}] ++ get_filters(T).

get_mailstate([], _) -> [];

get_mailstate([Key|T], M) ->
    Item = maps:get(Key, M),
    [Item] ++ get_mailstate(T, M).

get_keys([], _Filters) -> [];

get_keys([Key|T], Filters) ->
    {_, _, Default} = maps:get(Key, Filters),
    case Default of
      1 ->
        Ret = [Key];
      0 -> 
        Ret = []
    end,
    Ret ++ get_keys(T, Filters).


% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([]) -> {ok, {0, maps:new(), maps:new()}}.
% mail count(as MR), filters by label, mail state by MR

% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_call(get_state, _From, State) ->
    {_, _, Mails} = State,
    Keys = maps:keys(Mails),
    MailState = get_mailstate(Keys, Mails),
    Reply = {ok, MailState},
    {reply, Reply, State};

handle_call(get_all_state, _From, State) ->
    Reply = {ok, State},
    {reply, Reply, State};

handle_call({already_in, Label}, _From, State) ->
    {_, Filters, _} = State,
    Tmp = maps:is_key(Label, Filters),
    Reply = Tmp,
    {reply, Reply, State};

handle_call({mail_in, MR}, _From, State) ->
    {_, _, Mails} = State,
    Tmp = maps:is_key(MR, Mails),
    if Tmp ->
      {reply, true, State};
    true -> {reply, false, State}
    end;

handle_call({add_mail, Mail}, _From, State) ->
    {Count, Filters, Mails} = State,
    MR = Count,
    All_Keys = maps:keys(Filters),
    Keys = get_keys(All_Keys, Filters),
    Labelled_Res = get_filters(Keys),
    State1 = {Count + 1, Filters, Mails#{MR => {Mail, Labelled_Res}}},
    Reply = {ok, MR},
    {reply, Reply, State1};

handle_call({get_config, MR}, _From, State) ->
    {_, _, Mails} = State,
    Tmp = maps:is_key(MR, Mails),
    if Tmp -> 
      Reply = {ok, maps:get(MR, Mails)}; 
      true -> Reply = {error, "MR does not represent any mail in our system."}
    end,
    {reply, Reply, State}.

% handle_call({run_it, Filt, Mail, Data}, _From, State0) ->
%     case Filt of 
%       {simple, Fun} ->
%         Res = Fun(Mail, Data);
%       {chain, Filt_List} ->
%         Res = run_chain(Filt_List, Mail, Data, unchanged);
%       {timeout, Time, Filt1} ->
%         {ok, PID} = gen_server:start({global, {Filt, Mail, Data}}, ?MODULE, [], []),
%         Res = gen_server:call(PID, {run_it, Filt, Mail, Data}, Time),
%         stop_filter(PID),
%         global:unregister_name({Filt, Mail, Data})
%     end,
%     {reply, Res, State0}.


% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast({default, Label, Filt, Data}, State) ->
    {Count, Filters, Mails} = State,
    Tmp = maps:is_key(Label, Filters),
    if Tmp -> Filters1 = Filters;
    true -> Filters1 = Filters#{Label => {Filt, Data, 1}}
    end,
    State1 = {Count, Filters1, Mails},
    {noreply, State1};

handle_cast({enough, MR}, State) -> %stop the filters here
    {Count, Filters, Mails} = State,
    Tmp = maps:is_key(MR, Mails),
    if Tmp ->
      {_, List} =  maps:get(MR, Mails),
      stop_filters_by_list(List, MR),
      Tmp = maps:is_key(MR, Mails),
      if Tmp ->
        Mails1 = maps:remove(MR, Mails);
      true -> 
        Mails1 = Mails
      end,
      State1 = {Count, Filters, Mails1},
      {noreply, State1};
    true -> {noreply, State}
  end;

handle_cast({add_filter, MR, Label, Filt, Data}, State) ->
    {Count, Filters, Mails} = State,
    Tmp = maps:is_key(Label, Filters),
    if Tmp ->
      State1 = State;
    true ->
      Filters1 = Filters#{Label => {Filt, Data, 0}},
      Tmp1 = maps:is_key(MR, Mails),
      if Tmp1 ->
        {Mail, Labelled_Res} = maps:get(MR, Mails),
        Config = {Mail, Labelled_Res++[{Label, inprogress}]},
        Mails1 = Mails#{MR := Config},
        State1 = {Count, Filters1, Mails1};
      true ->
        State1 = State
      end
    end,
    {noreply, State1};

handle_cast(stop_all_filters, State) ->
    {_, _, Mails} = State,
    MRs = maps:keys(Mails),
    stop_all_filter(MRs, Mails),
    {noreply, State};

handle_cast({cast_filters, Label, MR}, State) -> % change PIDs
  {Count, Filters, Mails} = State,
  case global:whereis_name({MR, Label}) of
    undefined -> ok;
    Pid -> 
      exit(Pid, kill)
      % gen_server:stop(Pid)
  end,
  {ok, PID} = gen_server:start({global, {MR, Label}}, ?MODULE, [], []),
  gen_server:cast(PID, {run_all, MR, Label, State}),

  State1 = {Count, Filters, Mails},
  {noreply, State1};

handle_cast({update, MR, Label, Mail, Res}, State) ->
    {Count, Filters, Mails} = State,
    {_, Labelled_Res} = maps:get(MR, Mails),
    case Res of
      {just, New_Data} ->
        New_Labbeled_Res = change_result(Label, New_Data, Labelled_Res),
        New_Config = {Mail, New_Labbeled_Res};
      {transformed, New_Mail} ->
        Labbeled_Res1 = change_result(Label, nothing, Labelled_Res),
        New_Labbeled_Res = set_undo(Label, Labbeled_Res1),
        New_Config = {New_Mail, New_Labbeled_Res},
        % restart all other filters
        L_R = lists:delete({Label, {done, nothing}}, New_Labbeled_Res),
        cast_filter_by_list(L_R, MR);
      unchanged ->
        New_Labbeled_Res = change_result(Label, nothing, Labelled_Res),
        New_Config = {Mail, New_Labbeled_Res};
      {both, New_Mail, New_Data} ->
        Labbeled_Res1 = change_result(Label, New_Data, Labelled_Res),
        New_Labbeled_Res = set_undo(Label, Labbeled_Res1),
        New_Config = {New_Mail, New_Labbeled_Res},
        % restart all other filters
        L_R = lists:delete({Label, {done, New_Data}}, New_Labbeled_Res),
        cast_filter_by_list(L_R, MR)
    end,
    New_Mails = Mails#{MR := New_Config},
    New_State = {Count, Filters, New_Mails},
    {noreply, New_State};

handle_cast({run_all, MR, Label, State}, State0) ->
    {_, Filters, Mails} = State,
    {Filt, Data, _} = maps:get(Label, Filters),
    {Mail, _} = maps:get(MR, Mails),
    % {ok, PID} = gen_server:start({global, {MR, Label, 0}}, ?MODULE, [], []),
    % Res = gen_server:call(PID, {run_it, Filt, Mail, Data}, infinity),
    % stop_filter(PID),
    % global:unregister_name({MR, Label, 0}),
    case Filt of 
      {simple, Fun} ->
        Res = Fun(Mail, Data);
      {chain, Filt_List} ->
        Res = run_chain(Filt_List, Mail, Data, unchanged)
    end,
    gen_server:cast(?MODULE, {update, MR, Label, Mail, Res}),

    {stop, normal, State0}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

change_result(Label, Data, [{Label, _}|T]) ->
    [{Label, {done, Data}}] ++ T;

change_result(Label, Data, [H|T]) ->
    [H] ++ change_result(Label, Data, T).

set_undo(_Label, []) -> [];

set_undo(Label, [{Label1, Res}|T]) ->
    if
      Label =:= Label1 ->
        Res1 = [{Label1, Res}] ++ set_undo(Label, T);
      true ->
        Res1 = [{Label1, inprogress}] ++ set_undo(Label, T)
    end,
    Res1.


% all can be changed to call

stop_filters_by_list([], _MR) -> ok;

stop_filters_by_list([{Label, _}|T], MR) ->
    case global:whereis_name({MR, Label}) of
      undefined -> ok;
      Pid -> 
        exit(Pid, kill),
        global:unregister_name({MR, Label})
        % gen_server:stop(Pid)
    end,
    stop_filters_by_list(T, MR).

stop_all_filter([], _Mails) ->
    ok;

stop_all_filter([MR|T], Mails) ->
    {_, List} = maps:get(MR, Mails),
    stop_filters_by_list(List, MR),
    stop_all_filter(T, Mails).

run_chain([], _Mail, _Data, R0) -> R0;    

run_chain([Filter|T], Mail, Data, R0) ->
    case Filter of
      {simple, Fun} ->
        R = Fun(Mail, Data);
      {chain, Filt_List} ->
        R = run_chain(Filt_List, Mail, Data, R0)
    end,
    case R of
      {just, New_Data} ->
        M1 = Mail,
        D1 = New_Data;
      {transformed, New_Mail} ->
        M1 = New_Mail,
        D1 = Data;
      unchanged ->
        M1 = Mail,
        D1 = Data;
      {both, New_Mail, New_Data} ->
        M1 = New_Mail,
        D1 = New_Data
    end,
    run_chain(T, M1, D1, R).

test1(_M, _D) -> {transformed, "bb"}.
test2(_M, _D) -> {just, 0}.
test3(M, D) -> test3(M, D).