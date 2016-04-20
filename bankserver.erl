-module(bankserver).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
    error_logger:info_msg(".........Starting the server named [~p]......~n",[?MODULE]),
 %%   process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    error_logger:info_msg(".........Stoping the server anmed [~p]......~n",[?MODULE]),
    gen_server:call(?MODULE, {stop, normal, stopped}).

init(_Args) ->
    IdsTable = ets:new(ids, [ordered_set, {keypos, 2}]),
    ets:insert(IdsTable, {"XIAOMING HUANG", "330726196507040016"}), 
    ets:insert(IdsTable, {"JUNHUA TAO", "430421197710177894"}), 
    ets:insert(IdsTable, {"ZEGANG LUO", "342225198711260031"}), 
    AccTable = ets:new(accounts, [ordered_set]),
    ets:insert(AccTable, {"330726196507040016", "13102111212", "000000"}), 
    {ok, [IdsTable, AccTable]}.

handle_call({check_id, ID}, _From, [IdsTable, _AccTable]=_State) ->
    error_logger:info_msg("Terminal ~p requests to check ID:~p~n",[_From, ID]),
    1/0,
    Re = ets:lookup(IdsTable, ID),
    case Re of
        [{Name, _}] ->
            {reply, {valid, Name}, _State};
        [] ->
            {reply, invalid, _State}
    end;

handle_call({fill_personal_info, PersonalInfo}, _From, [_IdsTable, AccTable]=_State) ->
    error_logger:info_msg("Terminal ~p requests to fill personalInfo:~p~n",[_From, PersonalInfo]),
    ets:insert(AccTable, PersonalInfo),
    {reply, success, _State};

handle_call({set_pwd, {ID, Pwd}}, _From, [_IdsTable, AccTable]=_State) ->
    error_logger:info_msg("Terminal ~p requests to set the card's password of ~p~n",[_From, ID]),
    OldInfo = ets:lookup(AccTable, ID),
    error_logger:info_msg("OldInfo: ~p~n", [OldInfo]),
    {_, Tel, _} = hd(OldInfo),
    ets:insert(AccTable, {ID, Tel, Pwd}),
    {reply, success, _State}.

handle_cast(_Request, [_IdsTable, _AccTable]=State) ->
    error_logger:info_msg("Received handling cast event [~p]. ignored.~n", [?MODULE]),
    {noreply, State, infinity}.

handle_info(Info, State) ->
    error_logger:info_msg("Received handling info event [~p], ignored.~n", [Info]),
    {noreply, State, infinity}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    error_logger:info_msg("Server is required to terminate as [~p]~n", [Reason]).
