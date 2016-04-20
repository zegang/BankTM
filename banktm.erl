-module(banktm).
-behaviour(gen_fsm).

-export([start/0, stop/0, apply_new_account/0, check_id/0, fill_personal_info/2,set_pwd/2,pick_card/0]).

-export([init/1, terminate/3, handle_event/3, code_change/4, handle_info/3,handle_sync_event/4]).
-export([idle/2, checking_id/3, filling_personal_info/3, setting_pwd/3, picking_card/3]).
-export([checking_id/2, filling_personal_info/2, setting_pwd/2, picking_card/2]).

-define(MAX_WAITINGTIME, 60000).
-define(SERVER_NAME, {bankserver, server@ericsson}).

%% Interfaces opened to customers
start() ->
    start_link().

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    io:format("~20cTry to stop BankTM~20c~nNice to meet you again~n",[$.,$.]),
    gen_fsm:send_all_state_event(?MODULE, stop).

apply_new_account() ->
    gen_fsm:send_event(?MODULE, apply_new_account),
    check_id().

check_id() ->
    ID = string:strip(io:get_line("Please input your ID:"), right, $\n),
    case re:run(ID, "^\d{15}$|^\d{18}|^\d{17}(\d|X|x") of
         nomatch ->
             io:format("Invalid ID.~n"),
             check_id();
         {match, _} ->
             io:format("~n~20cPlease wait~20c~n It is checking your ID: ~p~n", [$., $., ID]),
             Reply = gen_fsm:sync_send_event(?MODULE, {check_id, ID}),
             case Reply of
                 {valid, Name} ->
                     io:format("Conguduations ~p. Your ID card [~p] is available~n", [Name,ID]),
                     fill_personal_info(ID,0);
                 invalid ->
                     io:format("Sorry, Your ID card is invalid. Please retry it again ~n",[]),
                     check_id();
                 _Any ->
                     io:format("Sorry. Your ID card is invalid. ~n~p", [_Any])
             end
    end.
   

is_valid_phonenumber(Tel) ->
     Tel_Regex = "^1[0-9]{10}$",
     re:run(Tel, Tel_Regex).

fill_personal_info(ID, Times) when Times < 3 ->
    Tel = string:strip(io:get_line("Please input your telephone number:"), right, $\n), case is_valid_phonenumber(Tel) of
         nomatch -> io:format("Invalid telephone number.~n"),
             fill_personal_info(ID, Times +1);
         {match, _} ->
             io:format("~n~20cPlease wait~20c~nIt is updating your personal information as:~n\ttelephone: ~p~n", [$.,$.,Tel]),
             Reply = gen_fsm:sync_send_event(?MODULE, {fill_personal_info, {ID, Tel}}),
             case Reply of
                 success ->
                     set_pwd(ID, 0);
                 _Any ->
                     io:format("Sorry.~n~p", [_Any])
             end
    end;
fill_personal_info(_ID, _Times) ->
     retry_too_much().

set_pwd(ID, Times) when Times < 3 ->
    Pwd = string:strip(io:get_line("Please input your password for bank card:"), right, $\n),
    Pwd_Regex = "[0-9]{6}",
    case re:run(Pwd, Pwd_Regex) of
        nomatch ->
            io:format("Invalid password.~n"),
            set_pwd(ID, Times + 1);
        {match, _} ->
             io:format("~n~20cPlease wait~20c~nIt is setting your password ~n", [$., $.]),
             Reply =  gen_fsm:sync_send_event(?MODULE, {set_pwd, {ID, Pwd}}),
             case Reply of
                 success ->
                     io:format("Set password successfully\n", []),
                     pick_card();
                 _Any ->
                     io:format("Sorry. Error. ~n~p", [_Any])
            end
    end;
set_pwd(_ID, _Times) ->
    retry_too_much().

pick_card() ->
    io:format("Please pick up your card~n"),
    try gen_fsm:sync_send_event(?MODULE, pick_card)
    catch
        exit:_Any ->
            error_logger:error_msg("Pick card Error: ~p~n", [_Any])
    end.

retry_too_much() ->
    io:format("Sorry, you have tried too much times.~n"),
    gen_fsm:send_all_state_event(?MODULE, {reinit, []}).

%% Internal status hanlders
init(_Args) ->
    io:format("------------------Welcom to use BankTM-------------------~n"),
    {ok, idle, []}.

idle(Event, _StateData) ->
    _CurrentState = idle,
    case Event of
        apply_new_account ->
            {next_state, checking_id, [], ?MAX_WAITINGTIME};
        _Any ->
            error_logger:error_msg("State [~p]: invalid action.~n~p~n", [_CurrentState, _Any]),
            reinit_fsm()
    end.

checking_id(Event, _From, _StateData) ->
    _CurrentState = checking_id,
    case Event of
        {check_id, _} ->
            Reply = gen_server:call(?SERVER_NAME, Event),
            case Reply of
                {valid, _} ->
                     {reply, Reply, filling_personal_info, [], ?MAX_WAITINGTIME};
                 invalid ->
                     {reply, Reply, checking_id, [], ?MAX_WAITINGTIME}
            end;
        _Any ->
            error_logger:error_msg("State [~p]: invalid action.~n~p~n", [_CurrentState, _Any]),
            reinit_fsm() 
    end.

checking_id(timeout, _StateData) ->
    log_timeout("checking_id").

filling_personal_info(Event, _From, _StateData) ->
    _CurrentState = filling_personal_info,
    case Event of
        {fill_personal_info, {ID, Tel}} ->
            Reply = gen_server:call(?SERVER_NAME, {fill_personal_info, {ID, Tel, "000000"}}),
            {reply, Reply, setting_pwd, [], ?MAX_WAITINGTIME};
        _Any ->
            error_logger:error_msg("State [~p]: invalid action.~n~p~n", [_CurrentState, _Any]),
            reinit_fsm()
      end.

filling_personal_info(timeout, _StateData) ->
    log_timeout("filling_personal_info").

setting_pwd(Event, _From, _StateData) ->
    _CurrentState = setting_pwd,
    case Event of
        {set_pwd, _} ->
            Reply = gen_server:call(?SERVER_NAME, Event),
            {reply, Reply, picking_card, [], ?MAX_WAITINGTIME};
        _Any ->
            error_logger:error_msg("State [~p]: invalid action.~n~p~n", [_CurrentState, _Any]),
            reinit_fsm()
      end.

setting_pwd(timeout, _StateData) ->
    log_timeout("setting_pwd").

picking_card(Event, _From, _StateData) ->
    CurrentState = picking_card,
    case Event of
        pick_card ->
            io:format("Bank cand is picked~n", []),
            {reply, card_is_picked, idle, [], infinity};
         _Any ->
            error_logger:error_msg("State [~p]: invalid action.~n~p~n", [CurrentState, _Any]),
            {reply, error, idle, [], infinity}
      end.

picking_card(timeout, _StateData) ->
    log_timeout("picking_card").

handle_event(Event, StateName, _StateData) ->
    case Event of
        stop ->
            error_logger:info_msg("Gen_fsm [~p] is commanded to stop when it is in state [~p]~n", [?MODULE, StateName]),
            {stop, normal, stopped};
        {reinit, _Res} ->
            reinit_fsm()
     end.

terminate(_Reason, _StateName, _StateData) ->
    error_logger:info_msg("Gen_fsm [~p] is terminated as [~p] in state [~p]~n", [?MODULE, _Reason, _StateName]),
    ok.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> 
    {ok, _StateName, []}.

handle_info(_Info, _StateName, _StateData) ->
    reinit_fsm().

handle_sync_event(_Event, _From, _StateName, _StateData) ->
    {reply, uninvalid_event, idle, [], infinity}.

log_timeout(CurrentState) ->
   error_logger:info_msg("State [~p], timeout.~n", [CurrentState]),
   reinit_fsm().

reinit_fsm() ->
    init([]),
    {next_state, idle, [], infinity}.
   
