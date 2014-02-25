-module(handler_websocket).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-record(state, {
          client = undefined :: undefined | binary(),
          state = undefined :: undefined | connected | running,
          room = undefined :: undefined | integer()
}).

init(_Any, _Req, _Opt) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opt) ->
    {Client, Req1} = cowboy_req:header(<<"x-forwarded-for">>, Req),
    State = #state{client = Client, state = connected},
    lager:info("Connection from: ~p", [Client]),
    {ok, Req1, State, hibernate}.

websocket_handle({text,Data}, Req, State) ->
    StateNew = case (State#state.state) of
                   started ->
                       State#state{state = running};
                   _ ->
                       State
                end,
    JSON = jsonerl:decode(Data),
    {M,Type} = element(1,JSON),
    case M of
        <<"type">> ->
            case Type of
                <<"TOPEER">> ->
                  {<<"to">>,Send_To} = element(3,JSON),
                  To = list_to_pid(binary_to_list(Send_To)),
                  To ! Data,
                  {ok, Req, StateNew, hibernate};
                <<"ROOM_CREATE">> ->
                    Room = generate_room(),
                    R = iolist_to_binary(jsonerl:encode({{type, <<"ROOM_ENTERED">>}, {value, Room}, {pid, list_to_binary(pid_to_list(self()))}})),
                    gproc:reg({p,l, Room}),
                    S = (StateNew#state{room = Room}),
                    {reply, {text, <<R/binary>>}, Req, S, hibernate};
                <<"ROOM_ENTER">> ->
                    {<<"value">>,Room} = element(2,JSON),
%                    Participants = gproc:lookup_pids({p,l,Room}),
%                    case length(Participants) of
%                        1 ->
%                            gproc:reg({p,l, Room}),
%                            S = (StateNew#state{room = Room}),
%                            {ok, Req, S, hibernate};
%                        _ ->
%                            R = iolist_to_binary(jsonerl:encode({{type, <<"ROOM_LEAVE">>}})),
%                            {reply, {text, <<R/binary>>}, Req, StateNew, hibernate}
%                    end;
                     gproc:reg({p,l, Room}),
                     B = iolist_to_binary(jsonerl:encode({{type, <<"PEER_JOINED">>}, {pid, list_to_binary(pid_to_list(self()))}})),
                     reply2peer(B, Room),
                     R = iolist_to_binary(jsonerl:encode({{type, <<"ROOM_ENTERED">>}, {value, Room}, {pid, list_to_binary(pid_to_list(self()))}})),
                     S = (StateNew#state{room = Room}),
                     {reply, {text, <<R/binary>>}, Req, S, hibernate};
                _ ->
                    reply2peer(Data, StateNew#state.room),
                    {ok, Req, StateNew, hibernate}
            end;
        _ ->
            reply2peer(Data, State#state.room),
            {ok, Req, StateNew, hibernate}
    end;

websocket_handle(_Any, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
    {reply, {text,_Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

reply2peer(R, Room) ->
%    lager:info("re-transmit packet: ~p", [R]),
    [P ! <<R/binary>> || P <- gproc:lookup_pids({p,l,Room}) -- [self()]].

generate_room() ->
    random:seed(now()),
    random:uniform(999999).

%pids2string(A,[H|T]) ->
%  pids2string(A ++ [list_to_binary(pid_to_list(H))],T);
%pids2string(A,[]) ->
%  A.
