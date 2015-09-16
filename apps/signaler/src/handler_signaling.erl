-module(handler_signaling).
-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-record(state, {
          ip = undefined :: undefined | binary(),
          state = undefined :: undefined | connected | running,
          room = undefined :: undefined | binary(),
          origin = undefined :: binary()
}).

init(_Any, _Req, _Opt) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(tcp, Req, _Opts) ->
    {Origin, Req1} = cowboy_req:header(<<"origin">>, Req),
    {IP, Req2} = cowboy_req:header(<<"x-forwarded-for">>, Req1),
    State = #state{ip = IP, state = connected, origin = Origin},
    {ok, Req2, State, hibernate}.

websocket_handle({text,Data}, Req, State) ->
    io:format("Received unexpected text message: ~p~n",[Data]),
    {ok, Req, State, hibernate};

websocket_handle({binary, Data}, Req, State) ->
    handle_bin(binary_to_term(Data), Req, State);

websocket_handle(_Any, Req, State) ->
    io:format("Received unexpected message of unknown type ~p~n",[_Any]),
    {ok, Req, State, hibernate}.

websocket_info({broadcast, PidFrom, newpeer}, Req, State) ->
    Resp = term_to_binary({<<"NEW_PEER">>,list_to_binary(pid_to_list(PidFrom))}),
    {reply, {binary, <<Resp/binary>>}, Req, State, hibernate};

websocket_info({frompeer, PidFrom, M}, Req, State) ->
    Resp = term_to_binary({<<"FROM">>,list_to_binary(pid_to_list(PidFrom)),M}),
    {reply, {binary, <<Resp/binary>>}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    io:format("Received unexpected message from other process ~p~n",[_Info]),
    {reply, {text,_Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

broadcast2room(R, Room) ->
    [P ! {broadcast, self(), R} || P <- gproc:lookup_pids({p,l,Room}) -- [self()]].

handle_bin({<<"ROOM_ENTER">>,{RoomParam,_}},Req,State) ->
    Room = {State#state.origin,RoomParam},
    Participants = gproc:lookup_pids({p,l,Room}),
    gproc:reg({p,l,Room}),
    case length(Participants) of
        0 ->
            Resp = term_to_binary({<<"ENTERED_ROOM">>,RoomParam}),
            {reply, {binary, <<Resp/binary>>}, Req, State#state{room = Room}, hibernate};
        1 ->
            broadcast2room(newpeer, Room),
            {ok, Req, State#state{room = Room}, hibernate};
        _ ->
            Resp = term_to_binary({<<"ROOM_IS_FULL">>}),
            {reply, {binary, <<Resp/binary>>}, Req, State, stop}
    end;

handle_bin({<<"TO">>,{To,Message}},Req,State) ->
    ToPid = list_to_pid(binary_to_list(To)),
    Participants = gproc:lookup_pids({p,l,State#state.room}),
    case lists:member(ToPid,Participants) of
        true ->
            ToPid ! {frompeer, self(), Message};
        false ->
            ok
    end,
    {ok, Req, State, hibernate};

handle_bin(Data,Req,State) ->
    io:format("Received unexpected binary message: ~p~n",[Data]),
    {ok, Req, State, hibernate}.
