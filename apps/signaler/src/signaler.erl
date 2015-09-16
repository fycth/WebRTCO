%%%-------------------------------------------------------------------
%%% @author Andrii Sergiienko <andrii@sergiienko.me>
%%% @copyright (C) 2015, OSLIKAS OÜ
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2015 1:45 AM
%%%-------------------------------------------------------------------

-module(signaler).
-author("Andrii Sergiienko <andrii@sergiienko.me>").
-behaviour(supervisor).
-behaviour(application).

%% API
-export([init/1,start/2,stop/1,main/1]).

-define(APP, signaler).

main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,?APP},?APP,[]).
stop(_)    -> ok.

port() ->
    [{port, wf:config(n2o,port,8000)}].
env() ->
    [{env, [{dispatch, dispatch_rules()}]}].

init([]) ->
    case cowboy:start_http(http,3,port(),env()) of
        {ok,_} -> ok;
        {error,_} -> halt(abort,[])
    end,
    {ok, {{one_for_one, 5, 10}, []}}.

mime() -> [{mimetypes, cow_mimetypes, all}].

dispatch_rules() ->
  cowboy_router:compile(
    [{'_', [
        {"/WebRTCO-app/signaling/1.9/[...]", handler_signaling, []},
        {'_', n2o_cowboy, []}
    ]}]).
