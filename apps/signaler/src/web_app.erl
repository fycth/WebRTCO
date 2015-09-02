%%%-------------------------------------------------------------------
%%% @author Andrii Sergiienko <andrii@sergiienko.me>
%%% @copyright (C) 2015, OSLIKAS OÜ
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2015 1:45 AM
%%%-------------------------------------------------------------------

-module(web_app).
-author("Andrii Sergiienko <andrii@sergiienko.me>").
-behaviour(application).

%% API
-export([start/0, start/2, stop/1, main/1]).

main(A) ->
  mad_repl:main(A).

start() -> start(normal, []).
start(_StartType, _StartArgs) -> web_sup:start_link().

stop(_State) -> ok.
