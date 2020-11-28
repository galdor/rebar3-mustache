%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(rebar3_mustache_prv_generate).

-export([init/1, do/1, format_error/1]).

%% Plugins are based on the behaviour defined in
%% https://github.com/tsloughter/providers/blob/master/src/provider.erl. But
%% for some reason we cannot use "-behaviour(provider)" (callback info not
%% available, etc.). Do not ask.

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  P = providers:create([{name, generate},
                        {namespace, mustache},
                        {module, ?MODULE},
                        {bare, true},
                        {deps, [{default, app_discovery}]},
                        {opts, []},
                        {example,
                         "rebar3 mustache generate"},
                        {short_desc,
                         "Generate files based on Mustache templates."},
                        {desc,
                         "Generate files based on Mustache templates."}]),
  {ok, rebar_state:add_provider(State, P)}.

-spec do(rebar_state:t()) ->
        {ok, rebar_state:t()} | {error, string()} | {error, {module(), any()}}.
do(State) ->
  Apps = case rebar_state:current_app(State) of
           undefined -> rebar_state:project_apps(State);
           App -> [App]
         end,
  case handle_apps(Apps) of
    ok ->
      {ok, State};
    {error, Reason} ->
      {error, {rebar3_mustache, Reason}}
  end.

-spec format_error(any()) -> iolist().
format_error(invalid_arguments) ->
  "invalid argument(s)";
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec handle_apps([rebar_app_info:t()]) -> ok | {error, term()}.
handle_apps([]) ->
  ok;
handle_apps([App | Apps]) ->
  case handle_app(App) of
    ok ->
      handle_apps(Apps);
    {error, Reason} ->
      {error, Reason}
  end.

-spec handle_app(rebar_app_info:t()) -> ok | {error, term()}.
handle_app(_App) ->
  ok.
