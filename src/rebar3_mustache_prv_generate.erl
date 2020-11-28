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

-type context() :: #{rebar := rebar_state:t(),
                     app := rebar_app_info:t(),
                     config := rebar3_mustache:config()}.

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
  case handle_apps(Apps, State) of
    ok ->
      {ok, State};
    {error, Reason} ->
      {error, Reason}
  end.

-spec format_error(any()) -> iolist().
format_error(invalid_arguments) ->
  "invalid argument(s)";
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec handle_apps([rebar_app_info:t()], rebar_state:t()) ->
        ok | {error, term()}.
handle_apps([], _State) ->
  ok;
handle_apps([App | Apps], State) ->
  Opts = rebar_app_info:opts(App),
  case dict:find(mustache, Opts) of
    {ok, Config} ->
      Context = #{rebar => State,
                  app => App,
                  config => Config},
      case handle_app(Context) of
        ok ->
          handle_apps(Apps, State);
        {error, Reason} ->
          {error, Reason}
      end;
    error ->
      ok
  end.

-spec handle_app(context()) -> ok | {error, term()}.
handle_app(Context = #{config := Config}) ->
  Templates = maps:get(templates, Config, []),
  handle_templates(Templates, Context).

-spec handle_templates([rebar3_mustache:template()], context()) ->
        ok | {error, term()}.
handle_templates([], _Context) ->
  ok;
handle_templates([Template | Templates], Context) ->
  case handle_template(Template, Context) of
    ok ->
      handle_templates(Templates, Context);
    {error, Reason} ->
      {error, Reason}
  end.

-spec handle_template(rebar3_mustache:template(), context()) ->
        ok | {error, term()}.
handle_template({InputPath, Data}, Context) ->
  handle_template({InputPath, Data, #{}}, Context);
handle_template(Template = {InputPath, Data, _}, #{config := Config}) ->
  OutputPath = rebar3_mustache_templates:output_path(Template),
  Options = rebar3_mustache_templates:options(Template, Config),
  rebar_api:debug("rendering template ~s to ~s", [InputPath, OutputPath]),
  rebar3_mustache_templates:render(InputPath, Data, Options, OutputPath).
