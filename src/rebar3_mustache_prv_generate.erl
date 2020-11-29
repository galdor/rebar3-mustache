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

-type context() :: #{state := rebar_state:t(),
                     app := rebar_app_info:t(),
                     config := rebar3_mustache:config(),
                     template_data => rebar3_mustache:template_data(),
                     profile => atom()}.

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
  Profiles = rebar_state:current_profiles(State),
  try
    lists:foreach(fun ({App, Profile}) ->
                      handle_app(App, Profile, State)
                  end, [{A, P} || A <- Apps, P <- Profiles]),
    {ok, State}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec format_error(any()) -> iolist().
format_error(invalid_arguments) ->
  "invalid argument(s)";
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec handle_app(rebar_app_info:t(), Profile :: atom(), rebar_state:t()) -> ok.
handle_app(App, Profile, State) ->
  Opts = rebar_app_info:opts(App),
  case dict:find(mustache, Opts) of
    {ok, Config} ->
      Context = #{state => State,
                  app => App,
                  config => Config,
                  profile => Profile},
      Context2 = maybe_load_template_data(Context),
      handle_app(Context2);
    error ->
      ok
  end.

-spec maybe_load_template_data(context()) -> context().
maybe_load_template_data(Context = #{config := Config}) ->
  case maps:find(template_data_path, Config) of
    {ok, Path} ->
      rebar_api:debug("Loading template data from ~s", [Path]),
      case rebar3_mustache_templates:read_data_file(Path) of
        {ok, Data} ->
          Context#{template_data => Data};
        {error, Reason} ->
          throw({error, Reason})
      end;
    error ->
      Context
  end.

-spec handle_app(context()) -> ok.
handle_app(Context = #{config := Config}) ->
  Templates = maps:get(templates, Config, []),
  lists:foreach(fun (T) ->
                    handle_template(T, Context)
                end, Templates).

-spec handle_template(rebar3_mustache:template(), context()) -> ok.
handle_template(Template = {InputPath, _},
                Context = #{config := Config, app := App}) ->
  OutputPath = rebar3_mustache_templates:output_path(Template),
  GlobalTemplateData = maps:get(template_data, Context, #{}),
  MustacheContext = rebar3_mustache_templates:mustache_context(
                      Template, GlobalTemplateData, App, rebar_data(Context)),
  Options = rebar3_mustache_templates:options(Template, Config),
  rebar_api:debug("Rendering template ~s to ~s", [InputPath, OutputPath]),
  case
    rebar3_mustache_templates:render(InputPath, MustacheContext, Options,
                                     OutputPath)
  of
    ok ->
      ok;
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec rebar_data(context()) -> rebar3_mustache:rebar_data().
rebar_data(#{app := App, profile := Profile}) ->
  AppName = binary_to_atom(rebar_app_info:name(App)),
  #{app => AppName,
    profile => Profile}.
