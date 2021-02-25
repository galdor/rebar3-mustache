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
                     rebar_data := rebar3_mustache:rebar_data(),
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
           undefined ->
             ProjectApps = rebar_state:project_apps(State),
             Names = lists:map(fun rebar_app_info:name/1, ProjectApps),
             rebar_api:debug("Project applications: ~0p", [Names]),
             ProjectApps;
           App ->
             rebar_api:debug("Current application: ~s",
                             [rebar_app_info:name(App)]),
             [App]
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
                  profile => Profile,
                  rebar_data => rebar_data(App, Profile)},
      Context2 = maybe_load_template_data(Context),
      handle_app(Context2);
    error ->
      ok
  end.

-spec maybe_load_template_data(context()) -> context().
maybe_load_template_data(Context = #{config := Config,
                                     rebar_data := RebarData}) ->
  case maps:find(template_data_path, Config) of
    {ok, PathTemplateString} ->
      MustacheContext = #{rebar => RebarData},
      Path = render_string(PathTemplateString, MustacheContext, #{}),
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
handle_app(Context = #{config := Config, app := App}) ->
  rebar_api:debug("Rendering templates for application ~s",
                  [rebar_app_info:name(App)]),
  Templates = maps:get(templates, Config, []),
  lists:foreach(fun (T) ->
                    handle_template(T, Context)
                end, Templates).

-spec handle_template(rebar3_mustache:template(), context()) -> ok.
handle_template(Template = {InputPath, _},
                Context = #{config := Config,
                            app := App,
                            rebar_data := RebarData}) ->
  AppName = binary_to_atom(rebar_app_info:name(App)),
  AppTemplateData = maps:get(template_data, Context, #{}),
  BaseContext = maps:merge(#{rebar => RebarData},
                           #{AppName => AppTemplateData}),
  TemplateContext = rebar3_mustache_templates:mustache_context(Template),
  MustacheContext = maps:merge(BaseContext, TemplateContext),
  OutputPath = output_path(Template, BaseContext),
  Options = rebar3_mustache_templates:options(Template, Config),
  rebar_api:debug("Rendering template ~s to ~s", [InputPath, OutputPath]),
  render_file(InputPath, OutputPath, MustacheContext, Options).

-spec output_path(rebar3_mustache:template(),
                  mustache:context()) -> file:name_all().
output_path(Template, Context) ->
  PathTemplateString = rebar3_mustache_templates:output_path(Template),
  render_string(PathTemplateString, Context, #{}).

-spec rebar_data(rebar_app_info:t(), Profile :: atom()) ->
        rebar3_mustache:rebar_data().
rebar_data(App, Profile) ->
  AppName = binary_to_atom(rebar_app_info:name(App)),
  #{app => AppName,
    profile => Profile}.

-spec render_file(file:name_all(), file:name_all(), mustache:context(),
                  rebar3_mustache:template_options()) -> ok.
render_file(InputPath, OutputPath, Context, Options) ->
  case
    rebar3_mustache_templates:render_file(InputPath, OutputPath,
                                          Context, Options)
  of
    ok ->
      ok;
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec render_string(binary() | string(), mustache:context(),
                    rebar3_mustache:template_options()) -> binary() | string().
render_string(Input, Context, Options) ->
  case
    rebar3_mustache_templates:render_string(Input, Context, Options)
  of
    {ok, Output} ->
      Output;
    {error, Reason} ->
      throw({error, Reason})
  end.
