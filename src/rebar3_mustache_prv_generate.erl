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

-export([init/1, do/1]).

%% Plugins are based on the behaviour defined in
%% https://github.com/tsloughter/providers/blob/master/src/provider.erl. But
%% for some reason we cannot use "-behaviour(provider)" (callback info not
%% available, etc.). Do not ask.

-type context() :: #{state := rebar_state:t(),
                     app => rebar_app_info:t(),
                     config := rebar3_mustache:config(),
                     rebar_data := rebar3_mustache:rebar_data(),
                     template_data => rebar3_mustache:template_data(),
                     profile => atom(),
                     base_path := filename:name_all()}.

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
  App = rebar_state:current_app(State),
  Profiles = rebar_state:current_profiles(State),
  try
    case rebar_state:current_app(State) of
      undefined ->
        lists:foreach(fun (P) -> process_project(P, State) end, Profiles);
      App ->
        lists:foreach(fun (P) -> process_app(App, P, State) end, Profiles)
    end,
    {ok, State}
  catch
    %% Returning error values yields meaningless error messages such as
    %% "command 'Reason' in namespace 'Module' not found." instead of calling
    %% Module:format_error(Reason). It used to work. I have no patience for
    %% this kind of regression, and abort always works. So abort it is.
    throw:{error, Reason} ->
      rebar_utils:abort("Mustache error: ~ts",
                        [rebar3_mustache:format_error(Reason)])
  end.

-spec process_project(Profile :: atom(), rebar_state:t()) -> ok.
process_project(Profile, State) ->
  debug("processing multi-application project", []),
  Opts = rebar_state:opts(State),
  case dict:find(mustache, Opts) of
    {ok, Config} ->
      Context = #{state => State,
                  config => Config,
                  profile => Profile,
                  rebar_data => rebar_data(Profile),
                  base_path => "."},
      Context2 = maybe_load_template_data(Context),
      process_context(Context2);
    error ->
      ok
  end.

-spec process_app(rebar_app_info:t(), Profile :: atom(), rebar_state:t()) -> ok.
process_app(App, Profile, State) ->
  debug("handling application ~s at ~s",
        [rebar_app_info:name(App), rebar_app_info:dir(App)]),
  Opts = rebar_app_info:opts(App),
  case dict:find(mustache, Opts) of
    {ok, Config} ->
      Context = #{state => State,
                  app => App,
                  config => Config,
                  profile => Profile,
                  rebar_data => rebar_data(Profile, App),
                  base_path => rebar_app_info:dir(App)},
      Context2 = maybe_load_template_data(Context),
      process_context(Context2);
    error ->
      ok
  end.

-spec maybe_load_template_data(context()) -> context().
maybe_load_template_data(Context = #{config := Config,
                                     rebar_data := RebarData,
                                     base_path := BasePath}) ->
  case maps:find(template_data_path, Config) of
    {ok, PathTemplateString} ->
      MustacheContext = #{rebar => RebarData},
      Path0 = render_string(PathTemplateString, MustacheContext, #{}),
      Path = filename:join(BasePath, Path0),
      debug("loading template data from ~s", [Path]),
      case rebar3_mustache_templates:read_data_file(Path) of
        {ok, Data} ->
          Context#{template_data => Data};
        {error, Reason} ->
          throw({error, Reason})
      end;
    error ->
      Context
  end.

-spec process_context(context()) -> ok.
process_context(Context = #{config := Config}) ->
  case maps:find(app, Context) of
    {ok, App} ->
      debug("rendering templates for application ~s",
            [rebar_app_info:name(App)]);
    error ->
      debug("rendering templates", [])
  end,
  Templates = maps:get(templates, Config, []),
  lists:foreach(fun (T) ->
                    process_template(T, Context)
                end, Templates).

-spec process_template(rebar3_mustache:template(), context()) -> ok.
process_template(Template = {InputPath0, _},
                 Context = #{config := Config,
                             rebar_data := RebarData,
                             base_path := BasePath}) ->
  Key = case maps:find(template_data_key, Config) of
          {ok, K} ->
            K;
          error ->
            case maps:find(app, Context) of
              {ok, App} ->
                binary_to_atom(rebar_app_info:name(App));
              error ->
                throw({error, missing_template_data_key})
            end
        end,
  TemplateData = maps:get(template_data, Context, #{}),
  BaseContext = maps:merge(#{rebar => RebarData},
                           #{Key => TemplateData}),
  TemplateContext = rebar3_mustache_templates:mustache_context(Template),
  MustacheContext = maps:merge(BaseContext, TemplateContext),
  InputPath = filename:join(BasePath, InputPath0),
  OutputPath0 = output_path(Template, BaseContext),
  OutputPath = filename:join(BasePath, OutputPath0),
  debug("rendering template ~s to ~s", [InputPath, OutputPath]),
  Options = rebar3_mustache_templates:options(Template, Config),
  render_file(InputPath, OutputPath, MustacheContext, Options).

-spec output_path(rebar3_mustache:template(),
                  mustache:context()) -> file:name_all().
output_path(Template, Context) ->
  PathTemplateString = rebar3_mustache_templates:output_path(Template),
  render_string(PathTemplateString, Context, #{}).

-spec rebar_data(Profile :: atom()) -> rebar3_mustache:rebar_data().
rebar_data(Profile) ->
  #{profile => Profile}.

-spec rebar_data(Profile :: atom(), rebar_app_info:t()) ->
        rebar3_mustache:rebar_data().
rebar_data(Profile, App) ->
  AppName = binary_to_atom(rebar_app_info:name(App)),
  #{profile => Profile,
    app => AppName}.

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

-spec debug(string(), [term()]) -> ok.
debug(Format, Args) ->
  rebar_api:debug("Mustache: " ++ Format, Args).
