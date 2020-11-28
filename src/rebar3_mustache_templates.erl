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

-module(rebar3_mustache_templates).

-export([output_path/1, options/2, context/2, render/4,
         format_error/1]).

-spec output_path(rebar3_mustache:template()) -> file:name_all().
output_path({InputPath, Data}) ->
  output_path({InputPath, Data, #{}});
output_path({_, _, #{output_path := OutputPath}}) ->
  OutputPath;
output_path({InputPath, _, _}) ->
  %% The default behaviour will generate "name.ext" from "name.ext.mustache"
  %% (or any other final extension).
  case filename:rootname(InputPath) of
    InputPath ->
      %% If the template file path has no extension, we have to find a name to
      %% avoid writing the output over the input file. ".txt" will do.
      filename:flatten([InputPath, ".txt"]);
    OutputPath ->
      OutputPath
  end.

-spec options(rebar3_mustache:template(), rebar3_mustache:config()) ->
        rebar3_mustache:template_options().
options({InputPath, Data}, Config) ->
  options({InputPath, Data, #{}}, Config);
options({_, _, Options}, Config) ->
  %% Start with default mustache options, then merge global options from the
  %% configuration map and and template level options.
  GlobalMustacheOptions = maps:get(mustache_options, Config, #{}),
  TemplateMustacheOptions = maps:get(mustache_options, Options, #{}),
  MustacheOptions = maps:merge(rebar3_mustache:default_mustache_options(),
                               maps:merge(GlobalMustacheOptions,
                                          TemplateMustacheOptions)),
  Options#{mustache_options => MustacheOptions}.

-spec context(rebar3_mustache:template(), rebar3_mustache:config()) ->
        {ok, mustache:context()} | {error, term()}.
context({InputPath, Data}, Config) ->
  context({InputPath, Data, #{}}, Config);
context({_, Data, _}, Config) ->
  case maybe_read_data_file(Config) of
    {ok, ExternalData} ->
      Context = maps:merge(ExternalData, Data),
      {ok, Context};
    {error, Reason} ->
      {error, Reason}
  end.

-spec maybe_read_data_file(rebar3_mustache:config()) ->
        {ok, rebar3_mustache:template_data()} | {error, term()}.
maybe_read_data_file(Config) ->
  case maps:find(data_path, Config) of
    {ok, Path} ->
      read_data_file(Path);
    {error, Reason} ->
      {error, Reason}
  end.

-spec read_data_file(file:name_all()) ->
        {ok, rebar3_mustache:template_data()} | {error, term()}.
read_data_file(Path) ->
  case file:consult(Path) of
    {ok, Terms} ->
      Data = lists:foldl(fun (Term, Acc) ->
                             maps:merge(Acc, Term) end,
                         #{}, Terms),
      {ok, Data};
    {error, Reason} ->
      {error, {?MODULE, {read_file, Reason, Path}}}
  end.

-spec render(file:name_all(), mustache:context(),
             rebar3_mustache:template_options(), file:name_all()) ->
        ok | {error, term()}.
render(InputPath, Context, Options, OutputPath) ->
  MustacheOptions = maps:get(mustache_options, Options,
                             rebar3_mustache:default_mustache_options()),
  case mustache:load_template(InputPath, {file, InputPath}, #{}) of
    {ok, Template} ->
      case mustache:render_template(Template, Context, MustacheOptions) of
        {ok, Data} ->
          case file:write_file(OutputPath, Data) of
            ok ->
              ok;
            {error, Reason} ->
              {error, {?MODULE, {write_file, Reason, OutputPath}}}
          end;
        {error, Reason} ->
          {error, {?MODULE, {render_template, Reason, InputPath}}}
      end;
    {error, Reason} ->
      {error, {?MODULE, {load_template, Reason, InputPath}}}
  end.

-spec format_error(any()) -> iolist().
format_error({load_template, Reason, Path}) ->
  io_lib:format("cannot load template ~s: ~p", [Path, Reason]);
format_error({render_template, Reason, Path}) ->
  io_lib:format("cannot render template ~s: ~p", [Path, Reason]);
format_error({write_file, Reason, Path}) ->
  io_lib:format("cannot write file ~s: ~p", [Path, Reason]);
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
