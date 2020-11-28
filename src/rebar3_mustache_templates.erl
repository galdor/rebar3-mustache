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

-export([output_path/2, render/4]).

-spec output_path(file:name_all(), rebar3_mustache:template_options()) ->
        file:name_all().
output_path(_, #{output_path := OutputPath}) ->
  OutputPath;
output_path(InputPath, _) ->
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

-spec render(file:name_all(), mustache:context(),
             rebar3_mustache:template_options(), file:name_all()) ->
        ok | {error, term()}.
render(InputPath, Context, Options, OutputPath) ->
  case mustache:load_template(InputPath, {file, InputPath}, #{}) of
    {ok, Template} ->
      case mustache:render_template(Template, Context, Options) of
        {ok, Data} ->
          case file:write_file(OutputPath, Data) of
            ok ->
              ok;
            {error, Reason} ->
              {error, {rebar3_mustache_templates, {write_file, Reason}}}
          end;
        {error, Reason} ->
          {error, {rebar3_mustache_templates, {render_template, Reason}}}
      end;
    {error, Reason} ->
      {error, {rebar3_mustache_templates, {load_template, Reason}}}
  end.
