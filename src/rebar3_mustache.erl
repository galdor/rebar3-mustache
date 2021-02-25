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

-module(rebar3_mustache).

-export([init/1,
         default_mustache_options/0,
         format_error/1]).

-export_type([config/0,
              template/0, template_data/0, template_options/0,
              rebar_data/0]).

-type config() :: #{mustache_options => mustache:options(),
                    template_data_path => file:name_all(),
                    template_data_key => mustache:context_key(),
                    templates => [template()]}.
-type template() :: {file:name_all(), template_options()}.
-type template_options() :: #{data => template_data(),
                              output_path => file:name_all(),
                              mustache_options => mustache:options()}.
-type template_data() :: map().

-type rebar_data() :: #{app => atom(),
                        profile => atom()}.

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State2} = rebar3_mustache_prv_generate:init(State),
    {ok, State2}.

-spec default_mustache_options() -> mustache:options().
default_mustache_options() ->
  #{disable_html_escaping => true,
    error_on_unknown_variable => true,
    error_on_unknown_partial => true,
    error_on_invalid_partial => true}.

-spec format_error(any()) -> iolist().
format_error(missing_template_data_key) ->
  "missing template_data_key setting in multi-application project";
format_error({load_template_file, Reason, Path}) ->
  io_lib:format("Cannot load template ~s: ~p", [Path, Reason]);
format_error({load_template_string, Reason, Name}) ->
  io_lib:format("Cannot load template ~s: ~p", [Name, Reason]);
format_error({render_template, Reason, Path}) ->
  io_lib:format("Cannot render template ~s: ~p", [Path, Reason]);
format_error({read_file, Reason, Path}) ->
  io_lib:format("Cannot read file ~s: ~p", [Path, Reason]);
format_error({write_file, Reason, Path}) ->
  io_lib:format("Cannot write file ~s: ~p", [Path, Reason]);
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
