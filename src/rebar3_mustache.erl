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
         rebar_data/2]).

-export_type([config/0,
              template/0, template_data/0, template_options/0,
              rebar_data/0]).

-type config() :: #{mustache_options => mustache:options(),
                    template_data_path => file:name_all(),
                    templates => [template()]}.
-type template() :: {file:name_all(), template_data()}
                  | {file:name_all(), template_data(),
                     template_options()}.
-type template_data() :: map().
-type template_options() :: #{output_path => file:name_all(),
                              mustache_options => mustache:options()}.

-type rebar_data() :: #{profile => atom()}.

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

-spec rebar_data(rebar_state:t(), rebar_app_info:t()) -> rebar_data().
rebar_data(_State, _App) ->
  AppName = binary_to_atom(rebar_app_info:name(App)),
  #{app => AppName}.
