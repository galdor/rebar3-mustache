# Project
This repository contains a [Rebar3](http://rebar3.org) plugin to generate
files based on template mustaches.

# Usage
The `rebar3 mustache generate` command will render all templates defined in
the configuration. For each OTP application, the plugin will render templates
once for each profile specified.

The configuration is stored in `rebar.config`:

Example:
```erlang
{mustache,
 #{template_data_path => "demo-data-{{rebar.profile}}.erl",
   templates =>
     [{"demo.txt.mustache",
       #{output_path => "demo-{{rebar.profile}}.txt"}}]}}.
```

## Context
Each template rendering operation uses a context, i.e. a set of data
accessible from the template. The context used for each template can be made
of up to three layers:

- Rebar data:
  ```erlang
  #{rebar => Data}.
  ```
  Where `Data` contains the following entries:
  - `app`: the name of the OTP application.
  - `profile`: the name of the current Rebar3 profile.
- Global template data:
  ```erlang
  #{AppName => Data}.
  ```
  Where `AppName` is the name of the OTP applicataion and `Data` the set of
  data loaded from the file reference by the `template_data_path`
  configuration entry.
- Template data: the set of data provided with the `data` template option.

These layers are merged before rendering.

For example, for an application named `demo` built with profile `prod`, using
a template data file containing `#{a => 1}` and a `data` template option set
to `#{b => 2}`, the context will be:

```erlang
#{rebar => #{app => demo, profile => prod},
  demo => #{a => 1},
  b => 2}.
```

## Configuration entries
- `template_data_path`: the path of a file containing one or more Erlang maps;
  all maps will be merged during loading. Note that this path is treated as a
  template, using Rebar data as context.
- `templates` a list of templated files to generate; each entry is a tuple
  whose first element is the path of the template file, and whose second
  element is a set of template options.

## Template options
- `output_path`: the path of the file to generate; if it is not set, the
  plugin will generate a name based on the path of the template file. Note
  that this path is treated as a template, using both Rebar data and global
  template data as context.
- `data`: an Erlang map which will be merged into the template context during
  rendering.
- `mustache_options`: a set of
  [mustache](https://github.com/galdor/erl-mustache) template options; they
  will be merged with default options.

# Contact
If you find a bug or have any question, feel free to open a GitHub issue or to
contact me [by email](mailto:khaelin@gmail.com).
