reload.mk
=========

![travis-ci](https://travis-ci.org/bullno1/reload.mk.svg?branch=master)

A live reload plugin for [erlang.mk](https://github.com/ninenines/erlang.mk).

Usage
-----

First, add `reload.mk` as a plugin and a build dependency:

```Makefile
PROJECT = myproject
BUILD_DEPS = reload_mk
DEP_PLUGINS = reload_mk

# Until it's accepted into erlang.mk index, we need to point to the repo
dep_reload_mk = git https://github.com/bullno1/reload.mk master

include erlang.mk
```

Enable `dev_mode` in the release by adding the following line to your `relx.config`:

```erlang
{dev_mode, true}.
```

Then in one terminal: `make run`.
Keep it running.

Now you can modify any source files in your project and run `make reload`:

```shell
% make reload
 DEPEND myproject.d
 ERLC   myproject.erl myproject_app.erl
 APP    myproject
 RELOAD myproject

=INFO REPORT==== 3-Oct-2015::15:05:18 ===
    reload_mk: "Reloaded"
    module: myproject
    path: "/home/bullno1/Projects/myproject/_rel/myproject_release/lib/myproject-rolling/ebin/myproject.beam"
** at node myproject@127.0.0.1 **
ok
```

To avoid having to type `make reload` all the time, use `make auto-reload`.
You need to have `inotify-tools` installed for this command to work.

An example project can be found at https://github.com/bullno1/reload.mk-example.

Configuration
-------------

`RELOAD_MK_WATCH_DIRS` can be set to a list of directories that `make auto-reload` needs to watch.
For example, if you are using `erlydtl` templates, put the following in your `Makefile`:

```Makefile
RELOAD_MK_WATCH_DIRS = src templates
```

By default, it is set to `src deps`.
