PROJECT = treebuilder
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS = cowboy jiffy exec sync

# Whitespace to be used when creating files from templates.
SP = 4

app:: priv/static/js/main.js priv/static/em_main.js

priv/static/em_main.js: priv/js_compile/em_main.cpp priv/compile/pattern.h
	/usr/lib/emscripten/em++ -O2 -s MAIN_MODULE=1 -o $@ -Ipriv/compile $<

%.js : %.jsx
	babel --presets es2015,react -o $@ $<

# priv/static/js/main.js : priv/static/jsx/main.jsx

include erlang.mk
