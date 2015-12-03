PROJECT = treebuilder
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS = cowboy jiffy exec sync erlydtl srly
LOCAL_DEPS=mnesia

# Whitespace to be used when creating files from templates.
SP = 4

SHELL_OPTS=-config rel/sys.config

app:: priv/static/js/main.js priv/static/js/react-codemirror.js priv/static/em_main.js

priv/static/em_main.js: priv/js_compile/em_main.cpp priv/compile/pattern.h
	# the stack on dynamic modules does not seem to free the stack; this is
	# supposed to be running on a uC anyway...
	/usr/lib/emscripten/em++ -s MAIN_MODULE=1 -s TOTAL_MEMORY=33554432 -s TOTAL_STACK=65536 -o $@ -Ipriv/compile $< priv/compile/led_positions.cpp

%.js : %.jsx
	babel --presets es2015,react -o $@ $<

# priv/static/js/main.js : priv/static/jsx/main.jsx

include erlang.mk
