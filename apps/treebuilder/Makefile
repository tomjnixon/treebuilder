PROJECT = treebuilder
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.6

DEPS = cowboy jiffy exec erlydtl ebus
LOCAL_DEPS = mnesia sasl

dep_cowboy_commit = 2.6.1
dep_ranch_commit = 1.7.1
DEP_PLUGINS = cowboy

app:: priv/static/js/main.js priv/static/js/react-codemirror.js priv/static/em_main.js

priv/static/em_main.js: priv/js_compile/em_main.cpp priv/compile/pattern.h priv/compile/led_positions.cpp
	# the stack on dynamic modules does not seem to free the stack; this is
	# supposed to be running on a uC anyway...
	em++ -s EXPORT_ALL=1 -s MAIN_MODULE=1 -s WASM=0 -s TOTAL_MEMORY=33554432 -s TOTAL_STACK=65536 -o $@ -Ipriv/compile $< priv/compile/led_positions.cpp

%.js : %.jsx
	babel --presets es2015,react -o $@ $<

include ../../erlang.mk
