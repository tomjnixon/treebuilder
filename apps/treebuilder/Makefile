PROJECT = treebuilder
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy jiffy exec erlydtl
LOCAL_DEPS = mnesia sasl

app:: priv/static/js/main.js priv/static/js/react-codemirror.js priv/static/em_main.js

priv/static/em_main.js: priv/js_compile/em_main.cpp priv/compile/pattern.h
	# the stack on dynamic modules does not seem to free the stack; this is
	# supposed to be running on a uC anyway...
	/usr/lib/emscripten/em++ -s MAIN_MODULE=1 -s TOTAL_MEMORY=33554432 -s TOTAL_STACK=65536 -o $@ -Ipriv/compile $< priv/compile/led_positions.cpp

%.js : %.jsx
	babel --presets es2015,react -o $@ $<

include ../../erlang.mk
