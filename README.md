# Treebuilder

More documentation to come...

maybe

## Available releases

There are a few releases defined:

- treebuilder: just the web interface, sketch storage, and compilation
  infrastructure
- device_manager: just the part that talks to the microcontroller
- combined: both of the above in one release
- local: same as treebuilder, used for development.

These can be built by running `make release=$RELEASE_NAME`

A complete configuration will therefore either be the 'treebuilder' release
running on a server, and the device_manager release running on something small
connected to the microcontroller, or just the 'combined' release running on
something connected to the microcontroller.

## Emscripten

The code currently requires an old version of emscripten, because of the
removal of JS side modules.

Get it with:

    git clone https://github.com/emscripten-core/emsdk.git
    cd emsdk
    ./emsdk install 1.38.48
    ./emsdk activate --embedded 1.38.48

Then, before building or running, run:

    source path_to_emsdk/emsdk_env.sh

## Teensy-loader-cli

Install the git version on the device manager; the current release doesn't seem
to work properly: https://aur.archlinux.org/packages/teensy-loader-cli-git/

## Creating the database

The database must be created manually, by running:

    sketch_manager:install([node()]).

on the node you want to install on.

If this has worked, as well as info messages this should return:

    {[ok],[]}

To get to an appropriately configured shell, either run 'make shell' if you're
planning on running the 'local' release, or from a release directory, run:

    CODE_LOADING_MODE=interactive ./bin/treebuilder_treebuilder_release console_clean
