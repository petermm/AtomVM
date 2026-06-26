#!/bin/bash
# Compile and package Erlang test application for openC6 Wokwi simulation
#
# This file is part of AtomVM.
#
# Copyright 2026 Peter M <petermm@gmail.com>
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
set -e

# Change directory to the script's location
cd "$(dirname "$0")"

BUILD_DIR="../build"
if [ ! -d "$BUILD_DIR" ]; then
    mkdir -p "$BUILD_DIR"
fi

echo "Compiling main.erl to main.beam..."
erlc -o "$BUILD_DIR" main.erl

echo "Packaging main.beam into main.avm..."
../../../../build/tools/packbeam/packbeam create --start main "$BUILD_DIR/main.avm" "$BUILD_DIR/main.beam"

echo "Success! main.avm compiled and ready in $BUILD_DIR."
