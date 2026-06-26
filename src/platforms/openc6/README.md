<!---
  This file is part of AtomVM.

  Copyright 2026 Peter M <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# openC6 platform support for AtomVM

This directory contains the platform port of AtomVM for running as a bare-metal payload application on the ESP32-C6 using the openC6 BIOS.

## Prerequisites

1. **ESP-IDF v5.5** toolchain active in your environment (providing the `riscv32-esp-elf-` toolchain).
2. **openC6 BIOS** cloned and built locally (e.g. at `/path/to/openc6-bios`).

## Building

To build the AtomVM payload, run the following commands:

```bash
cmake -S src/platforms/openc6 -B build \
  -DCMAKE_TOOLCHAIN_FILE=src/platforms/openc6/openc6-toolchain.cmake \
  -DOPENC6_BIOS_PATH=/path/to/openc6-bios

cmake --build build
```

This will compile the payload and generate `build/payload.bin`.

## Simulating with Wokwi

CMake will automatically generate the configured simulation files (`wokwi.toml`, `flasher_args.json`, and `diagram.json`) in the `build/` directory using the templates.

To run the simulation:

1. Compile the Erlang test application and build the AVM archive:
   ```bash
   # From src/platforms/openc6/wokwi
   ./build_sim.sh
   ```
   *Note: This script compiles `main.erl` and generates `build/main.avm` inside the build directory.*

2. Run `wokwi-cli` inside the build directory:
   ```bash
   wokwi-cli --timeout 10000 build/
   ```
