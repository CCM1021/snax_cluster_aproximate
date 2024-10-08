// Copyright 2023 KU Leuven.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0
//
// Xiaoling Yi <xiaoling.yi@esat.kuleuven.be>

#include <stdbool.h>
#include "snrt.h"
#include "stdint.h"

#pragma once

// Set STREAMER configuration CSR
void set_streamer_csr(int tempLoop0, int tempLoop1, int tempLoop2,
                      int tempStride0A, int tempStride2A, int spatialStride1A,
                      int tempStride0B, int tempStride1B, int spatialStride1B,
                      int tempStride1C, int tempStride2C, int spatialStride1C,
                      int delta_local_a, int delta_local_b, int delta_local_c,
                      int delta_local_d);

// Set CSR to start STREAMER
void set_streamer_start();

// Set GEMM configuration CSR
void set_block_gemm_csr(int tempLoop0, int tempLoop1, int tempLoop2,
                        int subtractions);

// Set CSR to start GEMM
void set_block_gemm_start();

// Poll until Streamer and GEMM accelerator finish
void wait_streamer_gemm();

// Read perforamcne counter of the Streamer, a read-only CSR
uint32_t read_gemm_streamer_perf_counter();

// Read perforamcne counter of GEMM, a read-only CSR
uint32_t read_gemm_perf_counter();
