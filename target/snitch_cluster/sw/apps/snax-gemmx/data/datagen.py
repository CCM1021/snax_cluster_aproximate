#!/usr/bin/env python3

# Copyright 2024 KU Leuven.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0
#
# Xiaoling Yi <xiaoling.yi@esat.kuleuven.be>

import numpy as np
import argparse
import pathlib
import hjson
import sys
import os

# Add data utility path
sys.path.append(os.path.join(os.path.dirname(__file__), "../../../../../../util/sim/"))
from data_utils import format_scalar_definition, format_vector_definition  # noqa E402

# Add golden model path
from snax_utils import (  # noqa E402
    conv2d,
    im2col,
    block_gemm_golden_model,
    data_reshuffler_golden_model,
    postprocessing_simd_golden_model,
    align_wide_addr,
)  # noqa E402

np.random.seed(42)


# Add stdint.h header
def emit_header_file(**kwargs):
    emit_str = "#include <stdint.h>\n\n"
    emit_str += emit_gemmx_data(**kwargs)
    return emit_str


MIN = -128
MAX = 127

bankWidth = 64
input_data_width = 8
output_data_width = 32
quantized_output_data_width = 8


def emit_conv_data(**kwargs):
    Cin = kwargs["Cin"]
    Cout = kwargs["Cout"]
    if kwargs["ifC8HW8datalayout"] is True:
        Nbatch, Cin8, H, W, _ = (
            kwargs["Nbatch"],
            kwargs["Cin"] // 8,
            kwargs["H"],
            kwargs["W"],
            8,
        )
        Cout8, Cin8, Kh, Kw, _, _ = (
            kwargs["Cout"] // 8,
            kwargs["Cin"] // 8,
            kwargs["Kh"],
            kwargs["Kw"],
            8,
            8,
        )

        # test data generation
        input_data = np.random.randint(-10, 10, size=(Nbatch, Cin8, H, W, 8))
        kernel = np.random.randint(-10, 10, size=(Cout8, Cin8, Kh, Kw, 8, 8))
    else:
        # conv2d settings
        Nbatch, H, W, Cin = (kwargs["Nbatch"], kwargs["H"], kwargs["W"], kwargs["Cin"])
        Cout, Kh, Kw, Cin = (kwargs["Cout"], kwargs["Kh"], kwargs["Kw"], kwargs["Cin"])

        # test data generation
        input_data = np.random.randint(MIN, MAX, size=(Nbatch, H, W, Cin))
        kernel = np.random.randint(MIN, MAX, size=(Cout, Kh, Kw, Cin))
    stride_h, stride_w = (kwargs["stride_h"], kwargs["stride_w"])
    pad_h, pad_w = (kwargs["pad_h"], kwargs["pad_w"])

    # inferred config from the input data and kernel
    padding = pad_h, pad_w
    stride = stride_h, stride_w

    # Padding the input data

    if kwargs["ifC8HW8datalayout"] is True:
        input_padding = np.pad(
            input_data,
            ((0, 0), (0, 0), (pad_h, pad_h), (pad_w, pad_w), (0, 0)),
            mode="constant",
        )
    else:
        input_padding = np.pad(
            input_data,
            ((0, 0), (pad_h, pad_h), (pad_w, pad_w), (0, 0)),
            mode="constant",
        )

    # Calculate the size of the output feature map
    out_height = (H + 2 * pad_h - Kh) // stride_h + 1
    out_width = (W + 2 * pad_w - Kw) // stride_w + 1

    assert out_width % 8 == 0, "out_width must be multiple of 8"

    M = out_height * out_width // 8
    K = Cin // 8 * Kh * Kw
    N = Cout // 8

    length_c = M * N * 8 * 8
    # bias = np.random.randint(MIN, MAX, length_c)
    bias = np.random.randint(-(2**30), 2**30 - 1, length_c)

    data_str = []

    # Generating conv2d settings
    data_str += [
        format_scalar_definition("int", "Nbatch", Nbatch),
        format_scalar_definition("int", "H", H),
        format_scalar_definition("int", "W", W),
        format_scalar_definition("int", "Cin", Cin),
        format_scalar_definition("int", "Cout", Cout),
        format_scalar_definition("int", "Kh", Kh),
        format_scalar_definition("int", "Kw", Kw),
        format_scalar_definition("int", "stride_h", stride_h),
        format_scalar_definition("int", "stride_w", stride_w),
        format_scalar_definition("int", "pad_h", pad_h),
        format_scalar_definition("int", "pad_w", pad_w),
    ]

    # Generating matrix size settings
    data_str += [
        format_scalar_definition("int", "Batch", Nbatch),
        format_scalar_definition("int", "M", M),
        format_scalar_definition("int", "K", K),
        format_scalar_definition("int", "N", N),
    ]

    # Generating base pointer settings
    delta_local_a = 0

    delta_local_b = input_padding.size
    assert input_padding.size == (Nbatch * Cin8 * (H + 2 * pad_h) * (W + 2 * pad_w) * 8)

    delta_local_b = align_wide_addr(delta_local_b, 64)
    assert delta_local_b % 64 == 0

    delta_local_c = delta_local_b + kernel.size
    assert kernel.size == (Cout8 * Cin8 * Kh * Kw * 8 * 8)
    delta_local_c = align_wide_addr(delta_local_c, 64)
    assert delta_local_c % 64 == 0

    delta_local_d8 = delta_local_c + length_c * 4
    delta_local_d8 = align_wide_addr(delta_local_d8, 64)
    assert delta_local_d8 % 64 == 0

    delta_local_d32 = delta_local_d8
    data_str += [
        format_scalar_definition("int32_t", "delta_local_a", delta_local_a),
        format_scalar_definition("int32_t", "delta_local_b", delta_local_b),
        format_scalar_definition("int32_t", "delta_local_d8", delta_local_d8),
        format_scalar_definition("int32_t", "delta_local_c", delta_local_c),
        format_scalar_definition("int32_t", "delta_local_d32", delta_local_d32),
    ]

    # for streamer cfg
    # streamer setting for data mover A
    if kwargs["ifC8HW8datalayout"] is True:
        # NC8HW8
        Aslstride0 = 1
        Aslstride1 = 8 * stride_w

        # K dim
        Atlbound0 = Kw
        Atlstride0 = 8

        Atlbound1 = Kh
        Atlstride1 = 8 * (W + 2 * pad_w)

        Atlbound2 = Cin8
        Atlstride2 = 8 * (W + 2 * pad_w) * (H + 2 * pad_h)

        # N dim
        Atlbound3 = Cout // 8
        Atlstride3 = 0

        # M dim
        Atlbound4 = out_width // 8
        Atlstride4 = 8 * 8 * stride_w

        Atlbound5 = out_height
        Atlstride5 = 8 * (W + 2 * pad_w) * stride_h

        # Batch dim
        Atlbound6 = Nbatch
        Atlstride6 = 8 * Cin8 * (H + 2 * pad_h) * (W + 2 * pad_w)

    else:
        # NHWC
        Aslstride0 = 1
        Aslstride1 = Cin * stride_w

        # K dim
        Atlbound0 = Cin // 8
        Atlstride0 = 8

        Atlbound1 = Kw
        Atlstride1 = Cin

        Atlbound2 = Kh
        Atlstride2 = Cin * (W + 2 * pad_w)

        # N dim
        Atlbound3 = Cout // 8
        Atlstride3 = 0

        # M dim
        Atlbound4 = out_width // 8
        Atlstride4 = Cin * 8

        Atlbound5 = out_height
        Atlstride5 = Cin * (W + 2 * pad_w) * stride_h

        # Batch dim
        Atlbound6 = Nbatch
        Atlstride6 = Cin * (H + 2 * pad_h) * (W + 2 * pad_w)

    assert (
        Atlstride0 % 8 == 0
        and Atlstride1 % 8 == 0
        and Atlstride2 % 8 == 0
        and Atlstride3 % 8 == 0
        and Atlstride4 % 8 == 0
        and Atlstride5 % 8 == 0
        and Atlstride6 % 8 == 0
    )

    assert (
        M * K * N
        == Atlbound0
        * Atlbound1
        * Atlbound2
        * Atlbound3
        * Atlbound4
        * Atlbound5
        * Atlbound6
    )

    data_str += [
        format_scalar_definition("int32_t", "Aslstride0", Aslstride0),
        format_scalar_definition("int32_t", "Aslstride1", Aslstride1),
        format_scalar_definition("int32_t", "Atlbound0", Atlbound0),
        format_scalar_definition("int32_t", "Atlstride0", Atlstride0),
        format_scalar_definition("int32_t", "Atlbound1", Atlbound1),
        format_scalar_definition("int32_t", "Atlstride1", Atlstride1),
        format_scalar_definition("int32_t", "Atlbound2", Atlbound2),
        format_scalar_definition("int32_t", "Atlstride2", Atlstride2),
        format_scalar_definition("int32_t", "Atlbound3", Atlbound3),
        format_scalar_definition("int32_t", "Atlstride3", Atlstride3),
        format_scalar_definition("int32_t", "Atlbound4", Atlbound4),
        format_scalar_definition("int32_t", "Atlstride4", Atlstride4),
        format_scalar_definition("int32_t", "Atlbound5", Atlbound5),
        format_scalar_definition("int32_t", "Atlstride5", Atlstride5),
        format_scalar_definition("int32_t", "Atlbound6", Atlbound6),
        format_scalar_definition("int32_t", "Atlstride6", Atlstride6),
    ]

    if kwargs["ifC8HW8datalayout"] is True:
        # Cout8Cin8FyFx88
        # streamer setting for data mover B
        Bslstride0 = 1
        Bslstride1 = 8

        # K dim
        Btlbound0 = Kw * Kh * Cin8
        Btlstride0 = 8 * 8

        # N dim
        Btlbound1 = Cout // 8
        Btlstride1 = 8 * 8 * Kw * Kh * Cin8

        # M dim
        Btlbound2 = out_width * out_height // 8
        Btlstride2 = 0

        # Batch dim
        Btlbound3 = Nbatch
        Btlstride3 = 0
    else:
        # NCoutFyFxCin
        # streamer setting for data mover B
        Bslstride0 = 1
        Bslstride1 = Cin * Kw * Kh

        # K dim
        Btlbound0 = Cin * Kw * Kh // 8
        Btlstride0 = 8

        # N dim
        Btlbound1 = Cout // 8
        Btlstride1 = Cin * Kw * Kh * 8

        # M dim
        Btlbound2 = out_width * out_height // 8
        Btlstride2 = 0

        # Batch dim
        Btlbound3 = Nbatch
        Btlstride3 = 0

    assert (
        Btlstride0 % 64 == 0
        and Btlstride1 % 64 == 0
        and Btlstride2 % 64 == 0
        and Btlstride3 % 64 == 0
    )

    assert K * N * M == Btlbound0 * Btlbound1 * Btlbound2 * Btlbound3, (
        "K * N * M",
        K * N * M,
        "Loopbounds multipliers ",
        Btlbound0 * Btlbound1 * Btlbound2 * Btlbound3,
    )

    data_str += [
        format_scalar_definition("int32_t", "Bslstride0", Bslstride0),
        format_scalar_definition("int32_t", "Bslstride1", Bslstride1),
        format_scalar_definition("int32_t", "Btlbound0", Btlbound0),
        format_scalar_definition("int32_t", "Btlstride0", Btlstride0),
        format_scalar_definition("int32_t", "Btlbound1", Btlbound1),
        format_scalar_definition("int32_t", "Btlstride1", Btlstride1),
        format_scalar_definition("int32_t", "Btlbound2", Btlbound2),
        format_scalar_definition("int32_t", "Btlstride2", Btlstride2),
        format_scalar_definition("int32_t", "Btlbound3", Btlbound3),
        format_scalar_definition("int32_t", "Btlstride3", Btlstride3),
    ]

    # streamer setting for data mover C
    # C is int32_t so the stride is 4 times of the int8_t
    if kwargs["ifC8HW8datalayout"] is True:
        # NHWC
        Cslstride0 = 4
        Cslstride1 = 8

        # N dim
        Ctlbound0 = Cout // 8
        Ctlstride0 = out_height * out_width // 8 * 8 * 8 * 4

        # M dim
        # K is merged because of the block gemm output stationarity
        Ctlbound1 = out_width // 8
        Ctlstride1 = 8 * 8 * 4

        Ctlbound2 = out_height
        Ctlstride2 = out_width // 8 * 8 * 8 * 4

        # Batch dim
        Ctlbound3 = Nbatch
        Ctlstride3 = Cout * out_height * out_width * 4

    else:
        Cslstride0 = 4
        Cslstride1 = 8

        # N dim
        Ctlbound0 = Cout // 8
        Ctlstride0 = 8 * 4

        # M dim
        # K is merged because of the block gemm output stationarity
        Ctlbound1 = out_width // 8
        Ctlstride1 = Cout * 8 * 4

        Ctlbound2 = out_height
        Ctlstride2 = Cout * W * 4

        # Batch dim
        Ctlbound3 = Nbatch
        Ctlstride3 = Cout * H * W * 4

    assert (
        Ctlstride0 % 64 == 0
        and Ctlstride1 % 64 == 0
        and Ctlstride2 % 64 == 0
        and Ctlstride3 % 64 == 0
    )
    assert M * N == Ctlbound0 * Ctlbound1 * Ctlbound2 * Ctlbound3

    data_str += [
        format_scalar_definition("int32_t", "Cslstride0", Cslstride0),
        format_scalar_definition("int32_t", "Cslstride1", Cslstride1),
        format_scalar_definition("int32_t", "Ctlbound0", Ctlbound0),
        format_scalar_definition("int32_t", "Ctlstride0", Ctlstride0),
        format_scalar_definition("int32_t", "Ctlbound1", Ctlbound1),
        format_scalar_definition("int32_t", "Ctlstride1", Ctlstride1),
        format_scalar_definition("int32_t", "Ctlbound2", Ctlbound2),
        format_scalar_definition("int32_t", "Ctlstride2", Ctlstride2),
        format_scalar_definition("int32_t", "Ctlbound3", Ctlbound3),
        format_scalar_definition("int32_t", "Ctlstride3", Ctlstride3),
    ]

    if kwargs["ifC8HW8datalayout"] is True:
        D32slstride0 = 1 * 4
        D32slstride1 = 8

        # N dim
        D32tlbound0 = Cout // 8
        D32tlstride0 = out_height * out_width // 8 * 8 * 8 * 4

        # M dim
        # K is merged because of the block gemm output stationarity
        D32tlbound1 = out_width // 8
        D32tlstride1 = 8 * 8 * 4

        D32tlbound2 = out_height
        D32tlstride2 = out_width // 8 * 8 * 8 * 4

        # Batch dim
        D32tlbound3 = Nbatch
        D32tlstride3 = Cout * out_height * out_width * 4
    else:
        # D32 is int32_t so the stride is 4 times of the int8_t
        D32out = Cout
        D32slstride0 = 1 * 4
        D32slstride1 = 8

        # N dim
        D32tlbound0 = D32out // 8
        D32tlstride0 = 8 * 4

        # M dim
        # K is merged because of the block gemm output stationarity
        D32tlbound1 = out_width // 8
        D32tlstride1 = D32out * 8 * 4

        D32tlbound2 = out_height
        D32tlstride2 = D32out * out_width * 4

        # Batch dim
        D32tlbound3 = Nbatch
        D32tlstride3 = D32out * out_height * out_width * 4

    assert (
        D32tlstride0 % 64 == 0
        and D32tlstride1 % 64 == 0
        and D32tlstride2 % 64 == 0
        and D32tlstride3 % 64 == 0
    )

    data_str += [
        format_scalar_definition("int32_t", "D32slstride0", D32slstride0),
        format_scalar_definition("int32_t", "D32slstride1", D32slstride1),
        format_scalar_definition("int32_t", "D32tlbound0", D32tlbound0),
        format_scalar_definition("int32_t", "D32tlstride0", D32tlstride0),
        format_scalar_definition("int32_t", "D32tlbound1", D32tlbound1),
        format_scalar_definition("int32_t", "D32tlstride1", D32tlstride1),
        format_scalar_definition("int32_t", "D32tlbound2", D32tlbound2),
        format_scalar_definition("int32_t", "D32tlstride2", D32tlstride2),
        format_scalar_definition("int32_t", "D32tlbound3", D32tlbound3),
        format_scalar_definition("int32_t", "D32tlstride3", D32tlstride3),
    ]

    # postprocessing D8 settings
    if kwargs["ifC8HW8datalayout"] is True:
        D8slstride0 = 1
        D8slstride1 = 8

        # N dim
        D8tlbound0 = Cout // 8
        D8tlstride0 = out_height * out_width // 8 * 8 * 8

        # M dim
        # K is merged because of the block gemm output stationarity
        D8tlbound1 = out_width // 8
        D8tlstride1 = 8 * 8

        D8tlbound2 = out_height
        D8tlstride2 = out_width // 8 * 8 * 8

        # Batch dim
        D8tlbound3 = Nbatch
        D8tlstride3 = Cout * out_height * out_width
    else:
        D8out = Cout
        D8slstride0 = 1
        D8slstride1 = D8out

        # N dim
        D8tlbound0 = D8out // 8
        D8tlstride0 = 8

        # M dim
        # K is merged because of the block gemm output stationarity
        D8tlbound1 = out_width // 8
        D8tlstride1 = D8out * 8

        D8tlbound2 = out_height
        D8tlstride2 = D8out * out_width

        # Batch dim
        D8tlbound3 = Nbatch
        D8tlstride3 = D8out * out_height * out_width

    assert (
        D8tlstride0 % 64 == 0
        and D8tlstride1 % 64 == 0
        and D8tlstride2 % 64 == 0
        and D8tlstride3 % 64 == 0
    )
    data_str += [
        format_scalar_definition("int32_t", "D8slstride0", D8slstride0),
        format_scalar_definition("int32_t", "D8slstride1", D8slstride1),
        format_scalar_definition("int32_t", "D8tlbound0", D8tlbound0),
        format_scalar_definition("int32_t", "D8tlstride0", D8tlstride0),
        format_scalar_definition("int32_t", "D8tlbound1", D8tlbound1),
        format_scalar_definition("int32_t", "D8tlstride1", D8tlstride1),
        format_scalar_definition("int32_t", "D8tlbound2", D8tlbound2),
        format_scalar_definition("int32_t", "D8tlstride2", D8tlstride2),
        format_scalar_definition("int32_t", "D8tlbound3", D8tlbound3),
        format_scalar_definition("int32_t", "D8tlstride3", D8tlstride3),
    ]

    # Generating random 8 integer a and b for subtraction
    subtraction_a = 0
    subtraction_b = 0

    # Writing the subtraction value to data.h
    data_str += [
        format_scalar_definition("int8_t", "subtraction_a", subtraction_a),
        format_scalar_definition("int8_t", "subtraction_b", subtraction_b),
    ]

    # direct conv2d
    if kwargs["ifC8HW8datalayout"] is True:
        direct_conv2d_res = conv2d(
            input_data, kernel, stride=stride, padding=padding, mode="C8HW8"
        )
    else:
        direct_conv2d_res = conv2d(
            input_data, kernel, stride=stride, padding=padding, mode="NHWC"
        )

    # output in NHWC format
    direct_conv2d_res = np.add(direct_conv2d_res.reshape(-1), bias)

    # Writing testing data and golden data into data.h
    # implicit im2col matrix and kernel, store original input data and kernel
    data_str += [format_vector_definition("int8_t", "A", input_padding.reshape(-1))]
    data_str += [format_vector_definition("int8_t", "B", kernel.reshape(-1))]
    data_str += [format_vector_definition("int32_t", "C", bias.reshape(-1))]

    data_str += [format_scalar_definition("int32_t", "transposed_A", 0)]
    data_str += [format_scalar_definition("int32_t", "transposed_B", 0)]

    return data_str, direct_conv2d_res


def emit_matmul_data(**kwargs):

    meshRow = kwargs["meshRow"]
    tileSize = kwargs["tileSize"]
    meshCol = kwargs["meshCol"]

    # matmul settings
    data_str = []

    data_str += [format_scalar_definition("int", "Batch", 1)]
    data_str += [format_scalar_definition("int", "M", kwargs["M"])]
    data_str += [format_scalar_definition("int", "K", kwargs["K"])]
    data_str += [format_scalar_definition("int", "N", kwargs["N"])]

    data_str += [format_scalar_definition("int32_t", "Aslstride0", 1)]
    data_str += [format_scalar_definition("int32_t", "Aslstride1", bankWidth / 8)]
    data_str += [format_scalar_definition("int32_t", "Atlbound0", kwargs["K"])]
    data_str += [
        format_scalar_definition(
            "int32_t", "Atlstride0", input_data_width * tileSize * meshRow / 8
        )
    ]
    data_str += [format_scalar_definition("int32_t", "Atlbound1", kwargs["N"])]
    data_str += [format_scalar_definition("int32_t", "Atlstride1", 0)]
    data_str += [format_scalar_definition("int32_t", "Atlbound2", kwargs["M"])]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "Atlstride2",
            kwargs["K"] * input_data_width * tileSize * meshRow / 8,
        )
    ]
    data_str += [format_scalar_definition("int32_t", "Atlbound3", 1)]
    data_str += [format_scalar_definition("int32_t", "Atlstride3", 0)]
    data_str += [format_scalar_definition("int32_t", "Atlbound4", 1)]
    data_str += [format_scalar_definition("int32_t", "Atlstride4", 0)]
    data_str += [format_scalar_definition("int32_t", "Atlbound5", 1)]
    data_str += [format_scalar_definition("int32_t", "Atlstride5", 0)]

    data_str += [format_scalar_definition("int32_t", "Bslstride0", 1)]
    data_str += [format_scalar_definition("int32_t", "Bslstride1", bankWidth / 8)]
    data_str += [format_scalar_definition("int32_t", "Btlbound0", kwargs["K"])]
    data_str += [
        format_scalar_definition(
            "int32_t", "Btlstride0", input_data_width * tileSize * meshCol / 8
        )
    ]
    data_str += [format_scalar_definition("int32_t", "Btlbound1", kwargs["N"])]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "Btlstride1",
            kwargs["K"] * input_data_width * tileSize * meshCol / 8,
        )
    ]
    data_str += [format_scalar_definition("int32_t", "Btlbound2", kwargs["M"])]
    data_str += [format_scalar_definition("int32_t", "Btlstride2", 0)]

    data_str += [format_scalar_definition("int32_t", "Cslstride0", 4)]
    data_str += [format_scalar_definition("int32_t", "Cslstride1", bankWidth / 8)]
    data_str += [format_scalar_definition("int32_t", "Ctlbound0", kwargs["N"])]
    data_str += [
        format_scalar_definition(
            "int32_t", "Ctlstride0", output_data_width * meshRow * meshCol / 8
        )
    ]
    data_str += [format_scalar_definition("int32_t", "Ctlbound1", kwargs["M"])]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "Ctlstride1",
            kwargs["N"] * output_data_width * meshRow * meshCol / 8,
        )
    ]
    data_str += [format_scalar_definition("int32_t", "Ctlbound2", 1)]
    data_str += [format_scalar_definition("int32_t", "Ctlstride2", 0)]

    data_str += [format_scalar_definition("int32_t", "D32slstride0", 4)]
    data_str += [format_scalar_definition("int32_t", "D32slstride1", bankWidth / 8)]
    data_str += [format_scalar_definition("int32_t", "D32tlbound0", kwargs["N"])]
    data_str += [
        format_scalar_definition(
            "int32_t", "D32tlstride0", output_data_width * meshRow * meshCol / 8
        )
    ]
    data_str += [format_scalar_definition("int32_t", "D32tlbound1", kwargs["M"])]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "D32tlstride1",
            kwargs["N"] * output_data_width * meshRow * meshCol / 8,
        )
    ]
    data_str += [format_scalar_definition("int32_t", "D32tlbound2", 1)]
    data_str += [format_scalar_definition("int32_t", "D32tlstride2", 0)]

    data_str += [format_scalar_definition("int32_t", "D8slstride0", 1)]
    data_str += [format_scalar_definition("int32_t", "D8slstride1", bankWidth / 8)]
    data_str += [format_scalar_definition("int32_t", "D8tlbound0", kwargs["N"])]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "D8tlstride0",
            quantized_output_data_width * meshRow * meshCol / 8,
        )
    ]
    data_str += [format_scalar_definition("int32_t", "D8tlbound1", kwargs["M"])]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "D8tlstride1",
            kwargs["N"] * quantized_output_data_width * meshRow * meshCol / 8,
        )
    ]
    data_str += [format_scalar_definition("int32_t", "D8tlbound2", 1)]
    data_str += [format_scalar_definition("int32_t", "D8tlstride2", 0)]

    delta_local_a = 0
    delta_local_b = (
        kwargs["K"] * kwargs["M"] * (meshRow * tileSize * input_data_width / 8)
    )
    delta_local_c = delta_local_b + kwargs["K"] * kwargs["N"] * (
        meshCol * tileSize * input_data_width / 8
    )
    delta_local_d32 = delta_local_c + kwargs["M"] * kwargs["N"] * (
        meshRow * meshCol * output_data_width / 8
    )
    delta_local_d8 = delta_local_d32
    data_str += [format_scalar_definition("int32_t", "delta_local_a", delta_local_a)]
    data_str += [format_scalar_definition("int32_t", "delta_local_b", delta_local_b)]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "delta_local_c",
            delta_local_c,
        )
    ]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "delta_local_d32",
            delta_local_d32,
        )
    ]
    data_str += [
        format_scalar_definition(
            "int32_t",
            "delta_local_d8",
            delta_local_d8,
        )
    ]

    # Generating random 8 integer a and b for subtraction
    subtraction_a = np.random.randint(MIN, MAX)
    subtraction_b = np.random.randint(MIN, MAX)

    # Writing the subtraction value to data.h
    data_str += [format_scalar_definition("int8_t", "subtraction_a", subtraction_a)]
    data_str += [format_scalar_definition("int8_t", "subtraction_b", subtraction_b)]

    A = np.random.randint(
        MIN, MAX, size=(kwargs["M"], kwargs["K"], meshRow, tileSize)
    ).reshape(-1)
    data_str += [format_vector_definition("int8_t", "A", A)]

    B = np.random.randint(
        MIN, MAX, size=(kwargs["K"], kwargs["N"], tileSize, meshCol)
    ).reshape(-1)
    data_str += [format_vector_definition("int8_t", "B", B)]

    if kwargs["channel_en_C"] == 1:
        C = np.random.randint(
            MIN, MAX, size=(kwargs["M"], kwargs["N"], meshRow, meshCol)
        ).reshape(-1)
    else:
        C = np.random.randint(
            0, 1, size=(kwargs["M"], kwargs["N"], meshRow, meshCol)
        ).reshape(-1)
    if kwargs["channel_en_C"] == 1:
        data_str += [
            format_scalar_definition("int32_t", "channel_en_C", ((1 << 32) - 1))
        ]
    else:
        data_str += [format_scalar_definition("int32_t", "channel_en_C", 0)]
    data_str += [format_vector_definition("int32_t", "C", C)]

    if kwargs["transposed_A"] == 1:
        A = A.reshape(kwargs["M"], kwargs["K"], meshRow, tileSize)
        A = A.transpose(0, 1, 3, 2).reshape(-1)
    if kwargs["transposed_B"] == 1:
        B = B.reshape(kwargs["K"], kwargs["N"], tileSize, meshCol)
        B = B.transpose(0, 1, 3, 2).reshape(-1)

    data_str += [
        format_scalar_definition("int32_t", "transposed_A", kwargs["transposed_A"])
    ]
    data_str += [
        format_scalar_definition("int32_t", "transposed_B", kwargs["transposed_B"])
    ]

    D32 = block_gemm_golden_model(
        kwargs["M"],
        kwargs["K"],
        kwargs["N"],
        meshRow,
        tileSize,
        meshCol,
        A,
        B,
        subtraction_a,
        subtraction_b,
        C,
    )

    return data_str, D32


def emit_gemmx_data(**kwargs):

    ifTestMatmul = kwargs["ifTestMatmul"]

    if ifTestMatmul == 1:
        data_str, D32 = emit_matmul_data(**kwargs)
        data_str += ["#define TEST_MATMUL"]
    else:
        data_str, D32 = emit_conv_data(**kwargs)

    data_str += [format_vector_definition("int32_t", "D32", D32)]

    # -----------------------------------------------------------
    # Postprocessing
    # -----------------------------------------------------------

    bypassSIMD = kwargs["bypassSIMD"]
    data_str += [format_scalar_definition("int32_t", "bypassSIMD", bypassSIMD)]

    # Generating random constant values
    group_num = 8
    input_zp_i = np.random.randint(MIN, MAX)
    output_zp_i = np.random.randint(MIN, MAX)
    max_int_i = MAX
    min_int_i = MIN
    double_round_i = np.random.randint(0, 1)

    shift_i = np.random.randint(0, 63, size=group_num)  # values between 0-63
    multiplier_i = np.random.randint(-(2**31), 2**31 - 1, size=group_num)

    # Writing the constant values to data.h
    data_str += [
        format_scalar_definition("int8_t", "input_zp_i", input_zp_i),
        format_scalar_definition("int8_t", "output_zp_i", output_zp_i),
        format_scalar_definition("int8_t", "max_int_i", max_int_i),
        format_scalar_definition("int8_t", "min_int_i", min_int_i),
        format_scalar_definition("int8_t", "double_round_i", double_round_i),
    ]

    shared_bitpacked_shift0 = (
        (shift_i[3] << 24) | (shift_i[2] << 16) | (shift_i[1] << 8) | shift_i[0]
    )
    shared_bitpacked_shift1 = (
        (shift_i[7] << 24) | (shift_i[6] << 16) | (shift_i[5] << 8) | shift_i[4]
    )
    data_str += [
        format_scalar_definition(
            "int32_t", "shared_bitpacked_shift0", shared_bitpacked_shift0
        )
    ]
    data_str += [
        format_scalar_definition(
            "int32_t", "shared_bitpacked_shift1", shared_bitpacked_shift1
        )
    ]

    data_str += [
        format_scalar_definition("int32_t", "shared_multiplier0", multiplier_i[0])
    ]
    data_str += [
        format_scalar_definition("int32_t", "shared_multiplier1", multiplier_i[1])
    ]
    data_str += [
        format_scalar_definition("int32_t", "shared_multiplier2", multiplier_i[2])
    ]
    data_str += [
        format_scalar_definition("int32_t", "shared_multiplier3", multiplier_i[3])
    ]
    data_str += [
        format_scalar_definition("int32_t", "shared_multiplier4", multiplier_i[4])
    ]
    data_str += [
        format_scalar_definition("int32_t", "shared_multiplier5", multiplier_i[5])
    ]
    data_str += [
        format_scalar_definition("int32_t", "shared_multiplier6", multiplier_i[6])
    ]
    data_str += [
        format_scalar_definition("int32_t", "shared_multiplier7", multiplier_i[7])
    ]

    D8 = np.zeros_like(D32, dtype=np.uint8)
    # output channel (innermost dim) has a different scale factor
    for i in range(group_num):
        D8[i::group_num] = postprocessing_simd_golden_model(
            D32[i::group_num],
            input_zp_i,
            output_zp_i,
            shift_i[i],
            max_int_i,
            min_int_i,
            double_round_i,
            multiplier_i[i],
        )

    data_str += [format_vector_definition("int8_t", "D8", D8)]

    data_str = "\n\n".join(data_str)

    return data_str


def test():
    np.set_printoptions(threshold=np.inf)

    # conv2d settings
    Nbatch, H, W, Cin = (1, 24, 24, 40)
    Cout, Kh, Kw, Cin = (8, 3, 3, 40)

    stride = (1, 1)
    padding = (1, 1)

    # test data generation
    input_data = np.random.randint(-10, 10, size=(Nbatch, H, W, Cin))
    kernel = np.random.randint(-10, 10, size=(Cout, Kh, Kw, Cin))

    im2col_matrix, im2col_kernel = im2col(
        input_data, kernel, stride=stride, padding=padding
    )

    M = im2col_matrix.shape[0] // 8
    K = im2col_matrix.shape[1] // 8
    N = im2col_kernel.shape[1] // 8

    # conv2d using im2col
    im2col_matrix, im2col_kernel = im2col(
        input_data, kernel, stride=stride, padding=padding
    )
    im2col_matrix = data_reshuffler_golden_model(
        K, M, 8, 8, 8, 8 * 8 * K, 1, 8 * K, im2col_matrix.reshape(-1)
    )
    im2col_kernel = data_reshuffler_golden_model(
        K, N, 8, 8, 8, 8 * 8 * K, 1, 8 * K, im2col_kernel.T.reshape(-1)
    )
    im2col_conv2d_res = block_gemm_golden_model(
        M, K, N, 8, 8, 8, im2col_matrix, im2col_kernel, 0, 0
    )

    # direct conv2d
    direct_conv2d_res = conv2d(input_data, kernel, stride=stride, padding=padding)
    direct_conv2d_res = direct_conv2d_res.reshape(-1)
    direct_conv2d_res = data_reshuffler_golden_model(
        N, M, 8, 8, 8, 8 * 8 * N, 1, 8 * N, direct_conv2d_res, 1
    )

    # result comparison
    assert (im2col_conv2d_res == direct_conv2d_res).all()


def main():
    # Parsing cmd args
    parser = argparse.ArgumentParser(description="Generate data for kernels")
    parser.add_argument(
        "-c",
        "--cfg",
        type=pathlib.Path,
        required=True,
        help="Select param config file kernel",
    )
    args = parser.parse_args()

    # Load param config file
    with args.cfg.open() as f:
        param = hjson.loads(f.read())

    # Emit header file
    print(emit_header_file(**param))


if __name__ == "__main__":

    # for testing
    # test()

    main()
