// Copyright 2024 KU Leuven.
// Solderpad Hardware License, Version 0.51, see LICENSE for details.
// SPDX-License-Identifier: SHL-0.51

<%
  num_stream2acc_ports = len(cfg["snax_streamer_cfg"]["data_reader_params"]["tcdm_ports_num"])
  num_acc2stream_ports = len(cfg["snax_streamer_cfg"]["data_writer_params"]["tcdm_ports_num"])
  num_tcdm_ports = sum(cfg["snax_streamer_cfg"]["data_reader_params"]["tcdm_ports_num"]) + \
                   sum(cfg["snax_streamer_cfg"]["data_writer_params"]["tcdm_ports_num"])

  # We make the assumption that all reader and writers
  # Have the same data widths
  stream_data_width = cfg["snax_streamer_cfg"]["fifo_reader_params"]["fifo_width"][0]
%>
//-----------------------------
// Streamer wrapper
//-----------------------------
module ${cfg["tag_name"]}_streamer_wrapper #(
  // These parameters are generated by the
  // template generation. Manually modifying this,
  // might cause mismatch in parameters. Change
  // these at your own risk!

  // Parameters related to TCDM
  parameter int unsigned TCDMDataWidth      = ${cfg["tcdm_data_width"]},
  parameter int unsigned TCDMReqPorts       = ${num_tcdm_ports},
  parameter int unsigned TCDMAddrWidth      = ${cfg["tcdm_addr_width"]},
  
  // Parameters related to streamers
  parameter int unsigned NumStream2AccPorts = ${num_stream2acc_ports},
  parameter int unsigned NumAcc2StreamPorts = ${num_acc2stream_ports},
  parameter int unsigned StreamerDataWidth  = ${stream_data_width}
)(
  //-----------------------------
  // Clocks and reset
  //-----------------------------
  input  logic clk_i,
  input  logic rst_ni,

  //-----------------------------
  // Accelerator ports
  //-----------------------------
 
  // Ports from accelerator to streamer
  output logic [NumStream2AccPorts-1:0][StreamerDataWidth-1:0] stream2acc_data_o,
  output logic [NumStream2AccPorts-1:0]                        stream2acc_valid_o,
  input  logic [NumStream2AccPorts-1:0]                        stream2acc_ready_i,

  // Ports from accelerator to streamer
  input  logic [NumAcc2StreamPorts-1:0][StreamerDataWidth-1:0] acc2stream_data_i,
  input  logic [NumAcc2StreamPorts-1:0]                        acc2stream_valid_i,
  output logic [NumAcc2StreamPorts-1:0]                        acc2stream_ready_o,

  //-----------------------------
  // TCDM ports
  //-----------------------------
  // Request
  output logic [TCDMReqPorts-1:0]                      tcdm_req_write_o,
  output logic [TCDMReqPorts-1:0][  TCDMAddrWidth-1:0] tcdm_req_addr_o,
  //Note that tcdm_req_amo_i is 4 bits based on reqrsp definition
  output logic [TCDMReqPorts-1:0][                3:0] tcdm_req_amo_o, 
  output logic [TCDMReqPorts-1:0][  TCDMDataWidth-1:0] tcdm_req_data_o,
  //Note that tcdm_req_user_core_id_i is 5 bits based on Snitch definition
  output logic [TCDMReqPorts-1:0][                4:0] tcdm_req_user_core_id_o,
  output logic [TCDMReqPorts-1:0]                      tcdm_req_user_is_core_o,
  output logic [TCDMReqPorts-1:0][TCDMDataWidth/8-1:0] tcdm_req_strb_o,
  output logic [TCDMReqPorts-1:0]                      tcdm_req_q_valid_o,

  // Response
  input  logic [TCDMReqPorts-1:0]                      tcdm_rsp_q_ready_i,
  input  logic [TCDMReqPorts-1:0]                      tcdm_rsp_p_valid_i,
  input  logic [TCDMReqPorts-1:0][  TCDMDataWidth-1:0] tcdm_rsp_data_i,

  //-----------------------------
  // CSR control ports
  //-----------------------------
  // Request
  input  logic [31:0] csr_req_bits_data_i,
  input  logic [31:0] csr_req_bits_addr_i,
  input  logic        csr_req_bits_write_i,
  input  logic        csr_req_valid_i,
  output logic        csr_req_ready_o,

  // Response
  input  logic        csr_rsp_ready_i,
  output logic        csr_rsp_valid_o,
  output logic [31:0] csr_rsp_bits_data_o
);

  //-----------------------------
  // Wiring and combinational logic
  //-----------------------------

  // Fixed ports that are defaulted
  // towards the TCDM from the streamer
  always_comb begin
    for(int i = 0; i < TCDMReqPorts; i++ ) begin
      tcdm_req_amo_o          [i] = '0;
      tcdm_req_user_core_id_o [i] = '0;
      tcdm_req_user_is_core_o [i] = '0;
      tcdm_req_strb_o         [i] = '1;
    end
  end

  // Streamer module that is generated
  // with template mechanics
  StreamerTop i_streamer_top (	
    //-----------------------------
    // Clocks and reset
    //-----------------------------
    .clock ( clk_i   ),
    .reset ( ~rst_ni ),

    //-----------------------------
    // Accelerator ports
    //-----------------------------
% for idx in range(len(cfg["snax_streamer_cfg"]["fifo_writer_params"]["fifo_width"])):
    .io_data_accelerator2streamer_data_${idx}_bits  (  acc2stream_data_i [${idx}] ),
    .io_data_accelerator2streamer_data_${idx}_valid ( acc2stream_valid_i [${idx}] ),
    .io_data_accelerator2streamer_data_${idx}_ready ( acc2stream_ready_o [${idx}] ),

% endfor
% for idx in range(len(cfg["snax_streamer_cfg"]["fifo_reader_params"]["fifo_width"])):
    .io_data_streamer2accelerator_data_${idx}_bits  (  stream2acc_data_o [${idx}] ),
    .io_data_streamer2accelerator_data_${idx}_valid ( stream2acc_valid_o [${idx}] ),
    .io_data_streamer2accelerator_data_${idx}_ready ( stream2acc_ready_i [${idx}] ),

% endfor
    //-----------------------------
    // TCDM Ports
    //-----------------------------
    // Request
% for idx in range(0, sum(cfg["snax_streamer_cfg"]["data_reader_params"]["tcdm_ports_num"]) + sum(cfg["snax_streamer_cfg"]["data_writer_params"]["tcdm_ports_num"])):
    .io_data_tcdm_rsp_${idx}_bits_data  ( tcdm_rsp_data_i[${idx}]    ),
    .io_data_tcdm_rsp_${idx}_valid      ( tcdm_rsp_p_valid_i[${idx}] ),
    .io_data_tcdm_req_${idx}_ready      ( tcdm_rsp_q_ready_i[${idx}] ),

% endfor
    // Response
% for idx in range(0, sum(cfg["snax_streamer_cfg"]["data_reader_params"]["tcdm_ports_num"]) + sum(cfg["snax_streamer_cfg"]["data_writer_params"]["tcdm_ports_num"])):
    .io_data_tcdm_req_${idx}_valid      ( tcdm_req_q_valid_o[${idx}] ),
    .io_data_tcdm_req_${idx}_bits_addr  ( tcdm_req_addr_o[${idx}]    ),
    .io_data_tcdm_req_${idx}_bits_write ( tcdm_req_write_o[${idx}]   ),
    .io_data_tcdm_req_${idx}_bits_data  ( tcdm_req_data_o[${idx}]    ),

% endfor
    //-----------------------------
    // CSR control ports
    //-----------------------------
    // Request
    .io_csr_req_bits_data  ( csr_req_bits_data_i  ),
    .io_csr_req_bits_addr  ( csr_req_bits_addr_i  ),
    .io_csr_req_bits_write ( csr_req_bits_write_i ),
    .io_csr_req_valid      ( csr_req_valid_i      ),
    .io_csr_req_ready      ( csr_req_ready_o      ),

    // Response
    .io_csr_rsp_bits_data  ( csr_rsp_bits_data_o  ),	
    .io_csr_rsp_valid      ( csr_rsp_valid_o      ),
    .io_csr_rsp_ready      ( csr_rsp_ready_i      )
  );

endmodule