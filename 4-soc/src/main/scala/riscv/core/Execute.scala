// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util._
import riscv.core.ALU
import riscv.core.ALUControl
import riscv.Parameters


class Execute extends Module {
  val io = IO(new Bundle {
    val instruction         = Input(UInt(Parameters.InstructionWidth))
    val instruction_address = Input(UInt(Parameters.AddrWidth))
    val reg1_data           = Input(UInt(Parameters.DataWidth))
    val reg2_data           = Input(UInt(Parameters.DataWidth))
    val immediate           = Input(UInt(Parameters.DataWidth))
    val aluop1_source       = Input(UInt(1.W))
    val aluop2_source       = Input(UInt(1.W))
    val csr_read_data       = Input(UInt(Parameters.DataWidth))
    val forward_from_mem    = Input(UInt(Parameters.DataWidth))
    val forward_from_wb     = Input(UInt(Parameters.DataWidth))
    val reg1_forward        = Input(UInt(2.W))
    val reg2_forward        = Input(UInt(2.W))
    val alu_bnrv            = Input(UInt(1.W))
    val mem_stall_input     = Input(Bool())

    val mem_alu_result = Output(UInt(Parameters.DataWidth))
    val mem_reg2_data  = Output(UInt(Parameters.DataWidth))
    val csr_write_data = Output(UInt(Parameters.DataWidth))
    val bnrv_done      = Output(Bool())
  })

  val opcode = io.instruction(6, 0)
  val funct3 = io.instruction(14, 12)
  val funct7 = io.instruction(31, 25)
  val uimm   = io.instruction(19, 15)

  val alu      = Module(new ALU)
  val alu_ctrl = Module(new ALUControl)

  alu_ctrl.io.opcode := opcode
  alu_ctrl.io.funct3 := funct3
  alu_ctrl.io.funct7 := funct7
  alu.io.func        := alu_ctrl.io.alu_funct

  val reg1_data = MuxLookup(
    io.reg1_forward,
    io.reg1_data
  )(
    IndexedSeq(
      ForwardingType.ForwardFromMEM -> io.forward_from_mem,
      ForwardingType.ForwardFromWB  -> io.forward_from_wb
    )
  )

  alu.io.op1 := Mux(
    io.alu_bnrv.asBool,
    0.U,
    Mux(
      io.aluop1_source === ALUOp1Source.InstructionAddress,
      io.instruction_address,
      reg1_data
    )
  )

  val reg2_data = MuxLookup(
    io.reg2_forward,
    io.reg2_data
  )(
    IndexedSeq(
      ForwardingType.ForwardFromMEM -> io.forward_from_mem,
      ForwardingType.ForwardFromWB  -> io.forward_from_wb
    )
  )
  alu.io.op2 := Mux(
    io.alu_bnrv.asBool,
    0.U,
    Mux(
      io.aluop2_source === ALUOp2Source.Immediate,
      io.immediate,
      reg2_data
    )
  )

  val bitnet_accel = Module(new SimpleBitNetAccel())
  bitnet_accel.io.rs1_data := reg1_data
  bitnet_accel.io.rs2_data := reg2_data
  bitnet_accel.io.alu_bnrv := io.alu_bnrv
  bitnet_accel.io.funct7   := io.instruction(31, 25)

  bitnet_accel.io.axi4_channels.read_address_channel.ARADDR  := 0.U
  bitnet_accel.io.axi4_channels.read_address_channel.ARVALID := false.B
  bitnet_accel.io.axi4_channels.read_address_channel.ARPROT  := 0.U
  bitnet_accel.io.axi4_channels.read_data_channel.RREADY     := false.B

  bitnet_accel.io.axi4_channels.write_address_channel.AWADDR  := 0.U
  bitnet_accel.io.axi4_channels.write_address_channel.AWVALID := false.B
  bitnet_accel.io.axi4_channels.write_address_channel.AWPROT  := 0.U
  bitnet_accel.io.axi4_channels.write_data_channel.WDATA      := 0.U
  bitnet_accel.io.axi4_channels.write_data_channel.WSTRB      := 0.U
  bitnet_accel.io.axi4_channels.write_data_channel.WVALID     := false.B
  bitnet_accel.io.axi4_channels.write_response_channel.BREADY := false.B

  val bnrv_result = bitnet_accel.io.bitnet_result
  io.bnrv_done := bitnet_accel.io.accel_done

  // Use alu_bnrv to select BNRV result (received from AXI4 Lite) 
  io.mem_alu_result := MuxLookup(
    io.alu_bnrv,
    alu.io.result
  )(
    IndexedSeq(
      ALUBnrvType.Bnrv      -> bnrv_result,
      ALUBnrvType.NoBnrv    -> alu.io.result
    )
  )

  io.mem_reg2_data  := reg2_data
  io.csr_write_data := MuxLookup(
    funct3,
    0.U
  )(
    IndexedSeq(
      InstructionsTypeCSR.csrrw  -> reg1_data,
      InstructionsTypeCSR.csrrc  -> io.csr_read_data.&((~reg1_data).asUInt),
      InstructionsTypeCSR.csrrs  -> io.csr_read_data.|(reg1_data),
      InstructionsTypeCSR.csrrwi -> Cat(0.U(27.W), uimm),
      InstructionsTypeCSR.csrrci -> io.csr_read_data.&((~Cat(0.U(27.W), uimm)).asUInt),
      InstructionsTypeCSR.csrrsi -> io.csr_read_data.|(Cat(0.U(27.W), uimm)),
    )
  )

  // For Debugging
  val debug_counter = RegInit(0.U(32.W))
  debug_counter := debug_counter + 1.U

  when(io.alu_bnrv.asBool) {
    printf(p"Time: ${debug_counter} | [EX] Select: ${io.alu_bnrv} | BitNet_In: ${bnrv_result} | ALU_In: ${alu.io.result} | -> MUX_Out: ${io.mem_alu_result}\n")
    printf(p"    [EX-Fwd] Fwd1_Sel: ${io.reg1_forward} (0:No, 1:WB, 2:MEM) | Reg1_In: ${io.reg1_data} | Fwd_MEM: ${io.forward_from_mem} | Fwd_WB: ${io.forward_from_wb} | -> Reg1_Final: ${reg1_data}\n")
    printf(p"    [EX-Fwd] Fwd2_Sel: ${io.reg2_forward} (0:No, 1:WB, 2:MEM) | Reg2_In: ${io.reg2_data} | -> Reg2_Final: ${reg2_data}\n")
  }
}
