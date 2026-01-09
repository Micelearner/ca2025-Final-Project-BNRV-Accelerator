/* * This file is used to test the stall and forwarding logic for BNRV instructions.
   * The "SimpleBitNetAccel" module here is a dummy module. It identifies 3 BNRV 
   * instructions, stalls for a few cycles to simulate latency, and performs simple 
   * arithmetic. The actual BitNet operation is implemented by partners.
*/

package riscv.core

import chisel3._
import chisel3.util._
import bus.AXI4LiteChannels
import riscv.Parameters

class SimpleBitNetAccel extends Module {
  val io = IO(new Bundle {
    val axi4_channels = Flipped(new AXI4LiteChannels(Parameters.AddrBits, Parameters.DataBits))
    val irq = Output(Bool())

    // Pipeline Interface
    val funct7 = Input(UInt(7.W))
    val rs1_data = Input(UInt(32.W))
    val rs2_data = Input(UInt(32.W))
    val bitnet_result = Output(UInt(32.W))
    val alu_bnrv = Input(UInt(1.W))
    
    // Handshake Signals
    val stall = Input(Bool())
    val accel_done = Output(Bool())
    val busy = Output(Bool())
  })

  io.axi4_channels.read_data_channel.RVALID := false.B
  io.axi4_channels.read_data_channel.RDATA := 0.U
  io.axi4_channels.read_data_channel.RRESP := 0.U
  io.axi4_channels.write_response_channel.BVALID := false.B
  io.axi4_channels.write_response_channel.BRESP := 0.U
  io.axi4_channels.read_address_channel.ARREADY := true.B
  io.axi4_channels.write_address_channel.AWREADY := true.B
  io.axi4_channels.write_data_channel.WREADY := true.B
  io.irq := false.B

  val sIdle :: sCompute :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val counter = RegInit(0.U(3.W)) 
  val result_reg = RegInit(0.U(32.W))

  io.busy := (state =/= sIdle)
  io.accel_done := (state === sDone)
  io.bitnet_result := result_reg

  switch(state) {
    is(sIdle) {
      when(io.alu_bnrv.asBool) {
        state := sCompute
        counter := 5.U
        
        switch(io.funct7) {
          is(0.U) { // bnsum4
            result_reg := io.rs1_data + io.rs2_data
          }
          is(1.U) { // bnstore
            result_reg := io.rs2_data
          }
          is(2.U) { // bnsum8
            result_reg := io.rs1_data + io.rs2_data + 8.U
          }
        }
      }
    }
    is(sCompute) {
      when(counter === 0.U) {
        state := sDone
      } .otherwise {
        counter := counter - 1.U
      }
    }
    is(sDone) {
      when(!io.stall) {
        state := sIdle
      }
    }
  }

  // For Debugging
  val cycle_count = RegInit(0.U(32.W))
  cycle_count := cycle_count + 1.U

  when(io.alu_bnrv.asBool || state =/= sIdle) {
    printf(p"Time: ${cycle_count} | State: ${state} | F7: ${io.funct7} | ResReg: ${result_reg} | Done: ${io.accel_done} | Stall: ${io.stall}\n")
  }
}