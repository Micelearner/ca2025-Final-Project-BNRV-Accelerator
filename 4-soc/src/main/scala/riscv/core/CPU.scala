// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import bus.AXI4LiteMaster
import chisel3._
import riscv.ImplementationType
import riscv.Parameters
// PipelinedCPU is now in the same package (riscv.core)

import chisel3.util._
// for FSM


class CPU(val implementation: Int = ImplementationType.FiveStageFinal) extends Module {
  val io = IO(new CPUBundle)

  implementation match {
    case ImplementationType.FiveStageFinal =>
      val cpu = Module(new PipelinedCPU)

      // Connect instruction fetch interface
      io.instruction_address   := cpu.io.instruction_address
      cpu.io.instruction       := io.instruction
      cpu.io.instruction_valid := io.instruction_valid

      // Connect memory/bus interface through AXI4-Lite master
      val axi_master = Module(new AXI4LiteMaster(Parameters.AddrBits, Parameters.DataBits))

      // Reconstruct full address from PipelinedCPU outputs
      val full_bus_address = cpu.io.device_select ## cpu.io.memory_bundle
        .address(Parameters.AddrBits - Parameters.SlaveDeviceCountBits - 1, 0)
      
      val BN_STATUS_ADDR = 0x60000004.U 
      val BN_STATUS_DONE = 2.U

      // FSM
      val sBnIdle :: sBnReadReq :: sBnReadWait :: sBnDone :: Nil = Enum(4)
      val bn_state = RegInit(sBnIdle)

      val alu_bnrv_to_accel = Wire(UInt(1.W))
      alu_bnrv_to_accel := Mux(bn_state === sBnDone, 0.U, cpu.io.alu_bnrv)
      
      // BitNet insturction asserts  
      val is_bn_instr = alu_bnrv_to_accel.asBool

      val bn_hijack_bus = WireInit(false.B)
      val bn_req_read   = WireInit(false.B)
      val bn_req_addr   = WireInit(0.U(Parameters.AddrBits.W))

      // Polling FSM
      switch(bn_state) {
        is(sBnIdle) {
          // When receiving BitNet insturction go to ReadReq State
          when(is_bn_instr) {
            bn_state := sBnReadReq
          }
        }
        
        is(sBnReadReq) {
          
          bn_hijack_bus := true.B
          bn_req_read   := true.B
          bn_req_addr   := BN_STATUS_ADDR
          
          when(axi_master.io.bundle.busy) {
            bn_state := sBnReadWait
          }
        }
        
        is(sBnReadWait) {
          bn_hijack_bus := true.B
          
          // Wait for Read Valid
          when(axi_master.io.bundle.read_valid) {
            when(axi_master.io.bundle.read_data === BN_STATUS_DONE) {
              // Done
              bn_state := sBnDone
            }.otherwise {
              // If status != done, go back to ReadReq State 
              bn_state := sBnReadReq
            }
          }
        }
        
        is(sBnDone) {
          // keep Done state til alu_bnrv low
          when(!is_bn_instr) {
            bn_state := sBnIdle
          }
        }
      }

      // from PipelinedCPU to Top
      io.alu_bnrv := alu_bnrv_to_accel
      io.rs1_data := cpu.io.rs1_data
      io.rs2_data := cpu.io.rs2_data
      io.funct7   := cpu.io.funct7

      // from Top to PipelinedCPU
      cpu.io.bnrv_result_input := io.bnrv_result_input


      // BusBundle to AXI4LiteMasterBundle adapter
      axi_master.io.bundle.address      := Mux(bn_hijack_bus, bn_req_addr, full_bus_address)
      axi_master.io.bundle.read         := Mux(bn_hijack_bus, bn_req_read, cpu.io.memory_bundle.request && cpu.io.memory_bundle.read)
      axi_master.io.bundle.write        := Mux(bn_hijack_bus, false.B,     cpu.io.memory_bundle.request && cpu.io.memory_bundle.write)
      axi_master.io.bundle.write_data   := cpu.io.memory_bundle.write_data
      axi_master.io.bundle.write_strobe := cpu.io.memory_bundle.write_strobe

      val polling_busy = is_bn_instr && (bn_state =/= sBnDone)
      cpu.io.memory_bundle.busy                := axi_master.io.bundle.busy || polling_busy
      cpu.io.memory_bundle.granted             := !axi_master.io.bundle.busy  && !polling_busy  // Granted when not busy

      cpu.io.accel_busy_input := polling_busy

      val cpu_visible = !polling_busy
      cpu.io.memory_bundle.read_data           := Mux(cpu_visible, axi_master.io.bundle.read_data, 0.U)
      cpu.io.memory_bundle.read_valid          := Mux(cpu_visible, axi_master.io.bundle.read_valid, false.B)
      cpu.io.memory_bundle.write_valid         := Mux(cpu_visible, axi_master.io.bundle.write_valid, false.B)
      cpu.io.memory_bundle.write_data_accepted := Mux(cpu_visible, axi_master.io.bundle.write_data_accepted, false.B)

      // Connect AXI4-Lite channels to top-level
      io.axi4_channels <> axi_master.io.channels

      // Initialize cpu's unused axi4_channels inputs (FiveStageCPUFinal doesn't use these)
      cpu.io.axi4_channels.read_address_channel.ARREADY  := false.B
      cpu.io.axi4_channels.read_data_channel.RVALID      := false.B
      cpu.io.axi4_channels.read_data_channel.RDATA       := 0.U
      cpu.io.axi4_channels.read_data_channel.RRESP       := 0.U
      cpu.io.axi4_channels.write_address_channel.AWREADY := false.B
      cpu.io.axi4_channels.write_data_channel.WREADY     := false.B
      cpu.io.axi4_channels.write_response_channel.BVALID := false.B
      cpu.io.axi4_channels.write_response_channel.BRESP  := 0.U

      // Connect device select and bus address from wrapper
      io.device_select := cpu.io.device_select

      // Latch bus address for the duration of each AXI transaction.
      // AXI4LiteMaster captures cpu.io.memory_bundle.address in the Idle state and
      // asserts ARVALID/AWVALID on the next cycle. If the CPU pipeline advances in
      // between, the combinational cpu.io.memory_bundle.address/device_select would
      // change and the BusSwitch could route the master's ARVALID/AWVALID to the
      // wrong slave. By registering bus_address when a new request starts and
      // holding it while the master is busy, we ensure stable routing.
      val bus_address_reg  = RegInit(0.U(Parameters.AddrWidth))
      val next_bus_address = full_bus_address

      // New transaction starts when master is idle (not busy) and CPU issues a
      // read or write request on the BusBundle.
      val start_bus_transaction =
        !axi_master.io.bundle.busy &&
          cpu.io.memory_bundle.request &&
          (cpu.io.memory_bundle.read || cpu.io.memory_bundle.write)

      when(start_bus_transaction) {
        bus_address_reg := next_bus_address
      }

      io.bus_address := Mux(bn_hijack_bus, bn_req_addr, bus_address_reg)

      // Connect wrapper memory_bundle outputs (pass through from CPU)
      io.memory_bundle.address      := cpu.io.memory_bundle.address
      io.memory_bundle.read         := cpu.io.memory_bundle.read
      io.memory_bundle.write        := cpu.io.memory_bundle.write
      io.memory_bundle.write_data   := cpu.io.memory_bundle.write_data
      io.memory_bundle.write_strobe := cpu.io.memory_bundle.write_strobe
      io.memory_bundle.request      := cpu.io.memory_bundle.request

      // Note: io.memory_bundle inputs (read_data, read_valid, write_valid, busy, granted)
      // are not connected at Top level - they're for debugging/bypass only
      // The actual memory interface goes through axi4_channels

      // Connect interrupt
      cpu.io.interrupt_flag := io.interrupt_flag

      // Connect debug interfaces
      cpu.io.debug_read_address := io.debug_read_address
      io.debug_read_data        := cpu.io.debug_read_data

      cpu.io.csr_debug_read_address := io.csr_debug_read_address
      io.csr_debug_read_data        := cpu.io.csr_debug_read_data

      // Connect debug bus signals
      io.debug_bus_write_enable := cpu.io.memory_bundle.write
      io.debug_bus_write_data   := cpu.io.memory_bundle.write_data
  }
}
