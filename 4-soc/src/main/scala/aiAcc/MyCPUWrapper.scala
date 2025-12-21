package aiAcc

import chisel3._
import chisel3.util._ 
import riscv.core.CPU
import riscv.ImplementationType
import riscv.Parameters

class MyCPUWrapper extends Module {
    val io = IO(new Bundle {
        val bus = Flipped(new SimpleRegIO())
        val irq_external = Input(Bool())
        val trap = Output(Bool())
    })

    val cpu = Module(new CPU(implementation = ImplementationType.FiveStageFinal))

    val dmem_write = cpu.io.memory_bundle.write_enable
    val dmem_addr = cpu.io.memory_bundle.address
    val dmem_wdata = cpu.io.memory_bundle.write_data

    val dmem_access_req = dmem_write || (dmem_addr =/= 0.U)
    
    io.bus.valid := true.B 
    io.bus.addr := cpu.io.instruction_address
    io.bus.wdata := 0.U
    io.bus.wen := false.B 
    io.bus.ren := true.B 

    cpu.io.instruction := io.bus.rdata
    cpu.io.instruction_valid := io.bus.ready && !dmem_access_req

    cpu.io.memory_bundle.read_data := 0.U

    when(dmem_access_req) {
        io.bus.addr := dmem_addr
        io.bus.wdata := dmem_wdata
        io.bus.wen := dmem_write
        io.bus.ren := !dmem_write
    }

    cpu.io.memory_bundle.read_data := io.bus.rdata

    cpu.io.instruction_valid := io.bus.ready && !dmem_access_req
    cpu.io.interrupt_flag := io.irq_external
    io.trap := false.B

    cpu.io.debug_read_address := 0.U
    cpu.io.csr_debug_read_address := 0.U

}