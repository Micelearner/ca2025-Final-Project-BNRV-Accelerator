/* * This file is used to test the stall and forwarding logic for BNRV instructions.
   * The "SimpleBitNetAccel" module here is a dummy module. It identifies 3 BNRV 
   * instructions, stalls for a few cycles to simulate latency, and performs simple 
   * arithmetic. The actual BitNet operation is implemented by partners.
*/

package riscv.core

import chisel3._
import chisel3.util._
import bus._
import bus.AXI4LiteChannels
import riscv.Parameters

class SimpleBitNetAccel extends Module {
  val io = IO(new Bundle {
    val axi4_channels = Flipped(new AXI4LiteChannels(Parameters.AddrBits, Parameters.DataBits))
    val irq = Output(Bool())

    val funct7 = Input(UInt(7.W))
    val rs1_data = Input(UInt(32.W))
    val rs2_data = Input(UInt(32.W))
    val bitnet_result = Output(UInt(32.W))
    val alu_bnrv = Input(UInt(1.W))

    val accel_done = Output(Bool())
    val busy = Output(Bool())
  })

  // channels
  val axi_slave = Module(new AXI4LiteSlave(Parameters.AddrBits, Parameters.DataBits))
  axi_slave.io.channels <> io.axi4_channels
  
  // bundle
  val regAddr   = axi_slave.io.bundle.address(7, 0)
  val isRead    = axi_slave.io.bundle.read
  val isWrite   = axi_slave.io.bundle.write
  val wData = axi_slave.io.bundle.write_data

  // Registers (may use)
  val ctrl            = RegInit(0.U(32.W))
  val status          = RegInit(0.U(32.W))
  val perfCycles      = RegInit(0.U(32.W))
  val sparsitySkipped = RegInit(0.U(32.W))

  // Inputs needs to be latched
  val latched_rs1_data = RegInit(0.U(32.W))
  val latched_rs2_data = RegInit(0.U(32.W))

  // BitNet 特性：权重使用 2-bit 编码
  // 00 = 0 (跳过), 01 = +1 (加法), 10 = -1 (减法), 11 = 保留

  //val weight = Mem(256, UInt(2.W))       // 权重（2-bit 编码）
  val weight = Reg(Vec(256, UInt(2.W)))
  val bnSum4_activation = latched_rs1_data.asSInt
  val bnSum8_activation = Cat(latched_rs2_data, latched_rs1_data).asSInt
  val storetobuffer = Cat(latched_rs2_data(7,0), latched_rs1_data(7,0))

  // 状态机
  val sIdle :: sCompute_SUM4 :: sCompute_SUM8 :: sDone :: sStore :: Nil = Enum(5) // add sStore state
  val state = RegInit(sIdle)
  
  // default outputs
  axi_slave.io.bundle.read_data := 0.U
  axi_slave.io.bundle.read_valid := isRead
  io.irq := false.B
  io.accel_done := false.B
  io.busy := (state =/= sIdle)

  // 計算索引以及累加器
  val i = RegInit(0.U(8.W))  // 行索引
  val accumulator = RegInit(0.S(32.W))
  
    // For Debugging
  val cycle_count = RegInit(0.U(32.W))
  cycle_count := cycle_count + 1.U

  // FSM
  switch(state) {
    is(sIdle) {
      io.irq := false.B
      status := 0.U
      io.accel_done := false.B
      when(io.alu_bnrv === BNRVCore.Active) {
        i := 0.U
        accumulator := 0.S
        perfCycles  := 0.U
        sparsitySkipped := 0.U
        latched_rs1_data := io.rs1_data
        latched_rs2_data := io.rs2_data
        printf(p"Time: ${cycle_count} | [BN-LATCH] Latching Inputs! RS1=${io.rs1_data} | RS2=${io.rs2_data}\n")
        switch(io.funct7) {
          is(InstructionsTypeC.Store) {
            state := sStore
            i := 0.U
          }
          is(InstructionsTypeC.Sum4) {
            state := sCompute_SUM4
          }
          is(InstructionsTypeC.Sum8) {
            state := sCompute_SUM8
          }
        }
      }.otherwise {
        state := sIdle
      }
    }

    is(sStore) { // for BN.STORE
      status := 1.U
      perfCycles := perfCycles + 1.U

      val currentWeight = (storetobuffer >> (i << 1.U))(1, 0)
      weight(i) := currentWeight

      printf(p"Time: ${cycle_count} | [STORE] i: ${i} | Writing Weight: ${currentWeight} to weight(${i})\n")

      when(i === 7.U) {
        state := sDone
      }.otherwise {
        i := i + 1.U
      }
    }

    is(sCompute_SUM4) {
      status := 1.U
      perfCycles := perfCycles + 1.U

      // 权重编码: 00=0, 01=+1, 10=-1
      val aIdx = i * 8.U
      val wIdx = i * 2.U
      val aVal = (bnSum4_activation >> aIdx)(7, 0).asSInt      
      val wVal = (latched_rs2_data >> wIdx)(1, 0)
      
      
      // BitNet 核心：根据权重值选择操作（无乘法！）
      val newAccum = Wire(SInt(32.W))
      when(wVal === 1.U) {
        // 权重 = +1: 加法
        newAccum := accumulator + aVal
      }.elsewhen(wVal === 2.U) {
        // 权重 = -1: 减法
        newAccum := accumulator - aVal
      }.otherwise {
        // 权重 = 0: 跳过（稀疏性优化）
        newAccum := accumulator
        sparsitySkipped := sparsitySkipped + 1.U
      }

      accumulator := newAccum

      when(i === 3.U) {
        state := sDone
      }.otherwise {
        i := i + 1.U
      }
    }

    is(sCompute_SUM8) {
      status := 1.U
      perfCycles := perfCycles + 1.U
      
      // 权重编码: 00=0, 01=+1, 10=-1
      val aIdx = i * 8.U
      val aVal = (bnSum8_activation >> aIdx)(7, 0).asSInt      
      val wVal = weight(i)
      printf(p"Time: ${cycle_count} | [SUM8] i: ${i} | Weight: ${wVal} | Act: ${aVal} | Accum: ${accumulator}\n")

      // BitNet 核心：根据权重值选择操作（无乘法！）
      val newAccum = Wire(SInt(32.W))
      when(wVal === 1.U) {
        // 权重 = +1: 加法
        newAccum := accumulator + aVal
      }.elsewhen(wVal === 2.U) {
        // 权重 = -1: 减法
        newAccum := accumulator - aVal
      }.otherwise {
        // 权重 = 0: 跳过（稀疏性优化）
        newAccum := accumulator
        sparsitySkipped := sparsitySkipped + 1.U
      }

      accumulator := newAccum

      when(i === 7.U) {
          state := sDone
      }.otherwise {
        // 更新索引，继续累加
          i := i + 1.U
      }
    }
    is(sDone) {
      status := 2.U
      io.irq := true.B
      
      io.accel_done := true.B
      
     when(!io.alu_bnrv.asBool) {
        state := sIdle
      }
    }
  }

  // 寄存器读写 (may use)
  when(isWrite || isRead) {

    when(isWrite) {
      switch(regAddr) {
        is(0x00.U) { ctrl := wData }
        // ...
      }
    }
    
    when(isRead) {
      switch(regAddr) {
        is(0x00.U) { axi_slave.io.bundle.read_data := ctrl }
        is(0x04.U) { axi_slave.io.bundle.read_data := status}
        //...
      }
    }
  }

  io.bitnet_result := accumulator.asUInt



  when(io.alu_bnrv.asBool || state =/= sIdle) {
    printf(p"Time: ${cycle_count} | State: ${state} | F7: ${io.funct7} | ResReg: ${io.bitnet_result} | Done: ${io.accel_done}\n")
  }

  when(state === sCompute_SUM4 || state === sCompute_SUM8) {
    val current_wVal = Mux(state === sCompute_SUM4, 
                          (latched_rs2_data >> (i * 2.U))(1, 0), 
                          weight(i))
    val current_aVal = Mux(state === sCompute_SUM4,
                          (bnSum4_activation >> (i * 8.U))(7, 0).asSInt,
                          (bnSum8_activation >> (i * 8.U))(7, 0).asSInt)
    
    printf(p"Time: ${cycle_count} | State: ${state} | i: ${i} | Accum: ${accumulator} | W_bits: ${current_wVal} | Act: ${current_aVal}\n")
  }
}