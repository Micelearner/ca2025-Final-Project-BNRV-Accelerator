// ============================================================================
// SimpleBitNetAccel - 真正的 BitNet 实现（无乘法器）
// 基于 BitNet 论文：权重只有 {-1, 0, +1}，使用加减法代替乘法
// 
// 特性：
// - 无乘法器设计，只使用加减法
// - 权重 2-bit 编码：00=0, 01=+1, 10=-1
// - 稀疏性优化：自动跳过零权重
// - 支持 2x2 到 16x16 矩阵
// ============================================================================

package riscv.core

import bus._
import chisel3._
import chisel3.util._
import chisel3.util.MuxLookup
import riscv.Parameters
import riscv.core.Instructions

class SimpleBitNetAccel extends Module {
  val io = IO(new Bundle {
    val axi4_channels = Flipped(new AXI4LiteChannels(Parameters.AddrBits, Parameters.DataBits))
    val irq = Output(Bool())

    val funct7 = Input(UInt(7.W))
    val rs1_data = Input(UInt(32.W))
    val rs2_data = Input(UInt(32.W))
    val bitnet_result = Output(UInt(32.W))
    val alu_bnrv = Input(UInt(1.W))
    val busy = Output(Bool())
  })
  
  // channels
  val axi_slave = Module(new AXI4LiteSlave(Parameters.AddrBits, Parameters.DataBits))
  axi_slave.io.channels <> io.axi4_channels

  // bundle
  val regAddr = axi_slave.io.bundle.address(7, 0)
  val isRead =  axi_slave.io.bundle.read
  val isWrite = axi_slave.io.bundle.write
  val wData =   axi_slave.io.bundle.write_data

  // 默认输出
  axi_slave.io.bundle.read_data := 0.U
  axi_slave.io.bundle.read_valid := isRead
  io.irq := false.B
  
  // 寄存器 (may use)
  val ctrl = RegInit(0.U(32.W))
  val status = RegInit(0.U(32.W))
  val perfCycles = RegInit(0.U(32.W))
  val sparsitySkipped = RegInit(0.U(32.W))  // 跳过的零权重计数
  
  // BitNet 特性：权重使用 2-bit 编码
  // 00 = 0 (跳过), 01 = +1 (加法), 10 = -1 (减法), 11 = 保留
  val weight = Mem(256, UInt(2.W))       // 权重（2-bit 编码）
  val bnSum4_activation = io.rs1_data.asSInt
  val bnSum8_activation = Cat(io.rs2_data, io.rs1_data).asSInt

  // 状态机
  val sIdle :: sCompute_SUM4 :: sCompute_SUM8 :: sDone :: sStore :: Nil = Enum(5) // add sStore state
  val state = RegInit(sIdle)
  
  // 計算索引以及累加器
  val i = RegInit(0.U(8.W))  // 行索引
  val accumulator = RegInit(0.S(32.W))
  
  // for BN.STORE
  val storetobuffer = Cat(io.rs2_data(7,0), io.rs1_data(7,0))
  io.busy := (state =/= sIdle)
  
  // 计算状态机
  switch(state) {
    is(sIdle) {
      status := 0.U
      when(io.alu_bnrv === BNRVCore.active) {
          i := 0.U
          accumulator := 0.S
          perfCycles := 0.U
          sparsitySkipped := 0.U

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
      val wVal = (io.rs2_data >> wIdx)(1, 0)
      
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
          // 更新索引, 继续累加
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
      state := sIdle
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
        is(0x000.U) { axi_slave.io.bundle.read_data := ctrl }
        is(0x004.U) { axi_slave.io.bundle.read_data := status}
      }
    }
  }

  io.bitnet_result := accumulator.asUInt
}