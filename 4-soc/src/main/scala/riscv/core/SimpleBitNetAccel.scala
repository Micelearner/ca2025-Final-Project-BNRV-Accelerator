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
  val regAddr = axi_slave.io.bundle.address(11, 0)
  val isRead =  axi_slave.io.bundle.read
  val isWrite = axi_slave.io.bundle.write
  val wData =   axi_slave.io.bundle.write_data

  // 默认输出
  // io.reg.rdata := 0.U
  axi_slave.io.bundle.read_data := 0.U
  // io.reg.ready := true.B
  axi_slave.io.bundle.read_valid := isRead
  io.irq := false.B
  
  // 寄存器
  val ctrl = RegInit(0.U(32.W))
  val status = RegInit(0.U(32.W))
  val config = RegInit(0.U(32.W))
  val matrixSize = RegInit(8.U(32.W))  // 默认 8x8（最大支持 8x8）
  val perfCycles = RegInit(0.U(32.W))
  val sparsitySkipped = RegInit(0.U(32.W))  // 跳过的零权重计数
  val errorCode = RegInit(0.U(32.W))  // 错误代码
  
  // BitNet 特性：权重使用 2-bit 编码
  // 00 = 0 (跳过), 01 = +1 (加法), 10 = -1 (减法), 11 = 保留
  // val activation = Mem(256, SInt(32.W))  // 激活值（8-bit 或 32-bit）
  val weight = Mem(256, UInt(2.W))       // 权重（2-bit 编码）
  // val result = Mem(256, SInt(32.W))      // 结果（32-bit）
  val rs1_rs2_activation = Cat(io.rs2_data, io.rs1_data).asSInt

  // 状态机
  val sIdle :: sCompute_SUM4 :: sCompute_SUM8 :: sDone :: sStore :: Nil = Enum(5) // add sStore state
  val state = RegInit(sIdle)
  
  // 矩阵乘法计算索引 (16x16)
  val i = RegInit(0.U(8.W))  // 行索引
  val j = RegInit(0.U(8.W))  // 列索引
  val k = RegInit(0.U(8.W))  // 累加索引
  val accumulator = RegInit(0.S(32.W))
  
  // 调试信息
  val lastSavedAddr = RegInit(0.U(8.W))
  val lastSavedValue = RegInit(0.S(32.W))
  
  // Finalize 计数器 - 等待多个周期确保写入完成
  val finalizeCounter = RegInit(0.U(3.W))
  
  // for BN.STORE
  val storetobuffer = Cat(io.rs2_data(7,0), io.rs1_data(7,0))
  io.busy := (state =/= sIdle)
  
  // 计算状态机
  switch(state) {
    is(sIdle) {
      status := 0.U
      when(ctrl(0)) {
        // 检查矩阵大小是否在支持范围内（2x2 到 8x8）
        when(matrixSize < 2.U || matrixSize > 8.U) {
          // 矩阵大小超出范围，设置错误状态
          status := 3.U  // 错误状态
          errorCode := 1.U  // 错误代码 1: 矩阵大小超出范围
        }.otherwise {
          // 矩阵大小有效，开始计算
          // state := sCompute
          perfCycles := 0.U
          sparsitySkipped := 0.U
          i := 0.U
          j := 0.U
          k := 0.U
          accumulator := 0.S
          errorCode := 0.U
        }
      }
      when(io.alu_bnrv === BNRVCore.active) {
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
        ctrl := 0.U  // 非 BitNet 指令，重置控制寄存器
        // need to reset other states?
      }
    }
    is(sStore) { // for BN.STORE
      status := 1.U
      
      val currentWeight = (storetobuffer >> (i << 1.U))(1, 0)
      weight(i) := currentWeight

      when(i === 7.U) {
        state := sIdle
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
      val aVal = (rs1_rs2_activation >> aIdx)(7, 0).asSInt      
      val wVal = (rs2_data >> wIdx)(1, 0)
      
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
      
      // 更新索引
      when(i < 4.U) {
        // 继续累加
        accumulator := newAccum
        i := i + 1.U
      }.otherwise {
        // i 循环完成，保存结果
          accumulator := newAccum
          state := sDone
      }
    }
    is(sCompute_SUM8) {
      status := 1.U
      perfCycles := perfCycles + 1.U
      
      // 权重编码: 00=0, 01=+1, 10=-1
      val aIdx = i * 8.U
      val aVal = (rs1_rs2_activation >> aIdx)(7, 0).asSInt      
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
      
      // 更新索引
      when(i < 8.U) {
        // 继续累加
        accumulator := newAccum
        i := i + 1.U
      }.otherwise {
        // i 循环完成，保存结果
          accumulator := newAccum
          state := sDone
      }
    }
    
    is(sDone) {
      status := 2.U
      io.irq := true.B
      ctrl := 0.U
      state := sIdle
    }
  }
  
  // 寄存器读写
  when(isWrite || isRead) {
    // val regAddr = io.reg.addr(11, 0)

    when(isWrite) {
      switch(regAddr) {
        is(0x000.U) { ctrl := wData }
        is(0x01C.U) { 
          // 限制矩阵大小在 2-8 范围内
          when(wData >= 2.U && wData <= 8.U) {
            matrixSize := wData
          }.otherwise {
            // 超出范围，设置为默认值 8
            matrixSize := 8.U
          }
        }
        is(0x020.U) { config := wData }
      }
      
      // 激活值写入（32-bit）
      when(regAddr >= 0x100.U && regAddr < 0x300.U) {
        val idx = (regAddr - 0x100.U) >> 2
        activation(idx) := wData.asSInt
      }
      
      // 权重写入（2-bit 编码）
      // 用户写入 32-bit 值，我们编码为 2-bit
      // 0 -> 00 (零), 1 -> 01 (+1), -1 -> 10 (-1)
      when(regAddr >= 0x300.U && regAddr < 0x500.U) {
        val idx = (regAddr - 0x300.U) >> 2
        val inputVal = wData.asSInt
        val encodedWeight = Wire(UInt(2.W))
        when(inputVal === 0.S) {
          encodedWeight := 0.U  // 00 = 0
        }.elsewhen(inputVal === 1.S) {
          encodedWeight := 1.U  // 01 = +1
        }.elsewhen(inputVal === -1.S) {
          encodedWeight := 2.U  // 10 = -1
        }.otherwise {
          // 对于非 BitNet 值，简化处理：大于0视为+1，小于0视为-1
          encodedWeight := Mux(inputVal > 0.S, 1.U, 2.U)
        }
        weight(idx) := encodedWeight
      }
    }
    
    when(isRead) {
      switch(regAddr) {
        is(0x000.U) { axi_slave.io.bundle.read_data := ctrl }
        is(0x004.U) { axi_slave.io.bundle.read_data := status }
        is(0x01C.U) { axi_slave.io.bundle.read_data := matrixSize }
        is(0x020.U) { axi_slave.io.bundle.read_data := config }
        is(0x028.U) { axi_slave.io.bundle.read_data := perfCycles }
        is(0x02C.U) { axi_slave.io.bundle.read_data := sparsitySkipped }  // 稀疏性统计
        is(0x030.U) { axi_slave.io.bundle.read_data := errorCode }  // 错误代码
      }
      
      // 激活值读取
      when(regAddr >= 0x100.U && regAddr < 0x300.U) {
        val idx = (regAddr - 0x100.U) >> 2
        axi_slave.io.bundle.read_data := activation(idx).asUInt
      }
      
      // 权重读取（解码回 32-bit）
      when(regAddr >= 0x300.U && regAddr < 0x500.U) {
        val idx = (regAddr - 0x300.U) >> 2
        val encodedWeight = weight(idx)
        val decodedWeight = Wire(SInt(32.W))
        when(encodedWeight === 0.U) {
          decodedWeight := 0.S
        }.elsewhen(encodedWeight === 1.U) {
          decodedWeight := 1.S
        }.elsewhen(encodedWeight === 2.U) {
          decodedWeight := -1.S
        }.otherwise {
          decodedWeight := 0.S
        }
        axi_slave.io.bundle.read_data := decodedWeight.asUInt
      }
      
      // 结果读取
      when(regAddr >= 0x500.U && regAddr < 0x900.U) {
        val idx = (regAddr - 0x500.U) >> 2
        axi_slave.io.bundle.read_data := result(idx).asUInt
      }
    }
  }

  io.bitnet_result := accumulator.asUInt
}