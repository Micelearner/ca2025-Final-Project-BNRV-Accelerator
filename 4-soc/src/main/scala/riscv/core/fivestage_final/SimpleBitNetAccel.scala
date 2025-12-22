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

package riscv.core.fivestage_final

import chisel3._
import chisel3.util.MuxLookup
import riscv.Parameters

// ============================================================================
// Simple Register Interface (替代 AXI4-Lite)
// ============================================================================

class SimpleRegIO extends Bundle {
  val addr = Input(UInt(32.W))
  val wdata = Input(UInt(32.W))
  val rdata = Output(UInt(32.W))
  val wen = Input(Bool())
  val ren = Input(Bool())
  val valid = Input(Bool())
  val ready = Output(Bool())
}

class SimpleBitNetAccel extends Module {
  val io = IO(new Bundle {

  // 
  val instruction = Input(UInt(Parameters.InstructionWidth))
  val rs1_data = Input(UInt(32.W))
  val rs2_data = Input(UInt(32.W)) 
  val mem_bitn_result = Output(UInt(32.W))
  val alu_bnrv = Input(UInt(1.W))
  //
    val reg = new SimpleRegIO()
    val irq = Output(Bool())
  })
  
  // 
  val funct7 = io.instruction(31, 25) 
  //


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
  val activation = Mem(256, SInt(32.W))  // 激活值（8-bit 或 32-bit）
  val weight = Mem(256, UInt(2.W))       // 权重（2-bit 编码）
  val result = Mem(256, SInt(32.W))      // 结果（32-bit）
  
  // 状态机
  val sIdle :: sCompute :: sFinalize :: sDone :: Nil = Enum(4)
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
  
  // 默认输出
  io.reg.rdata := 0.U
  io.reg.ready := true.B
  io.irq := false.B
  

//
when (alu_bnrv){
  when (funct7 === InstructionsTypeC.bnsum4){
    val rs1_vals = VecInit(Seq.fill(4)(0.S(8.W)))
    for (i <- 0 until 4) {
      rs1_vals(i) := io.rs1_data(8*i+7, 8*i).asSInt
    }

    val weights = VecInit(Seq.fill(4)(0.U(2.W)))
    for (i <- 0 until 4) {
      weights(i) := io.rs2_data(2*i+1, 2*i)
    }

    val sum4Result = rs1_vals.zip(weights).map { case (data, w) =>
      MuxLookup(w, 0.S(32.W), Seq(
        1.U -> data.asSInt,  
        2.U -> (-data).asSInt 
      ))
    }.reduce(_ + _)

    io.mem_bitn_result := sum4Result.asUInt

  }.otherwise {
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
            state := sCompute
            perfCycles := 0.U
            sparsitySkipped := 0.U
            i := 0.U
            j := 0.U
            k := 0.U
            accumulator := 0.S
            finalizeCounter := 0.U
            errorCode := 0.U
          }
        }
      }
      is(sCompute) {
        status := 1.U
        perfCycles := perfCycles + 1.U
        
        // BitNet 矩阵乘法: result[i][j] += activation[i][k] * weight[k][j]
        // 权重编码: 00=0, 01=+1, 10=-1
        val rowStride = 16.U  // 固定行跨度为 16
        val aIdx = i * rowStride + k
        val wIdx = k * rowStride + j
        val aVal = activation(aIdx)
        val wVal = weight(wIdx)
        
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
        when(k < matrixSize - 1.U) {
          // 继续累加
          accumulator := newAccum
          k := k + 1.U
        }.otherwise {
          // k 循环完成，保存结果
          val rIdx = i * rowStride + j
          result(rIdx) := newAccum
          lastSavedAddr := rIdx
          lastSavedValue := newAccum
          
          // 检查是否是最后一个元素
          val isLastElement = (i === matrixSize - 1.U) && (j === matrixSize - 1.U)
          
          when(isLastElement) {
            // 最后一个元素，直接进入 finalize
            state := sFinalize
          }.otherwise {
            // 不是最后一个元素，继续计算
            accumulator := 0.S
            k := 0.U
            
            // 移动到下一个元素
            when(j < matrixSize - 1.U) {
              j := j + 1.U
            }.otherwise {
              j := 0.U
              i := i + 1.U
            }
          }
        }
      }
      is(sFinalize) {
        // 等待多个周期，确保最后一次结果写入完成
        // 这对于 Mem 的写入很重要，特别是最后一行
        finalizeCounter := finalizeCounter + 1.U
        when(finalizeCounter >= 3.U) {
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
  }
  }.otherwise {
    rs1_data := 0.U
    rs2_data := 0.U
    mem_bitn_result := 0.U
  }

//  
  // 寄存器读写
  when(io.reg.valid) {
    val regAddr = io.reg.addr(11, 0)
    
    when(io.reg.wen) {
      switch(regAddr) {
        is(0x000.U) { ctrl := io.reg.wdata }
        is(0x01C.U) { 
          // 限制矩阵大小在 2-8 范围内
          when(io.reg.wdata >= 2.U && io.reg.wdata <= 8.U) {
            matrixSize := io.reg.wdata
          }.otherwise {
            // 超出范围，设置为默认值 8
            matrixSize := 8.U
          }
        }
        is(0x020.U) { config := io.reg.wdata }
      }
      
      // 激活值写入（32-bit）
      when(regAddr >= 0x100.U && regAddr < 0x300.U) {
        val idx = (regAddr - 0x100.U) >> 2
        activation(idx) := io.reg.wdata.asSInt
      }
      
      // 权重写入（2-bit 编码）
      // 用户写入 32-bit 值，我们编码为 2-bit
      // 0 -> 00 (零), 1 -> 01 (+1), -1 -> 10 (-1)
      when(regAddr >= 0x300.U && regAddr < 0x500.U) {
        val idx = (regAddr - 0x300.U) >> 2
        val inputVal = io.reg.wdata.asSInt
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
    
    when(io.reg.ren) {
      switch(regAddr) {
        is(0x000.U) { io.reg.rdata := ctrl }
        is(0x004.U) { io.reg.rdata := status }
        is(0x01C.U) { io.reg.rdata := matrixSize }
        is(0x020.U) { io.reg.rdata := config }
        is(0x028.U) { io.reg.rdata := perfCycles }
        is(0x02C.U) { io.reg.rdata := sparsitySkipped }  // 稀疏性统计
        is(0x030.U) { io.reg.rdata := errorCode }  // 错误代码
      }
      
      // 激活值读取
      when(regAddr >= 0x100.U && regAddr < 0x300.U) {
        val idx = (regAddr - 0x100.U) >> 2
        io.reg.rdata := activation(idx).asUInt
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
        io.reg.rdata := decodedWeight.asUInt
      }
      
      // 结果读取
      when(regAddr >= 0x500.U && regAddr < 0x900.U) {
        val idx = (regAddr - 0x500.U) >> 2
        io.reg.rdata := result(idx).asUInt
      }
    }
  }
}
