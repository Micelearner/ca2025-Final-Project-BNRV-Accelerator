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

  val useUpper = RegInit(false.B) 

  val storetobuffer = Cat(latched_rs2_data(15,0), latched_rs1_data(15,0))

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

      // val currentWeight = (storetobuffer >> (i << 1.U))(1, 0)
      for (idx <- 0 until 16) {
        weight(idx) := (storetobuffer >> (idx * 2))(1, 0)
      }
      useUpper := false.B
      state := sDone
    }

    is(sCompute_SUM4) {
      status := 1.U
      perfCycles := perfCycles + 1.U

      // 权重编码: 00=0, 01=+1, 10=-1
      // val aIdx = i * 8.U
      // val wIdx = i * 2.U
      // val aVal = (bnSum4_activation >> aIdx)(7, 0).asSInt      
      // val wVal = (latched_rs2_data >> wIdx)(1, 0)
      val a0 = latched_rs1_data(7,0).asSInt
      val a1 = latched_rs1_data(15,8).asSInt
      val a2 = latched_rs1_data(23,16).asSInt
      val a3 = latched_rs1_data(31,24).asSInt

      val w0 = latched_rs2_data(1,0)
      val w1 = latched_rs2_data(3,2)
      val w2 = latched_rs2_data(5,4)
      val w3 = latched_rs2_data(7,6)

      def contrib(a: SInt, w: UInt): SInt = {
        val out = Wire(SInt(32.W))
        when(w === 1.U) { out := a }        // +1
        .elsewhen(w === 2.U) { out := -a } // -1
        .otherwise { out := 0.S }          // 0 or reserved
        out
      }
      val c0 = contrib(a0, w0)
      val c1 = contrib(a1, w1)
      val c2 = contrib(a2, w2)
      val c3 = contrib(a3, w3)
      accumulator := c0 + c1 + c2 + c3
      
      state := sDone
    }

    is(sCompute_SUM8) {
      status := 1.U
      perfCycles := perfCycles + 1.U
      
      /// 8 activations from Cat(latched_rs2_data, latched_rs1_data)
      val a0 = bnSum8_activation(7,0).asSInt
      val a1 = bnSum8_activation(15,8).asSInt
      val a2 = bnSum8_activation(23,16).asSInt
      val a3 = bnSum8_activation(31,24).asSInt
      val a4 = bnSum8_activation(39,32).asSInt
      val a5 = bnSum8_activation(47,40).asSInt
      val a6 = bnSum8_activation(55,48).asSInt
      val a7 = bnSum8_activation(63,56).asSInt

      // 8 weights from buffer
      val base = Mux(useUpper, 8.U, 0.U)
      val w0 = weight(base + 0.U)
      val w1 = weight(base + 1.U)
      val w2 = weight(base + 2.U)
      val w3 = weight(base + 3.U)
      val w4 = weight(base + 4.U)
      val w5 = weight(base + 5.U)
      val w6 = weight(base + 6.U)
      val w7 = weight(base + 7.U)
      useUpper := !useUpper
      


      def contrib(a: SInt, w: UInt): SInt = {
        val out = Wire(SInt(32.W))
        when(w === 1.U) { out := a }        // +1
        .elsewhen(w === 2.U) { out := -a } // -1
        .otherwise { out := 0.S }          // 0 or reserved
        out
      }
      val c0 = contrib(a0, w0)
      val c1 = contrib(a1, w1)
      val c2 = contrib(a2, w2)
      val c3 = contrib(a3, w3)
      val c4 = contrib(a4, w4)
      val c5 = contrib(a5, w5)
      val c6 = contrib(a6, w6)
      val c7 = contrib(a7, w7)


      accumulator := c0 + c1 + c2 + c3 + c4 + c5 + c6 + c7

      state := sDone
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



  
}