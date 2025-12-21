package aiAcc

import circt.stage.ChiselStage

/**
 * 统一的 Verilog 生成器
 * * 生成所有设计的 SystemVerilog 代码
 */
object VerilogGenerator extends App {
  println("=" * 80)
  println("RISC-V AI 加速器 - 统一 Verilog 生成器")
  println("=" * 80)
  println()
  
  var successCount = 0
  var failCount = 0
  val totalDesigns = 1
  
  // 生成单个设计的辅助函数
  // 修改點：使用泛型 T 並限制為 RawModule，直接傳遞 generator 避免提前求值
  def generateDesign[T <: chisel3.RawModule](name: String, generator: => T, targetDir: String): Boolean = {
    println(s"[$successCount/$totalDesigns] 正在生成: $name")
    println(s"  目标目录: $targetDir")
    
    try {
      // 核心修改：直接傳入 generator (by-name parameter)
      // ChiselStage 會在內部分配好 Builder context 後才呼叫它
      ChiselStage.emitSystemVerilogFile(
        generator,
        firtoolOpts = GeneratorConfig.getFirtoolOpts,
        args = Array("--target-dir", targetDir)
      )
      
      // 后处理
      val mainFile = s"$targetDir/${name}.sv"
      if (new java.io.File(mainFile).exists()) {
        PostProcessVerilog.cleanupVerilogFile(mainFile)
        val lines = scala.io.Source.fromFile(mainFile).getLines().size
        println(s"  ✅ 成功: $mainFile ($lines 行)")
        successCount += 1
        true
      } else {
        println(s"  ❌ 失败: 文件未生成")
        failCount += 1
        false
      }
    } catch {
      case e: Exception =>
        // 印出詳細錯誤堆疊以便調試
        e.printStackTrace()
        println(s"  ❌ 错误: ${e.getMessage}")
        failCount += 1
        false
    }
  }
  
  println("\n" + "=" * 80)
  println("Phase 1: 生成 SimpleEdgeAiSoC (推荐设计)")
  println("=" * 80)
  println()
  
  // 這裡傳入的是一個代碼塊，只有在 generateDesign 內部呼叫 generator 時才會執行
  generateDesign(
    "SimpleEdgeAiSoC",
    new SimpleEdgeAiSoC(),
    "generated/simple_edgeaisoc"
  )
  
  println()
  println("=" * 80)
  println("生成总结")
  println("=" * 80)
  println(s"  总设计数: $totalDesigns")
  println(s"  ✅ 成功: $successCount")
  println(s"  ❌ 失败: $failCount")
  println()
  
  if (failCount == 0) {
    println("🎉 所有设计生成成功！")
    println()
    println("📁 生成的文件:")
    println("  - generated/simple_edgeaisoc/SimpleEdgeAiSoC.sv")
    println()
    println("🚀 下一步:")
    println("  1. 查看生成的 SystemVerilog 文件")
    println("  2. 运行测试: ./run.sh full SimpleEdgeAiSoC")
    println("  3. 阅读文档: docs/SimpleEdgeAiSoC_README.md")
    println()
    println("物理优化代码生成完成")  // 用于 run.sh 检测
  } else {
    println("⚠️  部分设计生成失败")
    println()
    println("💡 调试建议:")
    println("  1. 检查编译错误: sbt compile")
    println("  2. 查看详细日志")
    println("  3. 清理重编译: sbt clean compile")
    System.exit(1)
  }
}