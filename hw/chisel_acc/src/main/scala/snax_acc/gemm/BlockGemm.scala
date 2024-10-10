import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import snax_acc.gemm.{BlockGemm, DefaultConfig}
import scala.util.Random

class BlockGemmTest extends AnyFlatSpec with ChiselScalatestTester {
  "BlockGemm" should "process input data correctly with 2x2 matrix" in {
    test(new BlockGemm(DefaultConfig.gemmConfig)) { dut =>
      val num_mat: Int   = 256

      // Configurar parámetros del control
      dut.io.ctrl.bits.M_i.poke(num_mat.U)
      dut.io.ctrl.bits.K_i.poke(num_mat.U)
      dut.io.ctrl.bits.N_i.poke(num_mat.U)
      dut.io.ctrl.bits.subtraction_constant_i.poke(0.U)

      // Habilitar ctrl.valid
      dut.io.ctrl.valid.poke(true.B)
      dut.clock.step(1) // Esperar a que el módulo esté listo
      dut.io.ctrl.valid.poke(false.B)

      // Generar matrices aleatorias
      val random = new Random(100)
      val maxValue = (1 << num_mat) - 1 // Calcula el valor máximo para ese número de bits

      val matrixA = Array.fill(num_mat)(random.nextInt(999999999).U(num_mat.W))
      val matrixB = Array.fill(num_mat)(random.nextInt(999999999).U(num_mat.W))
      val matrixC = Array.fill(num_mat)(0.U(num_mat.W))



      // Mostrar matrices generadas (solo para depuración)
      println(s"Matrix A: ${matrixA.mkString(", ")}")
      println(s"Matrix B: ${matrixB.mkString(", ")}")
      println(s"Matrix C: ${matrixC.mkString(", ")}")

      // Calcular el resultado esperado en Scala usando solo binarios
      var expectedSum: BigInt = 0
      for (i <- 0 until num_mat) {
        val aValue = matrixA(i).litValue
        val bValue = matrixB(i).litValue
        expectedSum += (aValue * bValue) + matrixC(i).litValue
      }

      // Proveer datos de entrada
      dut.io.data.a_i.valid.poke(true.B)
      dut.io.data.b_i.valid.poke(true.B)
      dut.io.data.c_i.valid.poke(true.B)

      // Enviar los valores de las matrices
      for (i <- 0 until num_mat) {
        dut.io.data.a_i.bits.poke(matrixA(i))
        dut.io.data.b_i.bits.poke(matrixB(i))
        dut.io.data.c_i.bits.poke(matrixC(i))
        dut.clock.step(1) // Un paso por cada dato enviado
      }

      // Contador de ciclos
      var cycleCount = 0

      // Esperar a que el módulo procese los datos y contar los ciclos
      while (dut.io.data.d_o.valid.peek().litToBoolean == false) {
        dut.clock.step(1)
        cycleCount += 1
      }

      // Verificar salida
      val output = dut.io.data.d_o.bits.peek().litValue
      println(f"Output D: $output%d")
      println(f"Resultado calculado en TB: $expectedSum")

      // Comparar con el valor esperado
      val error = (output - expectedSum).abs
      println(f"Error de aproximación: $error")

      // Verificar si el error es aceptable
      if (error < 1) {
        println("Error de aproximación menor a 1")
      } else {
        println("Error de aproximación mayor al permitido")
      }

      // Imprimir el total de ciclos de reloj
      println(s"Total de ciclos de reloj: $cycleCount")
    }
  }
}
