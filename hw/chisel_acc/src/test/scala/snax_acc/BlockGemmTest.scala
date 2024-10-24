import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import snax_acc.gemm.{BlockGemm, DefaultConfig}
import scala.util.Random

class BlockGemmTest extends AnyFlatSpec with ChiselScalatestTester {
  "BlockGemm" should "process input data correctly with 2x2 matrix" in {
    test(new BlockGemm(DefaultConfig.gemmConfig)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val num_mat: Int = 4 // Usar 2x2 matriz para el test

      // Configurar parámetros del control
      dut.io.ctrl.bits.M_i.poke(num_mat.U)
      dut.io.ctrl.bits.K_i.poke(num_mat.U)
      dut.io.ctrl.bits.N_i.poke(num_mat.U)
      dut.io.ctrl.bits.subtraction_constant_i.poke(0.U)

      // Habilitar ctrl.valid y avanzar el reloj por unos ciclos fijos
      dut.io.ctrl.valid.poke(true.B)
      dut.clock.step(5)  // Esperar algunos ciclos para que el DUT procese el control
      dut.io.ctrl.valid.poke(false.B)

      // Generar matrices aleatorias
      val random = new Random(21)
      val maxValue = (1 << num_mat) - 1 // Ancho de bits para num_mat
      val matrixA = Array.fill(num_mat, num_mat)(random.nextInt(maxValue).U(num_mat.W))
      val matrixB = Array.fill(num_mat, num_mat)(random.nextInt(maxValue).U(num_mat.W))
      val matrixC = Array.fill(num_mat, num_mat)(0.U(num_mat.W)) // Inicializar matrixC en 0

      // Mostrar matrices generadas
      println(s"Matrix A (input a_i): ${matrixA.map(_.mkString(", ")).mkString("\n")}")
      println(s"Matrix B (input b_i): ${matrixB.map(_.mkString(", ")).mkString("\n")}")
      println(s"Matrix C (input c_i): ${matrixC.map(_.mkString(", ")).mkString("\n")}")

      // Calcular el resultado esperado
      val expectedMatrix = Array.ofDim[BigInt](num_mat, num_mat)
      for (i <- 0 until num_mat) {
        for (j <- 0 until num_mat) {
          expectedMatrix(i)(j) = (0 until num_mat).map(k => matrixA(i)(k).litValue * matrixB(k)(j).litValue).sum + matrixC(i)(j).litValue
        }
      }

      // Proveer datos de entrada sin depender de ready
      dut.io.data.a_i.valid.poke(true.B)
      dut.io.data.b_i.valid.poke(true.B)
      dut.io.data.c_i.valid.poke(true.B)

      // Enviar matrices completas fila por fila
      for (i <- 0 until num_mat) {
        // Enviar cada elemento de la fila
        for (j <- 0 until num_mat) {
          // Esperar a que el DUT esté listo para recibir datos
          while (dut.io.data.a_i.ready.peek().litToBoolean) {
            dut.clock.step(1) // Esperar hasta que esté listo
          }
          dut.io.data.a_i.bits.poke(matrixA(i)(j)) // Enviar cada elemento de A

          // Repetir para B y C
          while (dut.io.data.b_i.ready.peek().litToBoolean) {
            dut.clock.step(1)
          }
          dut.io.data.b_i.bits.poke(matrixB(i)(j)) // Enviar cada elemento de B

          while (dut.io.data.c_i.ready.peek().litToBoolean) {
            dut.clock.step(1)
          }
          dut.io.data.c_i.bits.poke(matrixC(i)(j)) // Enviar cada elemento de C

          dut.clock.step(1) // Avanzar un ciclo de reloj para procesar la fila completa
        }
      }

      // Esperar para que el módulo procese los datos
      dut.clock.step(10)

      // Verificar salida
      dut.io.data.d_o.ready.poke(true.B)
      val outputMatrix = Array.ofDim[BigInt](num_mat, num_mat)

      for (i <- 0 until num_mat) {
        var cycles = 0
        while (!dut.io.data.d_o.valid.peek().litToBoolean && cycles < 60) {
          println(s"Esperando por salida en posición ($i)... ciclo $cycles")
          dut.clock.step(1)
          cycles += 1
        }
        if (dut.io.data.d_o.valid.peek().litToBoolean) {
          for (j <- 0 until num_mat) {
            // Esperar a que el DUT esté listo para enviar la salida
            while (!dut.io.data.d_o.ready.peek().litToBoolean) {
              dut.clock.step(1) // Esperar hasta que esté listo
            }
            outputMatrix(i)(j) = dut.io.data.d_o.bits.peek().litValue
            println(s"Salida recibida en posición ($i, $j): ${outputMatrix(i)(j)}")
            dut.clock.step(1) // Asegurarse de que avanza para recibir la siguiente fila
          }
        } else {
          println(s"No se recibió la salida para la posición ($i) después de $cycles ciclos.")
        }
      }

      // Mostrar la matriz de salida y la matriz esperada
      println(s"Expected Matrix: ${expectedMatrix.map(_.mkString(", ")).mkString("\n")}")
      println(s"Output Matrix: ${outputMatrix.map(_.mkString(", ")).mkString("\n")}")
      println("Test completado")
    }
  }
}
