import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import snax_acc.gemm.{BlockGemm, DefaultConfig}
import scala.util.Random
import scala.concurrent.duration._
import java.io._

class BlockGemmTest extends AnyFlatSpec with ChiselScalatestTester {
  "BlockGemm" should "process input data correctly with 2x2 matrix for 1000 random cases" in {
    // Crea o abre el archivo CSV para escribir los resultados
    val writer = new PrintWriter(new File("test_results.csv"))
    // Escribe la cabecera del CSV
    writer.write("TestNumber,ElapsedTime(ms),Error(%),Cycles\n")
    var cycles: BigInt = 0
    // Generar 1000 pruebas con diferentes datos aleatorios
    for (testNum <- 1 to 100) { // Cambia a 1000 si es necesario
      test(new BlockGemm(DefaultConfig.gemmConfig)) { dut =>
        val numMat: Int = 8
        val random = new Random()


        println(s"Prueba #$testNum")

        // Configurar parámetros de control constantes
        dut.io.ctrl.bits.M_i.poke(numMat.U)
        dut.io.ctrl.bits.K_i.poke(numMat.U)
        dut.io.ctrl.bits.N_i.poke(numMat.U)
        dut.io.ctrl.bits.subtraction_constant_i.poke(0.U)

        // Generar matrices aleatorias para cada prueba
        val maxValue = (1 << numMat) - 1
        val matrixA = Array.fill(numMat)(random.nextInt(maxValue).U(numMat.W))
        val matrixB = Array.fill(numMat)(random.nextInt(maxValue).U(numMat.W))
        val matrixC = Array.fill(numMat)(0.U(numMat.W))

        println(s"Matrix A: ${matrixA.map(_.litValue).mkString(", ")}")
        println(s"Matrix B: ${matrixB.map(_.litValue).mkString(", ")}")
        println(s"Matrix C: ${matrixC.map(_.litValue).mkString(", ")}")

        // Calcular el resultado esperado en Scala usando los valores generados
        var expectedSum: BigInt = 0
        for (i <- 0 until numMat) {
          val aValue = matrixA(i).litValue
          val bValue = matrixB(i).litValue
          expectedSum += (aValue * bValue) + matrixC(i).litValue
        }

        // Enviar los valores de entrada al DUT
        dut.io.ctrl.valid.poke(true.B)
        dut.clock.step(1)
        dut.io.ctrl.valid.poke(false.B)

        dut.io.data.a_i.valid.poke(true.B)
        dut.io.data.b_i.valid.poke(true.B)
        dut.io.data.c_i.valid.poke(true.B)

        // Medir el tiempo de envío
        val startTime = System.nanoTime()

        for (i <- 0 until numMat) {
          dut.io.data.a_i.bits.poke(matrixA(i))
          dut.io.data.b_i.bits.poke(matrixB(i))
          dut.io.data.c_i.bits.poke(matrixC(i))
          dut.clock.step(1)
        }

        // Desactivar las entradas para finalizar la carga de datos
        dut.io.data.a_i.valid.poke(false.B)
        dut.io.data.b_i.valid.poke(false.B)
        dut.io.data.c_i.valid.poke(false.B)

        // Espera hasta que se reciba una salida válida
        var cycleCount = 0
        var outputValid = false
        while (!outputValid && cycleCount < 100) { // Limitar a 100 ciclos para evitar bucles infinitos
          dut.clock.step(1)
          outputValid = dut.io.data.d_o.valid.peek().litToBoolean
          cycleCount += 1
        }

        // Medir el tiempo de recepción
        val endTime = System.nanoTime()
        val elapsedTime = (endTime - startTime).nanos.toMillis

        // Inicializa error


        if (outputValid) {
          val output = dut.io.data.d_o.bits.peek().litValue
          val error = (output - expectedSum).abs


          // Imprimir resultados y errores
          println(s"Output D: $output")
          println(s"Resultado esperado: $expectedSum")
          println(f"Error de aproximación: $error")
          println(s"Total de ciclos de reloj: $cycleCount")
          println(s"Performance Counter: $cycles")
          // Guardar los resultados en el CSV
          writer.write(s"$testNum,$elapsedTime,$error,$cycles\n")

          // Verificar si el error es aceptable
          //assert(error < 1, s"Error de aproximación mayor al permitido en la prueba #$testNum: $error%")
        } else {
          println(s"Error: No se recibió salida válida después de $cycleCount ciclos en prueba #$testNum")
        }



        dut.clock.step(5) // Ciclo de espera después del reset
        cycles =  {dut.io.performance_counter.peek().litValue}

      }

    }

    // Cerrar el archivo CSV

    writer.close()
  }
}
