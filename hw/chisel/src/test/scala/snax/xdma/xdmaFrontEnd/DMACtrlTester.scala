package snax.xdma.xdmaFrontend

import chisel3._
import chisel3.util._

// Hardware and its Generation Param
import snax.csr_manager._
import snax.xdma.DesignParams._
import snax.readerWriter.ReaderWriterParam

// Import Chiseltest
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

// Import Random number generator
import scala.util.Random

// Import break support for loops
import scala.util.control.Breaks.{break, breakable}

class DMACtrlTester extends AnyFlatSpec with ChiselScalatestTester {

  def write_csr(dut: Module, port: SnaxCsrIO, addr: Int, data: Int) = {

    // give the data and address to the right ports
    port.req.bits.write.poke(true.B)

    port.req.bits.data.poke(data.U)
    port.req.bits.addr.poke(addr.U)
    port.req.valid.poke(1.B)

    // wait for grant
    while (port.req.ready.peekBoolean() == false) {

      dut.clock.step(1)
    }

    dut.clock.step(1)

    port.req.valid.poke(0.B)
  }

  def extractBits(in: BigInt, upper: Int, lower: Int): BigInt = {
    var temp = in >> lower
    temp = temp & ((BigInt(1) << (upper - lower + 1)) - 1)
    temp
  }

  "The DMACtrl" should " pass" in {
    test(
      new DMACtrl(
        readerparam = new DMADataPathParam(
          new AXIParam,
          new ReaderWriterParam(
            configurableChannel = true,
            configurableByteMask = false
          ),
          Seq()
        ),
        writerparam = new DMADataPathParam(
          new AXIParam,
          new ReaderWriterParam(
            configurableChannel = true,
            configurableByteMask = true
          ),
          Seq()
        )
      )
    ).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) {
      dut =>
        val clusterBaseAddress = 0x1000_0000
        dut.io.clusterBaseAddress.poke(clusterBaseAddress)

        // The threads list (empty at the beginning)
        var concurrent_threads = new chiseltest.internal.TesterThreadList(Seq())
        val unreceived_reader_cfg = new collection.mutable.HashSet[Int]()
        val unreceived_writer_cfg = new collection.mutable.HashSet[Int]()
        var testTerminated = false

        val endedThreadList = new collection.mutable.HashSet[String]()

        dut.clock.setTimeout(0)

        // The thread to push the data inside the Ctrl from local side and remote side
        concurrent_threads = concurrent_threads.fork {
          val Reader_PointerAddress = BigInt(0x1000_0000)
          // Under This Configurations, the full TCDM will be copied
          val Reader_Spatial_Strides = List(8)
          val Reader_Temporal_Bounds = List(32, 64)
          val Reader_Temporal_Strides = List(64, 2048)

          val Writer_PointerAddress = BigInt(0x1000_0000)
          // Under This Configurations, the full TCDM will be copied
          val Writer_Spatial_Strides = List(32)
          val Writer_Temporal_Bounds = List(32, 16)
          val Writer_Temporal_Strides = List(256, 8192)

          for (i <- 0 until 256) {
            if (testTerminated) break()
            if (Random.between(0, 2) == 0) {
              // Local Cfg injection
              // Applying Data into the CSRManager
              var currentCSR = 0x0

              val Reader_PointerAddress_ThisLoop =
                Reader_PointerAddress + Random.between(0, 2) * 256 * 1024 + i
              val Writer_PointerAddress_ThisLoop = Writer_PointerAddress + i
              unreceived_reader_cfg.add(Reader_PointerAddress_ThisLoop.toInt)
              unreceived_writer_cfg.add(Writer_PointerAddress_ThisLoop.toInt)

              // Reader: Pointers LSB + MSB
              write_csr(
                dut,
                dut.io.csrIO,
                addr = currentCSR,
                data = (Reader_PointerAddress_ThisLoop & 0xffff_ffff).toInt
              )
              currentCSR += 1
              write_csr(
                dut,
                dut.io.csrIO,
                addr = currentCSR,
                data =
                  ((Reader_PointerAddress_ThisLoop >> 32) & 0xffff_ffff).toInt
              )
              currentCSR += 1

              // Reader: Spatial Strides D0
              Reader_Spatial_Strides.foreach({ i =>
                write_csr(dut, dut.io.csrIO, addr = currentCSR, data = i)
                currentCSR += 1
              })
              // Reader: Temporal Strides D0 -> D1
              Reader_Temporal_Strides.foreach({ i =>
                write_csr(dut, dut.io.csrIO, addr = currentCSR, data = i)
                currentCSR += 1
              })
              // Reader: Temporal Bounds D0 -> D1
              Reader_Temporal_Bounds.foreach({ i =>
                write_csr(dut, dut.io.csrIO, addr = currentCSR, data = i)
                currentCSR += 1
              })
              // Enabled Channels
              write_csr(dut, dut.io.csrIO, addr = currentCSR, data = 0xff)
              currentCSR += 1

              // Writer: Pointers LSB + MSB
              write_csr(
                dut,
                dut.io.csrIO,
                addr = currentCSR,
                data = ((Writer_PointerAddress_ThisLoop) & 0xffff_ffff).toInt
              )
              currentCSR += 1
              write_csr(
                dut,
                dut.io.csrIO,
                addr = currentCSR,
                data =
                  (((Writer_PointerAddress_ThisLoop) >> 32) & 0xffff_ffff).toInt
              )
              currentCSR += 1

              // Writer: Spatial Strides D0
              Writer_Spatial_Strides.foreach({ i =>
                write_csr(dut, dut.io.csrIO, addr = currentCSR, data = i)
                currentCSR += 1
              })
              // Writer: Temporal Strides D0 -> D1
              Writer_Temporal_Strides.foreach({ i =>
                write_csr(dut, dut.io.csrIO, addr = currentCSR, data = i)
                currentCSR += 1
              })
              // Writer: Temporal Bounds D0 -> D1
              Writer_Temporal_Bounds.foreach({ i =>
                write_csr(dut, dut.io.csrIO, addr = currentCSR, data = i)
                currentCSR += 1
              })
              // Enabled Channels
              write_csr(dut, dut.io.csrIO, addr = currentCSR, data = 0xff)
              currentCSR += 1
              // Enabled Byte
              write_csr(dut, dut.io.csrIO, addr = currentCSR, data = 0xff)
              currentCSR += 1
              // Commit the config
              write_csr(dut, dut.io.csrIO, addr = currentCSR, data = 1)
              println(
                "[Local Reader Generator] " + Reader_PointerAddress_ThisLoop.toInt.toHexString
              )
              println(
                "[Local Writer Generator] " + Writer_PointerAddress_ThisLoop.toInt.toHexString
              )

            } else {

              // Remote Cfg injection
              unreceived_reader_cfg.add(
                (Reader_PointerAddress + 0x0000_1000 + i).toInt
              )
              val remoteConfig: BigInt =
                (Reader_PointerAddress + 0x0000_1000 + i) +
                  (BigInt(0x2000_0000) << 48) +
                  (BigInt(Reader_Spatial_Strides(0)) << 96) +
                  (BigInt(Reader_Temporal_Strides(0)) << 113) +
                  (BigInt(Reader_Temporal_Strides(1)) << 130) +
                  (BigInt(Reader_Temporal_Bounds(0)) << 147) +
                  (BigInt(Reader_Temporal_Bounds(1)) << 164)

              // 240b for AGU, 272b / 34B / 17 INT16 for Extension

              dut.io.remoteDMADataPathCfg.fromRemote.bits.poke(remoteConfig)
              dut.clock.step(Random.between(1, 31))
              dut.io.remoteDMADataPathCfg.fromRemote.valid.poke(true.B)
              while (
                !(dut.io.remoteDMADataPathCfg.fromRemote.ready.peekBoolean())
              ) {
                dut.clock.step()
              }
              dut.clock.step()
              dut.io.remoteDMADataPathCfg.fromRemote.valid.poke(false.B)
              println(
                "[Remote Reader Generator] " + (Reader_PointerAddress + 0x0000_1000 + i).toInt.toHexString
              )
            }
          }
          endedThreadList.add("Generator")
          println("Generator thread is terminated. ")
        }

        // The thread to pop the data outside the Ctrl from local reader side
        concurrent_threads = concurrent_threads.fork {
          breakable(
            while (true) {
              while (!dut.io.localDMADataPath.readerStart.peekBoolean()) {
                dut.clock.step()
                if (testTerminated) break()
              }
              dut.clock.step(Random.between(1, 16) + 32)
              dut.io.localDMADataPath.readerBusy.poke(true)
              println(
                "[Local Reader Checker] " + dut.io.localDMADataPath.readerCfg.readerPtr
                  .peekInt()
                  .toInt
                  .toHexString
              )
              if (
                !unreceived_reader_cfg.remove(
                  dut.io.localDMADataPath.readerCfg.readerPtr
                    .peekInt()
                    .toInt
                )
              )
                throw new Exception(
                  "[Local Reader Checker] The received pointer " + dut.io.localDMADataPath.readerCfg.readerPtr
                    .peekInt()
                    .toInt
                    .toHexString + " is not in the buffer"
                )
              dut.clock.step(Random.between(1, 16) + 32)
              dut.io.localDMADataPath.readerBusy.poke(false)
            }
          )
          println("Local Reader Checker is terminated. ")
        }

        // The thread to pop the data outside the Ctrl from local writer side
        concurrent_threads = concurrent_threads.fork {
          breakable(
            while (true) {
              while (!dut.io.localDMADataPath.writerStart.peekBoolean()) {
                dut.clock.step()
                if (testTerminated) break()
              }
              println(
                "[Local Writer Checker] " + dut.io.localDMADataPath.writerCfg.writerPtr
                  .peekInt()
                  .toInt
                  .toHexString
              )
              if (
                !unreceived_writer_cfg.remove(
                  dut.io.localDMADataPath.writerCfg.writerPtr
                    .peekInt()
                    .toInt
                )
              )
                throw new Exception(
                  "[Local Writer Checker] The received pointer " + dut.io.localDMADataPath.writerCfg.writerPtr
                    .peekInt()
                    .toInt
                    .toHexString + " is not in the buffer"
                )
              dut.clock.step(Random.between(1, 16) + 32)
              dut.io.localDMADataPath.writerBusy.poke(true)
              dut.clock.step(Random.between(1, 16) + 32)
              dut.io.localDMADataPath.writerBusy.poke(false)
            }
          )
          println("Local Writer Checker is terminated. ")
        }

        // The thread to pop the data outside the Ctrl from remote side
        concurrent_threads = concurrent_threads.fork {
          dut.io.remoteDMADataPathCfg.toRemote.ready.poke(false)
          breakable(
            while (true) {
              if (testTerminated) break()
              if (dut.io.remoteDMADataPathCfg.toRemote.valid.peekBoolean()) {
                println(
                  "[Remote Reader Checker] " + extractBits(
                    dut.io.remoteDMADataPathCfg.toRemote.bits.peekInt(),
                    95,
                    48
                  ).toInt.toHexString
                )
                if (
                  !unreceived_reader_cfg.remove(
                    extractBits(
                      dut.io.remoteDMADataPathCfg.toRemote.bits.peekInt(),
                      47,
                      0
                    ).toInt
                  )
                )
                  throw new Exception(
                    "[Remote Reader Checker] The received pointer " + extractBits(
                      dut.io.remoteDMADataPathCfg.toRemote.bits.peekInt(),
                      47,
                      0
                    ).toInt.toHexString + " is not in the buffer"
                  )
                dut.clock.step(Random.between(1, 16) + 32)
                dut.io.remoteDMADataPathCfg.toRemote.ready.poke(true)
                dut.clock.step(1)
                dut.io.remoteDMADataPathCfg.toRemote.ready.poke(false)
              } else dut.clock.step(1)
            }
          )
          println("Remote Reader Checker is terminated. ")
        }

        // The supervision thread
        concurrent_threads = concurrent_threads.fork {
          dut.clock.step(512)
          while (endedThreadList.size < 1) dut.clock.step()
          while (
            (!unreceived_reader_cfg.isEmpty) & (!unreceived_writer_cfg.isEmpty)
          ) {
            dut.clock.step()
          }
          println("Testbench finished. The checker will be terminated soon...")
          testTerminated = true
        }

        concurrent_threads.joinAndStep()
    }
  }
}
