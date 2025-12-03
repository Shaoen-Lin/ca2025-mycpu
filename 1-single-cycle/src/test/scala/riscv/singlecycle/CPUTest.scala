// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.singlecycle

import java.nio.ByteBuffer
import java.nio.ByteOrder

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import peripheral.InstructionROM
import peripheral.Memory
import peripheral.ROMLoader
import riscv.core.CPU
import riscv.core.ProgramCounter
import riscv.Parameters
import riscv.TestAnnotations

class TestTopModule(exeFilename: String) extends Module {
  val io = IO(new Bundle {
    val mem_debug_read_address  = Input(UInt(Parameters.AddrWidth))
    val regs_debug_read_address = Input(UInt(Parameters.PhysicalRegisterAddrWidth))
    val regs_debug_read_data    = Output(UInt(Parameters.DataWidth))
    val mem_debug_read_data     = Output(UInt(Parameters.DataWidth))
  })

  val mem             = Module(new Memory(8192))
  val instruction_rom = Module(new InstructionROM(exeFilename))
  val rom_loader      = Module(new ROMLoader(instruction_rom.capacity))

  rom_loader.io.rom_data     := instruction_rom.io.data
  rom_loader.io.load_address := Parameters.EntryAddress
  instruction_rom.io.address := rom_loader.io.rom_address

  val CPU_clkdiv = RegInit(UInt(2.W), 0.U)
  val CPU_tick   = Wire(Bool())
  val CPU_next   = Wire(UInt(2.W))
  CPU_next   := Mux(CPU_clkdiv === 3.U, 0.U, CPU_clkdiv + 1.U)
  CPU_tick   := CPU_clkdiv === 0.U
  CPU_clkdiv := CPU_next

  withClock(CPU_tick.asClock) {
    val cpu = Module(new CPU)
    cpu.io.debug_read_address  := 0.U
    cpu.io.instruction_valid   := rom_loader.io.load_finished
    mem.io.instruction_address := cpu.io.instruction_address
    cpu.io.instruction         := mem.io.instruction

    when(!rom_loader.io.load_finished) {
      rom_loader.io.bundle <> mem.io.bundle
      cpu.io.memory_bundle.read_data := 0.U
    }.otherwise {
      rom_loader.io.bundle.read_data := 0.U
      cpu.io.memory_bundle <> mem.io.bundle
    }

    cpu.io.debug_read_address := io.regs_debug_read_address
    io.regs_debug_read_data   := cpu.io.debug_read_data
  }

  mem.io.debug_read_address := io.mem_debug_read_address
  io.mem_debug_read_data    := mem.io.debug_read_data
}

class FibonacciTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Integration Tests")
  it should "correctly execute recursive Fibonacci(10) program" in {
    test(new TestTopModule("fibonacci.asmbin")).withAnnotations(TestAnnotations.annos) { c =>
      for (i <- 1 to 50) {
        c.clock.step(1000)
        c.io.mem_debug_read_address.poke((i * 4).U) // Avoid timeout
      }

      c.io.mem_debug_read_address.poke(4.U)
      c.clock.step()
      c.io.mem_debug_read_data.expect(55.U)
    }
  }
}

class QuicksortTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Integration Tests")
  it should "correctly execute Quicksort algorithm on 10 numbers" in {
    test(new TestTopModule("quicksort.asmbin")).withAnnotations(TestAnnotations.annos) { c =>
      for (i <- 1 to 50) {
        c.clock.step(1000)
        c.io.mem_debug_read_address.poke((i * 4).U) // Avoid timeout
      }
      for (i <- 1 to 10) {
        c.io.mem_debug_read_address.poke((4 * i).U)
        c.clock.step()
        c.io.mem_debug_read_data.expect((i - 1).U)
      }
    }
  }
}

class ByteAccessTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Integration Tests")
  it should "correctly handle byte-level store/load operations (SB/LB)" in {
    test(new TestTopModule("sb.asmbin")).withAnnotations(TestAnnotations.annos) { c =>
      for (i <- 1 to 500) {
        c.clock.step()
        c.io.mem_debug_read_address.poke((i * 4).U) // Avoid timeout
      }
      c.io.regs_debug_read_address.poke(5.U)
      c.io.regs_debug_read_data.expect(0xdeadbeefL.U)
      c.io.regs_debug_read_address.poke(6.U)
      c.io.regs_debug_read_data.expect(0xef.U)
      c.io.regs_debug_read_address.poke(1.U)
      c.io.regs_debug_read_data.expect(0x15ef.U)
    }
  }
}
/*
class FastRsqrtTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Assignment 2 FastRsqrt Tests")
  
  it should "correctly handle Assignment 2 problem(FastRsqrt Tests)" in {
    val expectedAnswers = Seq(
      65536,  // Input: 1
      46341,  // Input: 2
      37836,  // Input: 3 (約 0.577...)
      32768,  // Input: 4
      20724,  // Input: 10
      6553,   // Input: 100
      2072,   // Input: 1000
      655,    // Input: 10000
      1       // Input: 4294967295
    )

    test(new TestTopModule("fast_rsqrt.asmbin")).withAnnotations(TestAnnotations.annos) { c =>
     c.clock.setTimeout(0)
    
      for (_ <- 0 until 200) {
        c.clock.step(1000)
        c.io.mem_debug_read_address.poke(0.U) 
      }

      for ((expected, i) <- expectedAnswers.zipWithIndex) {

        val addr = 0x100 + i * 4
        
        c.io.mem_debug_read_address.poke(addr.U)
        c.clock.step() 

        c.io.mem_debug_read_data.expect(expected.U, s"Failed at index $i")
      }
    }
  }
}
*/
/*
class BF16Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Single Cycle CPU - Assignment 2 BF16 Tests")

  it should "correctly run Assignment 2 (BF16 implementation Tests)" in {
    val expectedAnswers = Seq(
      0x4000,  // Input: 1
      0x3F80,  // Input: 2
      0x40C0,  // Input: 3 
      0x4040,  // Input: 4
      1,       // Zero
      1,       // NaN 
      1        // Infinity
    )

    test(new TestTopModule("bf16_implementation.asmbin")).withAnnotations(TestAnnotations.annos) { c =>

      c.clock.setTimeout(0)

      for (_ <- 0 until 500) {
        c.clock.step(1000)
        c.io.mem_debug_read_address.poke(0.U) 
      }

      for ((expected, i) <- expectedAnswers.zipWithIndex) {
        val addr = 0x2000 + i * 4
        
        c.io.mem_debug_read_address.poke(addr.U)
        c.clock.step()
        
        c.io.mem_debug_read_data.expect(expected.U, s"Failed at index $i")
      }
    }
  }
 }
*/

