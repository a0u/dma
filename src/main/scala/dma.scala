package dma

import Chisel._
import uncore._

class DMAEngine extends Module {
  val io = new Bundle {
    val cmd = new DMAControlIO
    val mem = new TileLinkIO
    val dev = new Bundle {
      val rx = Decoupled(new DMAStream).flip
      val tx = Decoupled(new DMAStream)
    }
  }

  val tx = Module(new DMAEngineTx)
  val rx = Module(new DMAEngineRx)
  val arb = Module(new UncachedTileLinkIOArbiterThatUsesNewId(2))

  tx.io.chn <> io.cmd.tx
  rx.io.chn <> io.cmd.rx
  tx.io.dev <> io.dev.tx
  rx.io.dev <> io.dev.rx

  arb.io.in(0) <> tx.io.mem
  arb.io.in(1) <> rx.io.mem
  io.mem <> arb.io.out
  io.mem.probe.ready := Bool(false)
  io.mem.release.valid := Bool(false)

  io.cmd.irq := tx.io.irq || rx.io.irq
}

