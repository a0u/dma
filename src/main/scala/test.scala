package dma

import Chisel._

class DMALoopBack extends Module {
  val io = new Bundle {
    val tx = Decoupled(new DMAStream).flip
    val rx = Decoupled(new DMAStream)
  }

  val q = Module(new Queue(new DMAStream, 2048))
  q.io.enq <> io.tx
  io.rx <> q.io.deq
}
