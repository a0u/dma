package dma

import Chisel._
import uncore._

case object DMAStreamBits extends Field[Int]
case object DMACountBits extends Field[Int]
case object DMAOffsetRange extends Field[Int]
case object DMAOffsetBits extends Field[Int]
case object DMAPtrTxBits extends Field[Int]
case object DMAPtrRxBits extends Field[Int]

trait HasDMAAddress extends Bundle {
  val addr = UInt(width = params(PAddrBits))
}
trait HasDMACount extends Bundle {
  val cnt = UInt(width = params(DMACountBits))
}

class DMADescTx extends Bundle
  with HasDMAAddress
  with HasDMACount

class DMADescRx extends Bundle with HasDMAAddress
class DMAStatRx extends Bundle with HasDMACount

class DMAEnqIO[T <: Data](gen: T, w: Int) extends DecoupledIO(gen) {
  val ptr = UInt(INPUT, w)
}
class DMADeqIO[T <: Data](gen: T, w: Int) extends DecoupledIO(gen) {
  val ptr = UInt(OUTPUT, w)
}

class DMAChannelTxIO extends Bundle {
  private val w = params(DMAPtrTxBits)
  val enq = new DMAEnqIO(new DMADescTx, w).flip
  val deq = new Bundle {
    val ptr = UInt(OUTPUT, w)
  }
}

class DMAChannelRxIO extends Bundle {
  private val w = params(DMAPtrRxBits)
  val enq = new DMAEnqIO(new DMADescRx, w).flip
  val deq = new DMADeqIO(new DMAStatRx, w)
}

class DMAControlIO extends Bundle {
  val tx = new DMAChannelTxIO
  val rx = new DMAChannelRxIO
  val irq = Bool(OUTPUT)
}

class DMAStream extends Bundle {
  val data = Bits(width = params(DMAStreamBits))
  val last = Bool()
}

