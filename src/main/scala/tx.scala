package dma

import Chisel._
import uncore._

class DMAEngineTx extends Module {
  val io = new Bundle {
    val chn = new DMAChannelTxIO
    val mem = new UncachedTileLinkIO 
    val dev = Decoupled(new DMAStream)
    val irq = Bool(OUTPUT)
  }

  private val w = params(DMAPtrTxBits)
  require(w > 1)

  val desc = Mem(new DMADescTx, 1 << w)
  val eptr = Reg(init = UInt(0, w))
  val dptr = Reg(init = UInt(0, w))
  val eptr_next = eptr + UInt(1)
  val dptr_next = dptr + UInt(1)

  val empty = (eptr === dptr)
  val full = (eptr_next === dptr)

  io.chn.enq.ready := !full
  io.chn.enq.ptr := eptr
  io.chn.deq.ptr := dptr
  io.irq := Bool(false)

  when (io.chn.enq.fire()) {
    desc(eptr) := io.chn.enq.bits
    eptr := eptr_next
  }

  val txf = Module(new TxFrontEnd)
  val txb = Module(new TxBackEnd)
  val txq = Module(new Queue(Bits(width = params(TLDataBits)), 2))

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  val op = desc(dptr)
  val op_ready = txf.io.op.ready && txb.io.op.ready
  val op_valid = !empty && (op.cnt != UInt(0)) && (state === s_idle)
  txf.io.op.valid := op_valid
  txb.io.op.valid := op_valid
  txf.io.op.bits := op
  txb.io.op.bits := op

  io.mem <> txf.io.mem
  txq.io.enq <> txf.io.out
  txb.io.in <> txq.io.deq
  io.dev <> txb.io.out

  switch (state) {
    is (s_idle) {
      when (op_valid && op_ready) {
        state := s_busy
      }
    }
    is (s_busy) {
      when (op_ready) {
        io.irq := Bool(true)
        dptr := dptr_next
        state := s_idle
      }
    }
  }
}

class TxFrontEnd extends Module {
  val io = new Bundle {
    val op = Decoupled(new DMADescTx).flip
    val mem = new UncachedTileLinkIO
    val out = Decoupled(Bits(width = params(TLDataBits)))
  }

  private val w = log2Up(params(TLDataBits)) - 3
  val addr = Reg(UInt(width = params(TLAddrBits)))
  val count = Reg(UInt(width = params(DMACountBits) - w))

  val cnt_done = (count === UInt(0))
  // Account for leading padding due to access alignment
  val cnt_byte = io.op.bits.cnt + io.op.bits.addr(w-1, 0)
  // Round upwards to nearest multiple of TileLink block size
  val cnt_block = (cnt_byte >> UInt(w)) + cnt_byte(w-1, 0).orR

  val mem_gxid = Reg(Bits())
  val mem_gsrc = Reg(UInt())

  io.mem.acquire.valid := Bool(false)
  io.mem.acquire.bits.payload := UncachedRead(addr)
  io.mem.acquire.bits.header.src := UInt(params(LNClients)-1)
  io.mem.acquire.bits.header.dst := UInt(0) // FIXME (?)
  io.mem.grant.ready := Bool(false)
  io.mem.finish.valid := Bool(false)
  io.mem.finish.bits.payload.master_xact_id := mem_gxid 
  io.mem.finish.bits.header.dst := mem_gsrc

  val co = params(TLCoherence)
  val mem_ack = co.requiresAckForGrant(io.mem.grant.bits.payload)

  io.op.ready := Bool(false)
  io.out.valid := Bool(false)
  io.out.bits := io.mem.grant.bits.payload.data

  val s_idle :: s_req :: s_res :: s_ack :: Nil = Enum(UInt(), 4)
  val state = Reg(init = s_idle)
 
  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        addr := (io.op.bits.addr >> UInt(w))
        count := cnt_block
        state := s_req
      }
    }

    is (s_req) {
      io.mem.acquire.valid := Bool(true)
      when (io.mem.acquire.ready) {
        addr := addr + UInt(1)
        count := count - UInt(1)
        state := s_res
      }
    }

    is (s_res) {
      io.out.valid := io.mem.grant.valid
      io.mem.grant.ready := io.out.ready
      when (io.out.fire()) {
        mem_gxid := io.mem.grant.bits.payload.master_xact_id
        mem_gsrc := io.mem.grant.bits.header.src
        state := MuxCase(s_req, Array(
          mem_ack -> s_ack,
          cnt_done -> s_idle))
      }
    }

    is (s_ack) {
      io.mem.finish.valid := Bool(true)
      when (io.mem.finish.ready) {
        state := Mux(cnt_done, s_idle, s_req)
      }
    }
  }
}

class TxBackEnd extends Module {
  val io = new Bundle {
    val op = Decoupled(new DMADescTx).flip
    val in = Decoupled(Bits(width = params(TLDataBits))).flip
    val out = Decoupled(new DMAStream)
  }

  val count = Reg(UInt(width = params(DMACountBits)))
  val index = Reg(UInt(width = params(DMAOffsetBits)))
  val dequeue = io.out.bits.last ||
    (index === UInt(params(DMAOffsetRange)-1))

  private val wd = params(TLDataBits)
  private val ws = params(DMAStreamBits)
  require(wd % ws == 0)

  val buf = Vec((0 until wd by ws).map(
    i => io.in.bits(i + ws - 1, i)))
  io.out.bits.data := buf(index)
  io.out.bits.last := (count === UInt(1))

  io.op.ready := Bool(false)
  io.in.ready := Bool(false)
  io.out.valid := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        state := s_busy
        count := io.op.bits.cnt
        index := io.op.bits.addr(log2Up(wd)-4, log2Up(ws)-3)
      }
    }

    is (s_busy) {
      io.in.ready := io.out.ready && dequeue
      io.out.valid := io.in.valid

      when (io.out.fire()) {
        index := index + UInt(1)
        if (isPow2(params(DMAOffsetRange))) {
          when (dequeue) {
            index := UInt(0)
          }
        }
        count := count - UInt(1)
        when (io.out.bits.last) {
          state := s_idle
        }
      }
    }
  }

}
