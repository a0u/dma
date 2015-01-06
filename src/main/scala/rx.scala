package dma

import Chisel._
import uncore._

class DMAEngineRx extends Module {
  val io = new Bundle {
    val chn = new DMAChannelRxIO
    val mem = new UncachedTileLinkIO
    val dev = Decoupled(new DMAStream).flip
    val irq = Bool(OUTPUT)
  }

  private val w = params(DMAPtrRxBits)
  require(w > 1)

  val desc = Mem(new DMADescRx, 1 << w)
  val stat = Mem(new DMAStatRx, 1 << w)

  val eptr = Reg(init = UInt(0, w))
  val dptr = Reg(init = UInt(0, w))
  val cptr = Reg(init = UInt(0, w))
  val eptr_next = eptr + UInt(1)
  val dptr_next = dptr + UInt(1)
  val cptr_next = cptr + UInt(1)

  io.chn.enq.ready := (eptr_next != dptr)
  io.chn.enq.ptr := eptr
  when (io.chn.enq.fire()) {
    desc(eptr) := io.chn.enq.bits
    eptr := eptr_next
  }

  io.chn.deq.valid := (dptr != cptr)
  io.chn.deq.bits := stat(dptr)
  io.chn.deq.ptr := dptr
  when (io.chn.deq.fire()) {
    dptr := dptr_next
  }

  io.irq := Bool(false)

  val rxf = Module(new RxFrontEnd)
  val rxb = Module(new RxBackEnd)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  val op = desc(cptr)
  val op_ready = rxf.io.op.ready && rxb.io.op.ready
  val op_valid = (cptr != eptr) && (state === s_idle)
  rxf.io.op.valid := op_valid
  rxb.io.op.valid := op_valid
  rxf.io.op.bits := op
  rxb.io.op.bits := op

  rxf.io.in <> io.dev
  rxb.io.in <> rxf.io.out
  io.mem <> rxb.io.mem

  switch (state) {
    is (s_idle) {
      when (op_valid && op_ready) {
        state := s_busy
      }
    }

    is (s_busy) {
      when (rxf.io.stat.valid) {
        stat(cptr) := rxf.io.stat.bits
      }
      when (op_ready) {
        io.irq := Bool(true)
        cptr := cptr_next
        state := s_idle
      }
    }
  }
}

class DMABufRx extends Bundle {
  val data = Bits(width = params(TLDataBits))
  val last = Bool()
}

class RxFrontEnd extends Module {
  val io = new Bundle {
    val op = Decoupled(new DMADescRx).flip
    val in = Decoupled(new DMAStream).flip
    val out = Decoupled(new DMABufRx)
    val stat = Valid(new DMAStatRx)
  }

  io.op.ready := Bool(true)

  val buf = Vec.fill(params(DMAOffsetRange)){
    Reg(Bits(width = params(DMAStreamBits)))}

  private val IP_ALIGN = 2
  private val w = log2Up(params(DMAStreamBits)) - 3
  val index = Reg(init = UInt(IP_ALIGN, params(DMAOffsetBits)))
  val count = Reg(init = UInt(0, params(DMACountBits) - w))
  val last = Reg(Bool())

  val full = (index === UInt(buf.length-1))
  val incr = if (isPow2(buf.length)) Bool(true) else !full
  val next = full || io.in.bits.last

  io.in.ready := Bool(false)
  io.out.valid := Bool(false)
  io.out.bits.data := Cat((buf.length-1 to 0 by -1).map(buf(_)))
  io.out.bits.last := last
  io.stat.valid := Bool(false)
  io.stat.bits.cnt := (count << UInt(w))

  val s_pull :: s_push :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_pull)

  switch (state) {
    is (s_pull) {
      io.in.ready := Bool(true)
      last := io.in.bits.last
      when (io.in.valid) {
        buf(index) := io.in.bits.data
        index := Mux(io.in.bits.last, UInt(IP_ALIGN),
          (index + UInt(1)) & Fill(incr, params(DMAOffsetBits)))
        count := count + UInt(1)
        when (next) {
          state := s_push
        }
      }
    }

    is (s_push) {
      io.out.valid := Bool(true)
      when (io.out.ready) {
        when (io.out.bits.last) {
          io.stat.valid := Bool(true)
          count := UInt(0)
        }
        state := s_pull
      }
    }
  }
}

class RxBackEnd extends Module {
  val io = new Bundle {
    val op = Decoupled(new DMADescRx).flip
    val in = Decoupled(new DMABufRx).flip
    val mem = new UncachedTileLinkIO
  }

  private val w = log2Up(params(TLDataBits)) - 3
  val addr = Reg(UInt(width = params(TLAddrBits)))
  val last = Reg(Bool())

  val mem_gxid = Reg(Bits())
  val mem_gsrc = Reg(UInt())

  io.op.ready := Bool(false)
  io.in.ready := Bool(false)

  io.mem.acquire.valid := Bool(false)
  io.mem.acquire.bits.payload := UncachedWrite(addr, io.in.bits.data)
  io.mem.acquire.bits.header.src := UInt(params(LNClients)-1)
  io.mem.acquire.bits.header.dst := UInt(0) // FIXME (?)
  io.mem.grant.ready := Bool(false)
  io.mem.finish.valid := Bool(false)
  io.mem.finish.bits.payload.manager_xact_id := mem_gxid
  io.mem.finish.bits.header.dst := mem_gsrc

  val co = params(TLCoherence)
  val mem_ack = co.requiresAckForGrant(io.mem.grant.bits.payload)

  val s_idle :: s_req :: s_res :: s_ack :: Nil = Enum(UInt(), 4)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        addr := (io.op.bits.addr >> UInt(w))
        state := s_req
      }
    }

    is (s_req) {
      last := io.in.bits.last
      io.in.ready := io.mem.acquire.ready
      io.mem.acquire.valid := io.in.valid
      when (io.mem.acquire.fire()) {
        addr := addr + UInt(1)
        state := s_res
      }
    }

    is (s_res) {
      io.mem.grant.ready := Bool(true)
      when (io.mem.grant.valid) {
        mem_gxid := io.mem.grant.bits.payload.manager_xact_id
        mem_gsrc := io.mem.grant.bits.header.src
        state := MuxCase(s_req, Array(
          mem_ack -> s_ack,
          last -> s_idle))
      }
    }

    is (s_ack) {
      io.mem.finish.valid := Bool(true)
      when (io.mem.finish.ready) {
        state := Mux(last, s_idle, s_req)
      }
    }
  }
}

