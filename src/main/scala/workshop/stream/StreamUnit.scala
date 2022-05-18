package workshop.stream

import spinal.core._
import spinal.lib._


case class MemoryWrite() extends Bundle{
  val address = UInt(8 bits)
  val data    = Bits(32 bits)
}

case class StreamUnit() extends Component{
  val io = new Bundle{
    val memWrite = slave  Flow(MemoryWrite())
    val cmdA     = slave  Stream(UInt(8 bits))
    val cmdB     = slave  Stream(Bits(32 bits))
    val rsp      = master Stream(Bits(32 bits))
  }

  val mem = Mem(Bits(32 bits),1 << 8)

  mem.write(
    address = io.memWrite.payload.address,
    data    = io.memWrite.payload.data,
    enable  = io.memWrite.valid
  )

  val memReadData = mem.readSync(
    address = io.cmdA.payload,
    enable = io.cmdA.fire
  )
  val memReadReady = Bool()
  val memReadValid = Reg(Bool()) init(False) //read needs one cycle, so valid must sync with payload

  // cmdA handshake
  io.cmdA.ready := memReadReady || !memReadValid
  when(io.cmdA.ready){
    memReadValid := io.cmdA.valid
  }

  // join
  io.rsp.payload := memReadData ^ io.cmdB.payload
  io.rsp.valid := memReadValid && io.cmdB.valid
  io.cmdB.ready := io.rsp.fire
  memReadReady := io.rsp.fire
}
