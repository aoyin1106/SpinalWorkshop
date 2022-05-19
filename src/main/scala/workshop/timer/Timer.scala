package workshop.timer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


case class Timer(width : Int) extends Component {
  val io = new Bundle {
    val tick = in Bool()
    val clear = in Bool()
    val limit = in UInt (width bits)

    val full  = out Bool()
    val value = out UInt (width bits)
  }

  val timer = Reg(UInt(width bits)) init(0)
  when(io.tick && timer < io.limit){
    timer := timer + 1
  }
  when(io.clear){
    timer := 0
  }

  io.full  := timer === io.limit
  io.value := timer

  def driveFrom(busCtrl : BusSlaveFactory,baseAddress : BigInt)(ticks : Seq[Bool],clears : Seq[Bool]) = new Area {
    val ticksEnable  = busCtrl.createReadAndWrite(dataType = Bits(ticks.length  bits), address = baseAddress, bitOffset =  0)  init(0)
    val clearsEnable = busCtrl.createReadAndWrite(dataType = Bits(clears.length bits), address = baseAddress, bitOffset =  16) init(0)

    busCtrl.driveAndRead(io.limit, address = baseAddress + 4, bitOffset = 0)
    busCtrl.read(io.value, address = baseAddress + 8, bitOffset = 0)

    val busWriteClear = busCtrl.isWriting(address = baseAddress + 8) || busCtrl.isWriting(address = baseAddress + 4)

    io.tick := (ticksEnable & ticks.asBits()).orR
    io.clear:= (clearsEnable & clears.asBits()).orR || busWriteClear
  }
}
