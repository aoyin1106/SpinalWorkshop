package workshop.blackboxAndClock

import spinal.core._
import spinal.lib._


// Define a Ram as a BlackBox
case class Ram_1w_1r_2c(wordWidth: Int, addressWidth: Int,writeClock : ClockDomain,readClock : ClockDomain) extends BlackBox {
  // define Generics
  addGeneric("wordWidth", wordWidth)
  addGeneric("addressWidth", addressWidth)

  // define IO
  val io = new Bundle{
    val wr = new Bundle{
      val clk  = in Bool()
      val en   = in Bool()
      val addr = in UInt(addressWidth bits)
      val data = in Bits(wordWidth bits)
    }
    val rd = new Bundle{
      val clk  = in Bool()
      val en   = in Bool()
      val addr = in UInt(addressWidth bits)
      val data = out Bits(wordWidth bits)
    }
  }

  // define ClockDomains mappings
  mapClockDomain(clockDomain = readClock,  clock = io.rd.clk)
  mapClockDomain(clockDomain = writeClock, clock = io.wr.clk)
}

// Create the top level and instanciate the Ram
case class MemorySumming(writeClock : ClockDomain,sumClock : ClockDomain) extends Component {
  val io = new Bundle {
    val wr = new Bundle {
      val en   = in Bool()
      val addr = in UInt (8 bits)
      val data = in Bits (16 bits)
    }

    val sum = new Bundle{
      val start = in Bool()
      val done  = out Bool()
      val value = out UInt(16 bits)
    }
  }

  // define the ram
  val ram = new Ram_1w_1r_2c(wordWidth = 16, addressWidth = 8, writeClock = writeClock, readClock = sumClock)

  // connect the io.wr port to the ram
  io.wr.en   <> ram.io.wr.en
  io.wr.addr <> ram.io.wr.addr
  io.wr.data <> ram.io.wr.data

  val sumArea = new ClockingArea(sumClock){
    // memory read + summing logic
    val readEn = Reg(Bool) init(False)
    val counter = Reg(UInt(widthOf(ram.io.rd.addr) bits)) init(0) // holds address

    ram.io.rd.en := readEn
    ram.io.rd.addr := counter

    when(!readEn){
      counter := 0
      when(io.sum.start){
        readEn  := True
      }
    }otherwise{
      counter := counter + 1
      when(counter === counter.maxValue){
        readEn := False
      }
    }

    val sumEn = RegNext(readEn) init(False)
    val sum = Reg(UInt(widthOf(io.sum.value) bits)) init(0)

    io.sum.value := sum

    when(!sumEn){
      sum := 0
    }otherwise{
      sum := sum + ram.io.rd.data.asUInt
    }

    io.sum.done := sumEn.fall(initAt=False)
  }
}
