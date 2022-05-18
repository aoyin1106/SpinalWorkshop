package workshop.function

import spinal.core._
import spinal.lib._

import scala.collection.mutable

case class FunctionUnit() extends Component{
  val io = new Bundle{
    val cmd    = slave Flow(Bits(8 bits))
    val valueA = out Bits(8 bits)
    val valueB = out Bits(32 bits)
    val valueC = out Bits(48 bits)
  }

  def patternDetector(str : String) = new Area{
    val counter = Reg(UInt(log2Up(str.length) bits)) init(0)
    val hit = False
    val strHits = str.map(i => i.toInt === io.cmd.payload)
    val strHitHot = strHits(counter)
    
    when(io.cmd.valid){
      when(strHitHot){
        when(counter === str.length-1){
          counter := 0
          hit := True
        } otherwise {
          counter := counter + 1
        }
      } otherwise {
        counter := 0
      }
    }
  }

  def valueLoader(start : Bool,that : Data)= new Area{
    require(widthOf(that) % widthOf(io.cmd.payload) == 0) //You can make the assumption that the 'that' width is always an mulitple of 8

    val limit = widthOf(that) / widthOf(io.cmd.payload)

    val active  = Reg(Bool()) init(False)
    val counter = Reg(UInt(log2Up(limit) bits)) init(0)
    val buffer  = Reg(Bits(widthOf(that) bits)) init(0)

    when(!active){
      counter := 0
      active  := start
    }otherwise{
      when(io.cmd.valid){
        counter := (counter + 1).resized
        buffer.subdivideIn(limit slices)(counter) := io.cmd.payload
        when(counter === limit - 1){
          active := False
        }
      }
    }
    that.assignFromBits(buffer)
  }

  val setA    = patternDetector("setValueA")
  val loadA   = valueLoader(setA.hit,io.valueA)

  val setB    = patternDetector("setValueB")
  val loadB   = valueLoader(setB.hit,io.valueB)

  val setC    = patternDetector("setValueC")
  val loadC   = valueLoader(setC.hit,io.valueC)
}

