package workshop.pwm

import org.scalatest.FunSuite
import spinal.core._
import spinal.lib._

//APB configuration class (generic/parameter)
case class ApbConfig(addressWidth : Int,
                     dataWidth    : Int,
                     selWidth     : Int)

//APB interface definition
case class Apb(config: ApbConfig) extends Bundle with IMasterSlave {
  val PSEL       = Bits(config.selWidth bits)
  val PENABLE    = Bool()
  val PADDR      = UInt(config.addressWidth bit)
  val PWRITE     = Bool()
  val PWDATA     = Bits(config.dataWidth bit)
  val PRDATA     = Bits(config.dataWidth bit)
  val PREADY     = Bool()

  override def asMaster(): Unit = {
    out(PADDR,PSEL,PENABLE,PWRITE,PWDATA)
    in(PREADY,PRDATA)
  }
}

case class ApbPwm(apbConfig: ApbConfig,timerWidth : Int) extends Component{
  require(apbConfig.dataWidth == 32)
  require(apbConfig.selWidth == 1)

  val io = new Bundle{
    val apb = slave(Apb(apbConfig))
    val pwm = out (Reg(Bool()) init(False))
  }

  val logic = new Area {

    val enable = Reg(Bool()) init(False)
    val timer = Reg(UInt(timerWidth bits)) init(0)
    val dutyCycle = Reg(UInt(timerWidth bits)) init(0)

    when(enable){
      timer := timer + 1
    }

    when(timer === 0){
      io.pwm := True
    }
    when(timer === dutyCycle){
      io.pwm := False
    }
  }
  
  val control = new Area{
  
    io.apb.PREADY := True
    io.apb.PRDATA := 0
    switch(io.apb.PADDR){
      is(0){
        io.apb.PRDATA := logic.enable.asBits.resized
      }
      is(4){
        io.apb.PRDATA := logic.dutyCycle.asBits.resized
      }
    }

    when(io.apb.PENABLE && io.apb.PSEL(0) && io.apb.PWRITE){
      switch(io.apb.PADDR){
      is(0){
        logic.enable := io.apb.PWDATA(0)
      }
      is(4){
        logic.dutyCycle := io.apb.PWDATA.asUInt.resized
      }
    }
    }
  }
}