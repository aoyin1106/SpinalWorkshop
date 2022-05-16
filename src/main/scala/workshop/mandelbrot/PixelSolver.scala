package workshop.mandelbrot

import spinal.core._
import spinal.lib._

case class PixelSolverGenerics(fixAmplitude : Int,
                               fixResolution : Int,
                               iterationLimit : Int){
  val iterationWidth = log2Up(iterationLimit+1)
  def iterationType = UInt(iterationWidth bits)
  def fixType = SFix(
    peak = fixAmplitude exp,
    resolution = fixResolution exp
  )
}

case class PixelTask(g : PixelSolverGenerics) extends Bundle{
  val x,y = g.fixType
}

case class PixelResult(g : PixelSolverGenerics) extends Bundle{
  val iteration = g.iterationType
}

case class PixelSolver(g : PixelSolverGenerics) extends Component{
  val io = new Bundle{
    val cmd = slave  Stream(PixelTask(g))
    val rsp = master Stream(PixelResult(g))
  }

  import g._

  val idWidth = 3
  class Context extends Bundle{
    val id        = UInt(idWidth bits)
    val x0,y0     = fixType
    val iteration = UInt(iterationWidth bits)
    val done      = Bool
  }

  case class InserterContext() extends Context{
    val x,y = fixType
  }

  case class MulStageContext() extends Context{
    val xx,yy,xy = fixType
  }

  case class AddStageContext() extends Context{
    val x,y = fixType
  }

  case class RouterContext() extends Context{
    val x,y = fixType
  }

  val inserter = new Area{
    val loopback = Stream(RouterContext())
    val freeId = Counter(1 << idWidth,inc = io.cmd.fire)
    val cmdContext = InserterContext()
    cmdContext.id := freeId
    cmdContext.x0 := io.cmd.x
    cmdContext.y0 := io.cmd.y
    cmdContext.x  := 0.0
    cmdContext.y  := 0.0
    cmdContext.iteration := 0
    cmdContext.done := False

    val output = Stream(InserterContext())
    output.valid := io.cmd.valid || loopback.valid
    when(loopback.valid){
      output.payload.assignSomeByName(loopback.payload)
    } otherwise {
      output.payload.assignSomeByName(cmdContext)
    }
    io.cmd.ready := !loopback.valid
  }

  val mulStage = new Area{
    val input = inserter.output.stage()
    val output = Stream(MulStageContext())

    input.ready := output.ready

    output.valid := input.valid
    output.payload.assignSomeByName(input.payload)
    output.xx := (input.x * input.x).truncated
    output.yy := (input.y * input.y).truncated
    output.xy := (input.x * input.y).truncated
  }

  val addStage = new Area{
    val input = mulStage.output.stage()
    val output = Stream(AddStageContext())

    input.ready := output.ready

    output.valid := input.valid
    output.payload.assignSomeByName(input.payload)
    output.x         := (input.xx - input.yy + input.x0).truncated
    output.y         := (((input.xy) << 1)   + input.y0).truncated
    output.done.allowOverride
    output.iteration.allowOverride
    output.done      := input.done || input.xx + input.yy >= 4.0 || input.iteration === iterationLimit
    output.iteration := input.iteration + (!output.done).asUInt
  }


  val router = new Area{
    val input = addStage.output.stage()
    val wantedId = Counter(1 << idWidth,inc = io.rsp.fire)

    input.ready := True
    when(input.valid && input.done && wantedId === input.id){
      input.ready := io.rsp.ready
    }

    io.rsp.valid := input.valid && input.done && wantedId === input.id
    io.rsp.iteration := input.iteration

    inserter.loopback.valid := input.valid && (!(input.done && wantedId === input.id) )
    inserter.loopback.payload.assignSomeByName(input.payload)
  }
}

