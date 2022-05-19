package workshop.mandelbrot

import spinal.core._
import spinal.lib._


case class Dispatcher[T <: Data](dataType : T,outputsCount : Int) extends Component{
  val io = new Bundle {
    val input   = slave Stream(dataType)
    val outputs = Vec(master Stream(dataType),outputsCount)
  }
  
  // val counter = Reg(UInt(log2Up(outputsCount) bits)) init(0)
  // when(io.input.fire){
  //   counter := counter + 1
  //   if(counter == outputsCount - 1){
  //     counter := 0
  //   }
  // }

  // for (output <- io.outputs){
  //   output.valid := False
  //   output.payload := io.input.payload
  // }

  // io.outputs(counter).valid := io.input.valid
  // io.input.ready := io.outputs(counter).ready

  val dispatchedStreams = StreamDispatcherSequencial(input = io.input, outputCount = outputsCount)

  for (i <- 0 until outputsCount){
    io.outputs(i) << dispatchedStreams(i)
  }
}

case class Arbiter[T <: Data](dataType : T,inputsCount : Int) extends Component{
  val io = new Bundle {
    val inputs = Vec(slave Stream(dataType), inputsCount)
    val output = master Stream(dataType)
  }

  // val counter = Reg(UInt(log2Up(inputsCount) bits)) init(0)
  // when(io.output.fire){
  //   counter := counter + 1
  //   if(counter == inputsCount - 1){
  //     counter := 0
  //   }
  // }

  // for (input <- io.inputs){
  //   input.ready := False
  // }
  // io.inputs(counter).ready := io.output.ready

  // io.output.valid   := io.inputs(counter).valid
  // io.output.payload := io.inputs(counter).payload

  val arbitred = StreamArbiterFactory.sequentialOrder.on(io.inputs)
  io.output << arbitred
}


case class PixelSolverMultiCore(g : PixelSolverGenerics,coreCount : Int) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (PixelTask(g))
    val rsp = master Stream (PixelResult(g))
  }

  // instantiate all components
  val taskDispatcher = Dispatcher(PixelTask(g), coreCount)
  val pixelSolvers = List.fill(coreCount)(PixelSolver(g))
  val resultArbiter = Arbiter(PixelResult(g), coreCount)

  // interconnect all that stuff
  taskDispatcher.io.input << io.cmd
  for (solverId <- 0 until coreCount){
    pixelSolvers(solverId).io.cmd     <-< taskDispatcher.io.outputs(solverId)
    resultArbiter.io.inputs(solverId) <-< pixelSolvers(solverId).io.rsp
  }
  io.rsp << resultArbiter.io.output
}

