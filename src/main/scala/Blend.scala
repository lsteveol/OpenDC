package opendc


import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.ChiselStage



/**
  * Modified versions of https://stackoverflow.com/questions/16708338/how-to-design-a-64-x-64-bit-array-multiplier-in-verilog
  */
class MultiplierSingle(width: Int) extends MultiIOModule{
  val a   = IO(Input (UInt(width.W)))
  val b   = IO(Input (UInt(width.W)))
  val out = IO(Output(UInt((width*width).W)))
  
  val partials = Wire(Vec(width, UInt((width*width).W)))
  
  partials(0) := Mux(a(0), b, 0.U)
  for(i <- 1 until width){
    partials(i) := Mux(a(i), b << i, 0.U) + partials(i-1)
  }
  
  out := partials(width-1)
  
}

object MultiplierSingle{
  def apply(a: UInt, b: UInt): UInt = {
    val mp = Module(new MultiplierSingle(a.getWidth))
    mp.a := a
    mp.b := b
    mp.out
  }
}



class ColorBlender extends MultiIOModule{    
  val cold = IO(Input (UInt(8.W)))
  val cnew = IO(Input (UInt(8.W)))
  val anew = IO(Input (UInt(8.W)))
  val cout = IO(Output(UInt(8.W)))
  
  //out = (alpha * new) + (1 - alpha) * old
  cout := (MultiplierSingle(anew, cnew) + MultiplierSingle((255.U-anew), cold)) >> 8
}


object ColorBlender{
  def apply(o: UInt, cn: UInt, an: UInt): UInt = {
    val cb = Module(new ColorBlender)
    cb.cold := o
    cb.cnew := cn
    cb.anew := an
    cb.cout
  }
}






