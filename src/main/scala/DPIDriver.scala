package opendc

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.ChiselStage

class DPIBundle extends Bundle{
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val de    = Output(Bool())
  val data  = new Bundle{
    val red   = Output(UInt(8.W))
    val blue  = Output(UInt(8.W))
    val green = Output(UInt(8.W))
  }
  
  
}

//By default DecopupledIO is an output
class PixelPipeline extends Bundle{
  val a = UInt(8.W)
  val r = UInt(8.W)
  val g = UInt(8.W)
  val b = UInt(8.W)
}

class FrameConfig extends Bundle{
  //Pixel Clocks
  val hsync   = Input(UInt(16.W))
  val hbp     = Input(UInt(16.W))
  val hfp     = Input(UInt(16.W))
  val hcol    = Input(UInt(16.W))
  
  //Lines
  val vsync   = Input(UInt(16.W))
  val vfp     = Input(UInt(16.W))
  val vbp     = Input(UInt(16.W))
  val vlines  = Input(UInt(16.W))
}



object DPIState extends ChiselEnum{
  val IDLE        = Value(0.U)
  val VSYNC       = Value(1.U)
  val VBP         = Value(2.U)
  val VACT        = Value(3.U)
  val VFP         = Value(4.U)
}

class DPIDriver extends MultiIOModule{
  
  val en        = IO(Input (Bool()))
  val hsync_pol = IO(Input (Bool()))
  val vsync_pol = IO(Input (Bool()))
  val de_pol    = IO(Input (Bool()))
  
  val frame   = IO(new FrameConfig)
  val pix     = IO(Flipped(Decoupled(new PixelPipeline)))
  val dpi     = IO(new DPIBundle)
  
  import DPIState._
    
  val vcount_in   = Wire(UInt(16.W))
  val vcount      = RegNext(vcount_in, 0.U)
  
  val hcount_in   = Wire(UInt(16.W))
  val hcount      = RegNext(hcount_in, 0.U)
  
  
  
  val hcount_max  = frame.hsync + frame.hbp + frame.hcol + frame.hfp
  val hsync_act   =  hcount < frame.hsync
  val hbp_act     = (hcount < (frame.hsync + frame.hbp)) & ~hsync_act
  val hcol_act    = (hcount < (frame.hsync + frame.hbp + frame.hcol)) & ~hbp_act & ~hsync_act
  val hfp_act     = (hcount < (frame.hsync + frame.hbp + frame.hcol + frame.hfp)) & ~hcol_act & ~hbp_act & ~hsync_act
  dontTouch(hfp_act)  //for waves
  val eol         =  hcount >= hcount_max - 1.U
  
  hcount_in       := Mux(~en, 0.U, Mux(eol, 0.U, hcount + 1.U))
  
  val vcount_max  = frame.vsync + frame.vbp + frame.vlines + frame.vfp
  val vsync_act   =  vcount < frame.vsync
  val vbp_act     = (vcount < (frame.vsync + frame.vbp)) & ~vsync_act
  val vlines_act  = (vcount < (frame.vsync + frame.vbp + frame.vlines)) & ~vbp_act & ~vsync_act
  val vfp_act     = (vcount < (frame.vsync + frame.vbp + frame.vlines + frame.vfp)) & ~vlines_act & ~vbp_act & ~vsync_act
  dontTouch(vfp_act) //for waves
  val eof         =  vcount >= vcount_max - 1.U
  
  vcount_in       := Mux(~en, 0.U, Mux(eol, Mux(eof, 0.U, vcount + 1.U), vcount))
  
  
  
  dpi.hsync := RegNext(~(en & hsync_act) ^ hsync_pol,          false.B)
  dpi.vsync := RegNext(~(en & vsync_act) ^ vsync_pol,          false.B)
  dpi.de    := RegNext(~(en & hcol_act & vlines_act) ^ de_pol, false.B)
  
  dpi.data.red    := RegNext(pix.bits.r, 0.U)
  dpi.data.blue   := RegNext(pix.bits.b, 0.U)
  dpi.data.green  := RegNext(pix.bits.g, 0.U)
  
  pix.ready := hcol_act & vlines_act
  
  
}

object DPIGen extends App{
  val verilog = (new ChiselStage).emitVerilog(
    new DPIDriver,

    //args
    Array("--target-dir", "output")
  )
}
