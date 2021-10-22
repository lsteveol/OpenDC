package opendc

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.ChiselStage

class TestVideoGenerator extends MultiIOModule{
  val en        = IO(Input (Bool()))
  
  val col_width = IO(Input (UInt(16.W)))
  val color0    = IO(Input (UInt(24.W)))   
  val color1    = IO(Input (UInt(24.W)))
  val color2    = IO(Input (UInt(24.W)))
  val color3    = IO(Input (UInt(24.W)))
  
  val pix       = IO(Decoupled(new PixelPipeline))
  
  
  val count_in  = Wire(UInt(16.W))
  val count     = RegNext(count_in, 0.U)
  
  count_in      := Mux(~en, 0.U, Mux(pix.ready, Mux(count === col_width - 1.U, 0.U, count + 1.U), count))
  
  val color_sel = Wire(UInt(2.W))
  
  color_sel     := Mux(count <  (col_width >> 2), 0.U, 
                   Mux(count <  (col_width >> 1), 1.U, 
                   Mux(count < ((col_width >> 1) + (col_width >> 2)), 2.U, 3.U)))
  
  val r_sel     = MuxLookup(color_sel, "hdeadbeef".U, Seq(
                              0.U -> color0(23,16),
                              1.U -> color1(23,16),
                              2.U -> color2(23,16),
                              3.U -> color3(23,16)))
  
  val g_sel     = MuxLookup(color_sel, "hdeadbeef".U, Seq(
                              0.U -> color0(15,8),
                              1.U -> color1(15,8),
                              2.U -> color2(15,8),
                              3.U -> color3(15,8)))
  
  val b_sel     = MuxLookup(color_sel, "hdeadbeef".U, Seq(
                              0.U -> color0(7,0),
                              1.U -> color1(7,0),
                              2.U -> color2(7,0),
                              3.U -> color3(7,0)))
  
  pix.bits.a  := 0.U
  pix.bits.r  := r_sel
  pix.bits.g  := g_sel
  pix.bits.b  := b_sel
  pix.valid   := true.B
  
}


class TestLevel extends MultiIOModule{
  val en        = IO(Input (Bool()))
  val frame   = IO(new FrameConfig)
  
  val dpi     = IO(new DPIBundle)
  
  
  val tvg = Module(new TestVideoGenerator)
  tvg.en        := en
  tvg.col_width := frame.hcol
  tvg.color0    := "hffffff".U
  tvg.color1    := "h333333".U
  tvg.color2    := "haaaaaa".U
  tvg.color3    := "hbbbbbb".U
  
  
  val dpid = Module(new DPIDriver)
  dpid.en        := en
  dpid.hsync_pol := false.B
  dpid.vsync_pol := false.B
  dpid.de_pol    := false.B
  dpid.frame     := frame
  
  tvg.pix <> dpid.pix
  
  dpid.dpi <> dpi
}


object TestGen extends App{
  val verilog = (new ChiselStage).emitVerilog(
    new TestLevel,

    //args
    Array("--target-dir", "output")
  )
}
