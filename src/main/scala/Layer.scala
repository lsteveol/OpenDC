package opendc

import wav.common._

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.ChiselStage


import freechips.rocketchip.config._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

/**
  *   implicit clock will be the system clock
  */

object FrameLayerState extends ChiselEnum{
  val IDLE  = Value(0.U)
  val READ  = Value(1.U)
  val STALL = Value(2.U)
  val END   = Value(3.U)
}

class FrameLayerSettings extends Bundle{
  val en      = Input (Bool())
  val x       = Input (UInt(16.W))
  val y       = Input (UInt(16.W))
  val w       = Input (UInt(16.W))
  val h       = Input (UInt(16.W))
  
  //DMA Settings
  val addr_lo = Input (UInt(32.W))
  val addr_hi = Input (UInt(32.W))
  val size    = Input (UInt(8.W))
}

class FrameLayer(val dataWidth: Int = 32, val addrWidth: Int = 6, val tlSrc: Int = 0)(implicit p: Parameters) extends LazyModule{
  
  val node = TLClientNode(Seq(TLMasterPortParameters.v2(Seq(TLMasterParameters.v2(
    name = "frameLayer",
    emits = TLMasterToSlaveTransferSizes(
      get = TransferSizes(4,(dataWidth/8)),            
      putFull = TransferSizes(4,(dataWidth/8))))))))
  

  lazy val module = new LazyModuleImp(this) with RequireAsyncReset{
    val pix_clk           = IO(Input (Bool()))
    val pix_reset         = IO(Input (Bool()))
    val frameTime         = IO(Flipped(new FrameTiming))
    val settings          = IO(new FrameLayerSettings)
    
    val pix               = IO(Decoupled(new PixelPipeline))
    
    val swi_almost_empty  = IO(Input (UInt(addrWidth.W)))
    val swi_almost_full   = IO(Input (UInt(addrWidth.W)))
    
    val addr_in           = Wire(UInt(64.W))
    val addr              = RegNext(addr_in, 0.U)
    
    val end_addr          = Cat(settings.addr_hi, settings.addr_lo) + (MultiplierSingle(settings.w, settings.h) << 2)
    val next_addr         = addr + (1.U << settings.size)
    
    dontTouch(end_addr)
    dontTouch(next_addr)
    
    import FrameLayerState._
    
    val (tl, edge)  = node.out(0)
    val (_,  gbits) = edge.Get(tlSrc.asUInt, addr, settings.size)
    
    val rinc    = Wire(Bool())
    val fifo    = Module(new WavFIFO(dataWidth, addrWidth, "layer"))
    fifo.io.wclk   := clock.asBool
    fifo.io.wreset := reset.asBool
    fifo.io.winc   := tl.d.valid & tl.d.ready
    fifo.io.wdata  := tl.d.bits.data
    
    fifo.io.rclk   := pix_clk
    fifo.io.rreset := pix_reset
    fifo.io.rinc   := rinc
    
    
    fifo.io.swi_almost_empty := swi_almost_empty
    fifo.io.swi_almost_full  := swi_almost_full
    
    
    val nstate    = WireDefault(IDLE)
    val state     = RegNext(nstate, IDLE)
    val avalid_in = Wire(Bool())
    val avalid    = RegNext(avalid_in, false.B)
    
    nstate        := state
    avalid_in     := false.B
    addr_in       := addr
    
    tl.a.bits     := gbits
    
    when(state === IDLE){
      when(settings.en && frameTime.sof){                           //FIXME: Change to start on a certain line?
        addr_in     := Cat(settings.addr_hi, settings.addr_lo)
        avalid_in   := true.B
        nstate      := READ
      }
    }.elsewhen(state === READ){
      when(tl.a.ready){
        when(next_addr >= end_addr){
          nstate    := IDLE
        }.elsewhen(fifo.io.half_full){                                       //Change to settting
          avalid_in := false.B
          nstate    := STALL
        }.otherwise{
          addr_in   := next_addr
          avalid_in := true.B
          nstate    := READ
        }
      }
    }.elsewhen(state === STALL){
      when(~fifo.io.half_full){
        addr_in     := next_addr
        avalid_in   := true.B
        nstate      := READ
      }
    }.otherwise{
      nstate        := IDLE
    }
    
    tl.a.valid := avalid    
    tl.b.ready := false.B
    tl.c.valid := false.B
    tl.e.valid := false.B
    
    tl.d.ready := ~fifo.io.wfull
    
    //--------------------------------
    // Pixel Read
    //--------------------------------
    val pixel_time_valid  = ((frameTime.col_count  >= settings.x) &&
                             (frameTime.col_count  <= (settings.x + settings.w)) &&
                             (frameTime.line_count >= settings.y) &&
                             (frameTime.line_count <= (settings.y + settings.h)) &&
                             frameTime.hcol_act)
    
    val rdata_count_in    = Wire(UInt(log2Ceil(dataWidth/32).W))
    val rdata_count       = RegNext(rdata_count_in, 0.U)
    val rdata_count_max   = ((dataWidth/32) - 1).U
    val rdata_end         = rdata_count === rdata_count_max
    
    rdata_count_in        := Mux(pix.valid & pix.ready, Mux(rdata_end, 0.U, rdata_count + 1.U), rdata_count)
    
    
    val pixel_vec         = Wire(Vec(dataWidth/32, UInt(32.W)))
    for(i <- 0 until dataWidth/32){
      pixel_vec(i)        := Cat(fifo.io.rdata((i*32)+31, (i*32)+24),
                                 fifo.io.rdata((i*32)+23, (i*32)+16),
                                 fifo.io.rdata((i*32)+15, (i*32)+ 8),
                                 fifo.io.rdata((i*32)+ 7, (i*32)+ 0))
    }
    
    pix.valid           := ~fifo.io.rempty & pixel_time_valid
    pix.bits.a          := pixel_vec(rdata_count)(31,24)
    pix.bits.r          := pixel_vec(rdata_count)(23,16)
    pix.bits.g          := pixel_vec(rdata_count)(15, 8)
    pix.bits.b          := pixel_vec(rdata_count)( 7, 0)
    rinc                := pix.ready & pix.valid & rdata_end
    
  }  
}



class FrameLayerTest()(implicit p: Parameters) extends LazyModule{
  
  val dataWidth = 256
  
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0xffff)))
  
  val layer = LazyModule(new FrameLayer(dataWidth=dataWidth))
  
  ram.node := layer.node
  
  lazy val module = new LazyModuleImp(this) with RequireAsyncReset{
    val blah = IO(Input(Bool()))
    
    val timing = Module(new PixelTimingGenerator)
    timing.en   := ~reset.asBool
    timing.incr := ~reset.asBool
    timing.incr_amt := 0.U
    timing.frame.hsync := 5.U
    timing.frame.hbp   := 5.U
    timing.frame.hcol  := 10.U
    timing.frame.hfp   := 5.U
    
    timing.frame.vsync := 2.U
    timing.frame.vbp   := 2.U
    timing.frame.vlines:= 10.U
    timing.frame.vfp   := 3.U
    
    layer.module.frameTime <> timing.frameTime
    
    layer.module.pix_clk := clock.asBool
    layer.module.pix_reset := reset.asBool
    
    layer.module.settings.en  := ~reset.asBool
    layer.module.settings.x   := 0.U
    layer.module.settings.y     := 0.U
    layer.module.settings.w := 10.U
    layer.module.settings.h := 10.U
    layer.module.settings.addr_lo := 0.U
    layer.module.settings.addr_hi := 0.U
    layer.module.settings.size    := log2Ceil(dataWidth/8).U
    
    layer.module.swi_almost_empty := 0.U
    layer.module.swi_almost_full := 0.U
    
    layer.module.pix.ready := timing.frameTime.hcol_act & timing.frameTime.vlines_act
    
    
  }
}


object FrameLayerTestGen extends App{
  implicit val p: Parameters = Parameters.empty
  
  val verilog = (new ChiselStage).emitVerilog(
    LazyModule(new FrameLayerTest).module,

    //args
    Array("--target-dir", "output")
  )
}
