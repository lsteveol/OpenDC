package opendc

import chisel3._
import chiseltest._
import org.scalatest._

import flatspec._
import matchers._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
//import chiseltest.simulator.RequiresVerilator

class MultiplierSpec extends AnyFlatSpec with ChiselScalatestTester with should.Matchers{
  
  behavior of "mulitplier"
  
  val annos = Seq(VerilatorBackendAnnotation)
  //val annos = Seq()
  

  it should "test FrameLayerTest " /*taggedAs RequiresVerilator*/ in {
    implicit val p: Parameters = Parameters.empty
    
    test(LazyModule(new FrameLayerTest()(p.alterPartial({case MonitorsEnabled => false}))).module).withAnnotations(annos) { dut =>
      dut.blah.poke(false.B)
      
      
      dut.clock.setTimeout(0)
      for(i <- 0 until 100000){
        dut.clock.step()
      }
    }
  }
  
  
}
