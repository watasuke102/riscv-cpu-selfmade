package mycpu

import chisel3._
import org.scalatest._
import chiseltest._

class HexTest extends FlatSpec with ChiselScalatestTester {
  "mycpu" should "work through hex" in {
    // c: instance of Top class
    val file = "src/riscv/rv32mi-p-scall.hex"
    test(new Top(file)) { c =>
      while (!c.io.exit.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.gp.expect(1.U)
    }
  }
}
