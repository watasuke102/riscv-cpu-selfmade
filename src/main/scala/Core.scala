package mycpu

import chisel3._
import chisel3.util._
import common._
import common.Consts._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new ImemPortIo())
    val dmem = Flipped(new DmemPortIo())
    val exit = Output(Bool())
  })

  val regfile = Mem(32, UInt(WORD_LEN.W))

  // IF (Instruction Fetch)
  val pc_reg = RegInit(START_ADDR) // count up each cycles
  pc_reg       := pc_reg + 4.U(WORD_LEN.W)
  io.imem.addr := pc_reg
  val inst = io.imem.inst

  // ID (Instruction Decode)
  val rs1_addr = inst(19, 15)
  val rs2_addr = inst(24, 20)
  val wb_addr  = inst(11, 7)
  val rs1_data =
    Mux(rs1_addr =/= 0.U(WORD_LEN.W), regfile(rs1_addr), 0.U(WORD_LEN.W))
  val rs2_data =
    Mux(rs2_addr =/= 0.U(WORD_LEN.W), regfile(rs2_addr), 0.U(WORD_LEN.W))

  val imm_i      = inst(31, 20)
  val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)

  val imm_s      = Cat(inst(31, 25), inst(11, 7))
  val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)

  // EX (EXecute)
  val alu_out = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (inst === Instructions.LW || inst === Instructions.ADDI) -> (rs1_data + imm_i_sext),
      (inst === Instructions.SW)  -> (rs1_data + imm_s_sext),
      (inst === Instructions.ADD) -> (rs1_data + rs2_data),
      (inst === Instructions.SUB) -> (rs1_data - rs2_data)
    )
  )

  // MEM (MEMory access)
  io.dmem.addr := alu_out

  io.dmem.wen   := (inst === Instructions.SW)
  io.dmem.wdata := rs2_data

  // WB (Write Back)
  val wb_data = MuxCase(
    alu_out,
    Seq(
      (inst === Instructions.LW) -> io.dmem.rdata
    )
  )
  when(
    inst === Instructions.LW || inst === Instructions.ADD || inst === Instructions.ADDI || inst === Instructions.SUB
  ) {
    regfile(wb_addr) := wb_data
  }

  // debug
  io.exit := (inst === 0x00602823.U(WORD_LEN.W))
  printf(p"pc_reg     : 0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst       : 0x${Hexadecimal(inst)}\n")
  printf(p"rs1_addr   :   ${rs1_addr}\n")
  printf(p"rs2_addr   :   ${rs2_addr}\n")
  printf(p"wb_addr    :   ${wb_addr}\n")
  printf(p"rs1_data   : 0x${Hexadecimal(rs1_data)}\n")
  printf(p"rs2_data   : 0x${Hexadecimal(rs2_data)}\n")
  printf(p"wb_data    : 0x${Hexadecimal(wb_data)}\n")
  printf(p"dmem.addr  :   ${io.dmem.addr}\n")
  printf(p"dmem.wen   :   ${io.dmem.wen}\n")
  printf(p"dmem.wdata : 0x${Hexadecimal(io.dmem.wdata)}\n")
  printf(p"--------------------\n")
}
