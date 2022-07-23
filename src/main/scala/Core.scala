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

  // format: off
  val csignals = ListLookup(
    inst,
    List(ALU_X, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
    Array(
      Instructions.LW    -> List(ALU_ADD,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM),
      Instructions.SW    -> List(ALU_ADD,  OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X),
                                           
      Instructions.ADD   -> List(ALU_ADD,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.ADDI  -> List(ALU_ADD,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      Instructions.SUB   -> List(ALU_SUB,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
                                           
      Instructions.AND   -> List(ALU_AND,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.ANDI  -> List(ALU_AND,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      Instructions.OR    -> List(ALU_OR,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.ORI   -> List(ALU_OR,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      Instructions.XOR   -> List(ALU_XOR,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.XORI  -> List(ALU_XOR,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
                                           
      Instructions.SLL   -> List(ALU_SLL,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.SRL   -> List(ALU_SRL,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.SRA   -> List(ALU_SRL,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.SLLI  -> List(ALU_SLL,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      Instructions.SRLI  -> List(ALU_SRL,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      Instructions.SRAI  -> List(ALU_SRL,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
                                           
      Instructions.SLT   -> List(ALU_SLT,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.SLTU  -> List(ALU_SLTU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      Instructions.SLTI  -> List(ALU_SLT,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      Instructions.SLTIU -> List(ALU_SLTU, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
    )
  )
  // format: on

  val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wen :: wb_sel :: Nil =
    csignals

  val op1_data = MuxCase(
    0.U,
    Seq(
      (op1_sel === OP1_RS1) -> rs1_data
    )
  )

  val op2_data = MuxCase(
    0.U(WORD_LEN),
    Seq(
      (op2_sel === OP2_RS2) -> rs2_data,
      (op2_sel === OP2_IMI) -> imm_i_sext,
      (op2_sel === OP2_IMS) -> imm_s_sext
    )
  )

  // EX (EXecute)
  val alu_out = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (exe_fun === ALU_ADD) -> (op1_data + op2_data),
      (exe_fun === ALU_SUB) -> (op1_data - op2_data),
      (exe_fun === ALU_AND) -> (op1_data & op2_data),
      (exe_fun === ALU_OR)  -> (op1_data | op2_data),
      (exe_fun === ALU_XOR) -> (op1_data ^ op2_data),
      (exe_fun === ALU_SLL) -> (op1_data << op2_data(4, 0))(31, 0),
      (exe_fun === ALU_SRL) -> (op1_data >> op2_data(4, 0)).asUInt(),
      (exe_fun === ALU_SRA) -> (op1_data.asSInt() >> op2_data(4, 0)).asUInt,
      (exe_fun === ALU_SLT) -> (op1_data.asSInt() < op2_data.asSInt()).asUInt(),
      (exe_fun === ALU_SLT) -> (op1_data < op2_data).asUInt(),
    )
  )

  // MEM (MEMory access)
  io.dmem.addr := alu_out

  io.dmem.wen   := mem_wen
  io.dmem.wdata := rs2_data

  // WB (Write Back)
  val wb_data = MuxCase(
    alu_out,
    Seq(
      (wb_sel === WB_MEM) -> io.dmem.rdata
    )
  )
  when(rf_wen === REN_S) {
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
