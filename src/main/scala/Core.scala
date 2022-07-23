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
    val gp   = Output(UInt(WORD_LEN.W))
  })

  val regfile = Mem(32, UInt(WORD_LEN.W))

  // IF (Instruction Fetch)
  val pc_reg = RegInit(START_ADDR) // count up each cycles
  io.imem.addr := pc_reg
  val inst    = io.imem.inst
  val jmp_flg = (inst === Instructions.JAL || inst === Instructions.JALR)
  val alu_out = Wire(UInt(WORD_LEN.W))

  val pc_plus4  = pc_reg + 4.U(WORD_LEN.W)
  val br_flg    = Wire(Bool())
  val br_target = Wire(UInt(WORD_LEN.W))
  val pc_next = MuxCase(
    pc_plus4,
    Seq(
      br_flg                        -> br_target,
      jmp_flg                       -> alu_out,
      (inst === Instructions.ECALL) -> csr_regfile(0x305),
    )
  )
  pc_reg := pc_next

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

  val imm_b      = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8))
  val imm_b_sext = Cat(Fill(19, imm_b(11)), imm_b, 0.U(1.U))

  val imm_j = Cat(inst(31), inst(19, 12), inst(20), inst(30, 25), inst(24, 21))
  val imm_j_sext = Cat(Fill(11, imm_j(19)), imm_j, 0.U(1.U))

  val imm_u         = Cat(inst(31, 12))
  val imm_u_shifted = Cat(imm_u, Fill(12, 0.U))

  val imm_z         = Cat(inst(19, 15))
  val imm_z_shifted = Cat(Fill(27, 0.U), imm_z)

  // format: off
  val csignals = ListLookup(
    inst,
    List(ALU_X, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
    Array(
      Instructions.LW     -> List(ALU_ADD,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM, CSR_X),
      Instructions.SW     -> List(ALU_ADD,   OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X,   CSR_X),
                                          
      Instructions.ADD    -> List(ALU_ADD,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.ADDI   -> List(ALU_ADD,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SUB    -> List(ALU_SUB,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
                                          
      Instructions.AND    -> List(ALU_AND,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.ANDI   -> List(ALU_AND,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.OR     -> List(ALU_OR,    OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.ORI    -> List(ALU_OR,    OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.XOR    -> List(ALU_XOR,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.XORI   -> List(ALU_XOR,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
                                         
      Instructions.SLL    -> List(ALU_SLL,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SRL    -> List(ALU_SRL,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SRA    -> List(ALU_SRL,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SLLI   -> List(ALU_SLL,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SRLI   -> List(ALU_SRL,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SRAI   -> List(ALU_SRL,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
                                       
      Instructions.SLT    -> List(ALU_SLT,   OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SLTU   -> List(ALU_SLTU,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SLTI   -> List(ALU_SLT,   OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.SLTIU  -> List(ALU_SLTU,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X), 

      Instructions.BEQ    -> List(BR_BEQ,    OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,   CSR_X),
      Instructions.BNE    -> List(BR_BNE,    OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,   CSR_X),
      Instructions.BGE    -> List(BR_BGE,    OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,   CSR_X),
      Instructions.BGEU   -> List(BR_BGEU,   OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,   CSR_X),
      Instructions.BLT    -> List(BR_BLT,    OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,   CSR_X),
      Instructions.BLTU   -> List(BR_BLTU,   OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,   CSR_X), 

      Instructions.JAL    -> List(ALU_ADD,   OP1_PC,  OP2_IMJ, MEN_X, REN_S, WB_PC,  CSR_X),
      Instructions.JALR   -> List(ALU_JALR,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC,  CSR_X),

      Instructions.LUI    -> List(ALU_ADD,   OP1_X,   OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),
      Instructions.AUIPC  -> List(ALU_ADD,   OP1_PC,  OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),

      Instructions.CSRRW  -> List(ALU_COPY1, OP1_RS1, OP2_X,   MEN_X, REN_S, WB_CSR, CSR_W),
      Instructions.CSRRWI -> List(ALU_COPY1, OP1_IMZ, OP2_X,   MEN_X, REN_S, WB_CSR, CSR_W),
      Instructions.CSRRS  -> List(ALU_COPY1, OP1_RS1, OP2_X,   MEN_X, REN_S, WB_CSR, CSR_S),
      Instructions.CSRRSI -> List(ALU_COPY1, OP1_IMZ, OP2_X,   MEN_X, REN_S, WB_CSR, CSR_S),
      Instructions.CSRRC  -> List(ALU_COPY1, OP1_RS1, OP2_X,   MEN_X, REN_S, WB_CSR, CSR_C),
      Instructions.CSRRCI -> List(ALU_COPY1, OP1_IMZ, OP2_X,   MEN_X, REN_S, WB_CSR, CSR_C),
      
      Instructions.ECALL  -> List(ALU_X,     OP1_X,   OP2_X,   MEN_X, REN_X, WB_X,   CSR_E),
    )
  )
  // format: on

  val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wen :: wb_sel :: csr_cmd :: Nil =
    csignals

  val op1_data = MuxCase(
    0.U,
    Seq(
      (op1_sel === OP1_RS1) -> rs1_data,
      (op1_sel === OP1_PC)  -> pc_reg,
    )
  )

  val op2_data = MuxCase(
    0.U(WORD_LEN),
    Seq(
      (op2_sel === OP2_RS2) -> rs2_data,
      (op2_sel === OP2_IMI) -> imm_i_sext,
      (op2_sel === OP2_IMS) -> imm_s_sext,
      (op2_sel === OP2_IMJ) -> imm_j_sext,
      (op2_sel === OP2_IMU) -> imm_u_shifted,
    )
  )

  // EX (EXecute)
  alu_out := MuxCase(
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
      (exe_fun === ALU_SLTU)  -> (op1_data < op2_data).asUInt(),
      (exe_fun === ALU_JALR)  -> ((op1_data + op2_data) & (~1.U(WORD_LEN.W))),
      (exe_fun === ALU_COPY1) -> op1_data,
    )
  )

  br_flg := MuxCase(
    false.B,
    Seq(
      (exe_fun === BR_BEQ)  -> (op1_data === op1_data),
      (exe_fun === BR_BNE)  -> !(op1_data === op1_data),
      (exe_fun === BR_BLT)  -> (op1_data < op1_data),
      (exe_fun === BR_BGE)  -> !(op1_data < op1_data),
      (exe_fun === BR_BLTU) -> (op1_data.asSInt() < op1_data.asSInt()),
      (exe_fun === BR_BGEU) -> !(op1_data.asSInt() < op1_data.asSInt()),
    )
  )
  br_target := pc_reg + imm_b_sext

  // MEM (MEMory access)
  io.dmem.addr := alu_out

  io.dmem.wen   := Mux(mem_wen === MEN_S, 1.U(WORD_LEN.W), 0.U(WORD_LEN.W))
  io.dmem.wdata := rs2_data

  // CSR
  val csr_regfile = Mem(4096, UInt(WORD_LEN.W))
  val csr_addr  = Mux(csr_cmd === CSR_E, 0x342.U(CSR_ADDR_LEN.W), inst(31, 20))
  val csr_rdata = csr_regfile(csr_addr)
  val csr_wdata = MuxCase(
    0.U,
    Seq(
      (csr_cmd === CSR_W) -> op1_data,
      (csr_cmd === CSR_S) -> (csr_rdata | op1_data),
      (csr_cmd === CSR_C) -> (csr_rdata & ~op1_data),
      (csr_cmd === CSR_E) -> 11.U(WORD_LEN.W), // ECALL from machine mode
    )
  )

  when(csr_cmd > 0.U) {
    csr_regfile(csr_addr) := csr_wdata
  }

  // WB (Write Back)
  val wb_data = MuxCase(
    alu_out,
    Seq(
      (wb_sel === WB_MEM) -> io.dmem.rdata,
      (wb_sel === WB_PC)  -> pc_plus4,
      (wb_sel === WB_CSR) -> csr_rdata
    )
  )
  when(rf_wen === REN_S) {
    regfile(wb_addr) := wb_data
  }

  // debug
  io.gp   := regfile(3)
  io.exit := (pc_reg === 0x44.U(WORD_LEN.W))
  printf(p"pc_reg     : 0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst       : 0x${Hexadecimal(inst)}\n")
  printf(p"gp         :   ${regfile(3)}\n")
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
