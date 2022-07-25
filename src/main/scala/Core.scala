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

  val regfile     = Mem(32, UInt(WORD_LEN.W))
  val csr_regfile = Mem(4096, UInt(WORD_LEN.W))

  // Pipeline State Registers
  // format: off
  val id_reg_pc             = RegInit(0.U(WORD_LEN.W))
  val id_reg_inst           = RegInit(0.U(WORD_LEN.W))

  val exe_reg_pc            = RegInit(0.U(WORD_LEN.W))
  val exe_reg_wb_addr       = RegInit(0.U(ADDR_LEN.W))
  val exe_reg_op1_data      = RegInit(0.U(WORD_LEN.W))
  val exe_reg_op2_data      = RegInit(0.U(WORD_LEN.W))
  val exe_reg_rs2_data      = RegInit(0.U(WORD_LEN.W))
  val exe_reg_exe_fun       = RegInit(0.U(EXE_FUN_LEN.W))
  val exe_reg_mem_wen       = RegInit(0.U(MEN_LEN.W))
  val exe_reg_rf_wen        = RegInit(0.U(REN_LEN.W))
  val exe_reg_wb_sel        = RegInit(0.U(WB_SEL_LEN.W))
  val exe_reg_csr_addr      = RegInit(0.U(CSR_ADDR_LEN.W))
  val exe_reg_csr_cmd       = RegInit(0.U(CSR_LEN.W))
  val exe_reg_imm_i_sext    = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_s_sext    = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_b_sext    = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_u_shifted = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_z_uext    = RegInit(0.U(WORD_LEN.W))

  val mem_reg_pc            = RegInit(0.U(WORD_LEN.W))
  val mem_reg_wb_addr       = RegInit(0.U(ADDR_LEN.W))
  val mem_reg_op1_data      = RegInit(0.U(WORD_LEN.W))
  val mem_reg_rs2_data      = RegInit(0.U(WORD_LEN.W))
  val mem_reg_mem_wen       = RegInit(0.U(MEN_LEN.W))
  val mem_reg_rf_wen        = RegInit(0.U(REN_LEN.W))
  val mem_reg_wb_sel        = RegInit(0.U(WB_SEL_LEN.W))
  val mem_reg_csr_addr      = RegInit(0.U(CSR_ADDR_LEN.W))
  val mem_reg_csr_cmd       = RegInit(0.U(CSR_LEN.W))
  val mem_reg_imm_z_uext    = RegInit(0.U(WORD_LEN.W))
  val mem_reg_alu_out       = RegInit(0.U(WORD_LEN.W))

  val wb_reg_wb_addr        = RegInit(0.U(ADDR_LEN.W))
  val wb_reg_rf_wen         = RegInit(0.U(REN_LEN.W))
  val wb_reg_wb_data        = RegInit(0.U(WORD_LEN.W))
  // format: on

  // IF (Instruction Fetch)
  val if_reg_pc = RegInit(START_ADDR) // count up each cycles
  io.imem.addr := if_reg_pc
  val if_inst = io.imem.inst

  val exe_jmp_flg   = Wire(Bool())
  val exe_br_flg    = Wire(Bool())
  val exe_br_target = Wire(UInt(WORD_LEN.W))
  val exe_alu_out   = Wire(UInt(WORD_LEN.W))

  val if_pc_plus4 = if_reg_pc + 4.U(WORD_LEN.W)
  val if_pc_next = MuxCase(
    if_pc_plus4,
    Seq(
      exe_br_flg                       -> exe_br_target,
      exe_jmp_flg                      -> exe_alu_out,
      (if_inst === Instructions.ECALL) -> csr_regfile(0x305),
    )
  )
  if_reg_pc := if_pc_next

  // for next
  id_reg_pc   := if_reg_pc
  id_reg_inst := if_inst
  id_reg_inst := Mux((exe_br_flg || exe_jmp_flg), BUBBLE, if_inst)

  // ID (Instruction Decode)
  val id_inst     = Mux((exe_br_flg || exe_jmp_flg), BUBBLE, id_reg_inst)
  val id_rs1_addr = id_inst(19, 15)
  val id_rs2_addr = id_inst(24, 20)
  val id_wb_addr  = id_inst(11, 7)
  val id_rs1_data =
    Mux(id_rs1_addr =/= 0.U(WORD_LEN.W), regfile(id_rs1_addr), 0.U(WORD_LEN.W))
  val id_rs2_data =
    Mux(id_rs2_addr =/= 0.U(WORD_LEN.W), regfile(id_rs2_addr), 0.U(WORD_LEN.W))

  val id_imm_i      = id_inst(31, 20)
  val id_imm_i_sext = Cat(Fill(20, id_imm_i(11)), id_imm_i)

  val id_imm_s      = Cat(id_inst(31, 25), id_inst(11, 7))
  val id_imm_s_sext = Cat(Fill(20, id_imm_s(11)), id_imm_s)

  val id_imm_b = Cat(
    id_inst(31),
    id_inst(7),
    id_inst(30, 25),
    id_inst(11, 8)
  )
  val id_imm_b_sext = Cat(Fill(19, id_imm_b(11)), id_imm_b, 0.U(1.U))

  val id_imm_j = Cat(
    id_inst(31),
    id_inst(19, 12),
    id_inst(20),
    id_inst(30, 25),
    id_inst(24, 21)
  )
  val id_imm_j_sext = Cat(Fill(11, id_imm_j(19)), id_imm_j, 0.U(1.U))

  val id_imm_u         = Cat(id_inst(31, 12))
  val id_imm_u_shifted = Cat(id_imm_u, Fill(12, 0.U))

  val id_imm_z      = Cat(id_inst(19, 15))
  val id_imm_z_uext = Cat(Fill(27, 0.U), id_imm_z)

  // format: off
  val csignals = ListLookup(
    id_inst,
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

  val id_exe_fun :: id_op1_sel :: id_op2_sel :: id_mem_wen :: id_rf_wen :: id_wb_sel :: id_csr_cmd :: Nil =
    csignals

  val id_op1_data = MuxCase(
    0.U,
    Seq(
      (id_op1_sel === OP1_RS1) -> id_rs1_data,
      (id_op1_sel === OP1_PC)  -> id_reg_pc,
      (id_op1_sel === OP1_IMZ) -> id_imm_z_uext,
    )
  )

  val id_op2_data = MuxCase(
    0.U(WORD_LEN),
    Seq(
      (id_op2_sel === OP2_RS2) -> id_rs2_data,
      (id_op2_sel === OP2_IMI) -> id_imm_i_sext,
      (id_op2_sel === OP2_IMS) -> id_imm_s_sext,
      (id_op2_sel === OP2_IMJ) -> id_imm_j_sext,
      (id_op2_sel === OP2_IMU) -> id_imm_u_shifted,
    )
  )

  val id_csr_addr =
    Mux(id_csr_cmd === CSR_E, 0x342.U(CSR_ADDR_LEN.W), id_inst(31, 20))

  // for next
  exe_reg_pc            := id_reg_pc
  exe_reg_op1_data      := id_op1_data
  exe_reg_op2_data      := id_op2_data
  exe_reg_rs2_data      := id_rs2_data
  exe_reg_wb_addr       := id_wb_addr
  exe_reg_rf_wen        := id_rf_wen
  exe_reg_exe_fun       := id_exe_fun
  exe_reg_wb_sel        := id_wb_sel
  exe_reg_imm_i_sext    := id_imm_i_sext
  exe_reg_imm_s_sext    := id_imm_s_sext
  exe_reg_imm_b_sext    := id_imm_b_sext
  exe_reg_imm_u_shifted := id_imm_u_shifted
  exe_reg_imm_z_uext    := id_imm_z_uext
  exe_reg_csr_addr      := id_csr_addr
  exe_reg_csr_cmd       := id_csr_cmd
  exe_reg_mem_wen       := id_mem_wen

  // EX (EXecute)
  exe_alu_out := MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (exe_reg_exe_fun === ALU_ADD) -> (exe_reg_op1_data + exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_SUB) -> (exe_reg_op1_data - exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_AND) -> (exe_reg_op1_data & exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_OR)  -> (exe_reg_op1_data | exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_XOR) -> (exe_reg_op1_data ^ exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_SLL) -> (exe_reg_op1_data << exe_reg_op2_data(4, 0))(31, 0),
      (exe_reg_exe_fun === ALU_SRL) -> (exe_reg_op1_data >> exe_reg_op2_data(4, 0)).asUInt(),
      (exe_reg_exe_fun === ALU_SRA) -> (exe_reg_op1_data.asSInt() >> exe_reg_op2_data(4, 0)).asUInt,
      (exe_reg_exe_fun === ALU_SLT) -> (
        exe_reg_op1_data.asSInt() < exe_reg_op2_data.asSInt()
      ).asUInt(),
      (exe_reg_exe_fun === ALU_SLTU) -> (exe_reg_op1_data < exe_reg_op2_data).asUInt(),
      (exe_reg_exe_fun === ALU_JALR) -> ((exe_reg_op1_data + exe_reg_op2_data) &
        (~1.U(WORD_LEN.W))),
      (exe_reg_exe_fun === ALU_COPY1) -> exe_reg_op1_data,
    )
  )

  exe_br_flg := MuxCase(
    false.B,
    Seq(
      (exe_reg_exe_fun === BR_BEQ)  -> (exe_reg_op1_data === exe_reg_op1_data),
      (exe_reg_exe_fun === BR_BNE)  -> !(exe_reg_op1_data === exe_reg_op1_data),
      (exe_reg_exe_fun === BR_BLT)  -> (exe_reg_op1_data < exe_reg_op1_data),
      (exe_reg_exe_fun === BR_BGE)  -> !(exe_reg_op1_data < exe_reg_op1_data),
      (exe_reg_exe_fun === BR_BLTU) -> (exe_reg_op1_data.asSInt() < exe_reg_op1_data.asSInt()),
      (exe_reg_exe_fun === BR_BGEU) -> !(exe_reg_op1_data.asSInt() < exe_reg_op1_data.asSInt()),
    )
  )
  exe_br_target := exe_reg_pc + exe_reg_imm_b_sext
  exe_jmp_flg   := (exe_reg_wb_sel === WB_PC)

  // for next
  mem_reg_pc         := exe_reg_pc
  mem_reg_op1_data   := exe_reg_op1_data
  mem_reg_rs2_data   := exe_reg_rs2_data
  mem_reg_wb_addr    := exe_reg_wb_addr
  mem_reg_alu_out    := exe_alu_out
  mem_reg_rf_wen     := exe_reg_rf_wen
  mem_reg_wb_sel     := exe_reg_wb_sel
  mem_reg_csr_addr   := exe_reg_csr_addr
  mem_reg_csr_cmd    := exe_reg_csr_cmd
  mem_reg_imm_z_uext := exe_reg_imm_z_uext
  mem_reg_mem_wen    := exe_reg_mem_wen

  // MEM (MEMory access)
  io.dmem.addr := mem_reg_alu_out

  io.dmem.wen   := mem_reg_mem_wen
  io.dmem.wdata := mem_reg_rs2_data

  // CSR
  val csr_rdata = csr_regfile(mem_reg_csr_addr)
  val csr_wdata = MuxCase(
    0.U,
    Seq(
      (mem_reg_csr_cmd === CSR_W) -> mem_reg_op1_data,
      (mem_reg_csr_cmd === CSR_S) -> (csr_rdata | mem_reg_op1_data),
      (mem_reg_csr_cmd === CSR_C) -> (csr_rdata & ~mem_reg_op1_data),
      (mem_reg_csr_cmd === CSR_E) -> 11.U(WORD_LEN.W), // ECALL from machine mode
    )
  )

  when(mem_reg_csr_cmd > 0.U) {
    csr_regfile(mem_reg_csr_addr) := csr_wdata
  }

  val mem_wb_data = MuxCase(
    mem_reg_alu_out,
    Seq(
      (mem_reg_wb_sel === WB_MEM) -> io.dmem.rdata,
      (mem_reg_wb_sel === WB_PC)  -> (mem_reg_pc + 4.U(WORD_LEN.W)),
      (mem_reg_wb_sel === WB_CSR) -> csr_rdata
    )
  )

  // for next
  wb_reg_wb_addr := mem_reg_wb_addr
  wb_reg_rf_wen  := mem_reg_rf_wen
  wb_reg_wb_data := mem_wb_data

  // WB (Write Back)
  when(mem_reg_rf_wen === REN_S) {
    regfile(mem_reg_wb_addr) := wb_reg_wb_data
  }

  // debug
  io.gp   := regfile(3)
  io.exit := (if_inst === UNIMP)
  printf(p"if_reg_pc        : 0x${Hexadecimal(if_reg_pc)}\n")
  printf(p"id_reg_pc        : 0x${Hexadecimal(id_reg_pc)}\n")
  printf(p"id_reg_inst      : 0x${Hexadecimal(id_reg_inst)}\n")
  printf(p"id_inst          : 0x${Hexadecimal(id_inst)}\n")
  printf(p"exe_reg_pc       : 0x${Hexadecimal(exe_reg_pc)}\n")
  printf(p"exe_reg_op1_data : 0x${Hexadecimal(exe_reg_op1_data)}\n")
  printf(p"exe_reg_op2_data : 0x${Hexadecimal(exe_reg_op2_data)}\n")
  printf(p"exe_alu_out      : 0x${Hexadecimal(exe_alu_out)}\n")
  printf(p"mem_reg_pc       : 0x${Hexadecimal(mem_reg_pc)}\n")
  printf(p"mem_wb_data      : 0x${Hexadecimal(mem_wb_data)}\n")
  printf(p"wb_reg_wb_data   : 0x${Hexadecimal(wb_reg_wb_data)}\n")
  printf(p"--------------------\n")
}
