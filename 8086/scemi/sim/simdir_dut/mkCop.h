/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Wed Nov 26 13:26:23 EST 2014
 * 
 */

/* Generation options: keep-fires */
#ifndef __mkCop_h__
#define __mkCop_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkCop module */
class MOD_mkCop : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_Reg<tUInt32> INST_causeReg;
  MOD_Reg<tUInt8> INST_copFifo_clearReq_ehrReg;
  MOD_Wire<tUInt8> INST_copFifo_clearReq_ignored_wires_0;
  MOD_Wire<tUInt8> INST_copFifo_clearReq_ignored_wires_1;
  MOD_Reg<tUInt8> INST_copFifo_clearReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_copFifo_clearReq_virtual_reg_1;
  MOD_Wire<tUInt8> INST_copFifo_clearReq_wires_0;
  MOD_Wire<tUInt8> INST_copFifo_clearReq_wires_1;
  MOD_Reg<tUInt64> INST_copFifo_data_0;
  MOD_Reg<tUInt64> INST_copFifo_data_1;
  MOD_Reg<tUInt8> INST_copFifo_deqP;
  MOD_Reg<tUInt8> INST_copFifo_deqReq_ehrReg;
  MOD_Wire<tUInt8> INST_copFifo_deqReq_ignored_wires_0;
  MOD_Wire<tUInt8> INST_copFifo_deqReq_ignored_wires_1;
  MOD_Wire<tUInt8> INST_copFifo_deqReq_ignored_wires_2;
  MOD_Reg<tUInt8> INST_copFifo_deqReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_copFifo_deqReq_virtual_reg_1;
  MOD_Reg<tUInt8> INST_copFifo_deqReq_virtual_reg_2;
  MOD_Wire<tUInt8> INST_copFifo_deqReq_wires_0;
  MOD_Wire<tUInt8> INST_copFifo_deqReq_wires_1;
  MOD_Wire<tUInt8> INST_copFifo_deqReq_wires_2;
  MOD_Reg<tUInt8> INST_copFifo_empty;
  MOD_Reg<tUInt8> INST_copFifo_enqP;
  MOD_Reg<tUInt64> INST_copFifo_enqReq_ehrReg;
  MOD_Wire<tUInt64> INST_copFifo_enqReq_ignored_wires_0;
  MOD_Wire<tUInt64> INST_copFifo_enqReq_ignored_wires_1;
  MOD_Wire<tUInt64> INST_copFifo_enqReq_ignored_wires_2;
  MOD_Reg<tUInt8> INST_copFifo_enqReq_virtual_reg_0;
  MOD_Reg<tUInt8> INST_copFifo_enqReq_virtual_reg_1;
  MOD_Reg<tUInt8> INST_copFifo_enqReq_virtual_reg_2;
  MOD_Wire<tUInt64> INST_copFifo_enqReq_wires_0;
  MOD_Wire<tUInt64> INST_copFifo_enqReq_wires_1;
  MOD_Wire<tUInt64> INST_copFifo_enqReq_wires_2;
  MOD_Reg<tUInt8> INST_copFifo_full;
  MOD_Reg<tUInt32> INST_cycles;
  MOD_Reg<tUInt32> INST_epcReg_ehrReg;
  MOD_Wire<tUInt32> INST_epcReg_ignored_wires_0;
  MOD_Wire<tUInt32> INST_epcReg_ignored_wires_1;
  MOD_Reg<tUInt8> INST_epcReg_virtual_reg_0;
  MOD_Reg<tUInt8> INST_epcReg_virtual_reg_1;
  MOD_Wire<tUInt32> INST_epcReg_wires_0;
  MOD_Wire<tUInt32> INST_epcReg_wires_1;
  MOD_Reg<tUInt32> INST_numInsts;
  MOD_Reg<tUInt8> INST_startReg;
  MOD_Reg<tUInt32> INST_statusReg_ehrReg;
  MOD_Wire<tUInt32> INST_statusReg_ignored_wires_0;
  MOD_Wire<tUInt32> INST_statusReg_ignored_wires_1;
  MOD_Reg<tUInt8> INST_statusReg_virtual_reg_0;
  MOD_Reg<tUInt8> INST_statusReg_virtual_reg_1;
  MOD_Wire<tUInt32> INST_statusReg_wires_0;
  MOD_Wire<tUInt32> INST_statusReg_wires_1;
 
 /* Constructor */
 public:
  MOD_mkCop(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUInt8 PORT_EN_cpuToHost;
  tUInt8 PORT_EN_start;
  tUInt8 PORT_EN_wr;
  tUInt8 PORT_EN_causeException;
  tUInt8 PORT_EN_returnFromException;
  tUInt8 PORT_rd_idx;
  tUInt32 PORT_wr_idx;
  tUInt32 PORT_wr_val;
  tUInt32 PORT_causeException_current_pc;
  tUInt8 PORT_causeException_cause;
  tUInt64 PORT_cpuToHost;
  tUInt8 PORT_RDY_cpuToHost;
  tUInt8 PORT_RDY_start;
  tUInt8 PORT_started;
  tUInt8 PORT_RDY_started;
  tUInt32 PORT_rd;
  tUInt8 PORT_RDY_rd;
  tUInt8 PORT_RDY_wr;
  tUInt8 PORT_RDY_causeException;
  tUInt8 PORT_RDY_returnFromException;
  tUInt32 PORT_getEPC;
  tUInt8 PORT_RDY_getEPC;
  tUInt8 PORT_isUserMode;
  tUInt8 PORT_RDY_isUserMode;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_WILL_FIRE_cpuToHost;
  tUInt8 DEF_CAN_FIRE_cpuToHost;
  tUInt8 DEF_WILL_FIRE_returnFromException;
  tUInt8 DEF_WILL_FIRE_causeException;
  tUInt8 DEF_WILL_FIRE_wr;
  tUInt8 DEF_WILL_FIRE_start;
  tUInt8 DEF_WILL_FIRE_RL_count;
  tUInt8 DEF_CAN_FIRE_RL_count;
  tUInt8 DEF_WILL_FIRE_RL_epcReg_canonicalize;
  tUInt8 DEF_CAN_FIRE_RL_epcReg_canonicalize;
  tUInt8 DEF_WILL_FIRE_RL_statusReg_canonicalize;
  tUInt8 DEF_CAN_FIRE_RL_statusReg_canonicalize;
  tUInt8 DEF_WILL_FIRE_RL_copFifo_canonicalize;
  tUInt8 DEF_CAN_FIRE_RL_copFifo_canonicalize;
  tUInt8 DEF_WILL_FIRE_RL_copFifo_clearReq_canonicalize;
  tUInt8 DEF_CAN_FIRE_RL_copFifo_clearReq_canonicalize;
  tUInt8 DEF_WILL_FIRE_RL_copFifo_deqReq_canonicalize;
  tUInt8 DEF_CAN_FIRE_RL_copFifo_deqReq_canonicalize;
  tUInt8 DEF_WILL_FIRE_RL_copFifo_enqReq_canonicalize;
  tUInt8 DEF_CAN_FIRE_RL_copFifo_enqReq_canonicalize;
  tUInt8 DEF_CAN_FIRE_isUserMode;
  tUInt8 DEF_CAN_FIRE_getEPC;
  tUInt8 DEF_CAN_FIRE_returnFromException;
  tUInt8 DEF_CAN_FIRE_causeException;
  tUInt8 DEF_copFifo_empty__h5662;
  tUInt8 DEF_CAN_FIRE_wr;
  tUInt8 DEF_copFifo_full__h5630;
  tUInt8 DEF_CAN_FIRE_rd;
  tUInt8 DEF_CAN_FIRE_started;
  tUInt8 DEF_CAN_FIRE_start;
  tUInt8 DEF_startReg___d107;
 
 /* Local definitions */
 private:
  tUInt8 DEF_IF_copFifo_enqReq_wires_1_whas_THEN_copFifo_en_ETC___d13;
  tUInt8 DEF_IF_copFifo_clearReq_wires_0_whas__3_THEN_copFi_ETC___d46;
  tUInt64 DEF_copFifo_enqReq_wires_1_wget____d5;
  tUInt64 DEF_copFifo_enqReq_wires_0_wget____d8;
  tUInt64 DEF_copFifo_enqReq_ehrReg___d10;
  tUInt32 DEF_epcReg_ehrReg___d104;
  tUInt32 DEF_statusReg_ehrReg___d97;
  tUInt32 DEF__read__h6794;
  tUInt8 DEF_statusReg_virtual_reg_1_read____d137;
  tUInt8 DEF_copFifo_clearReq_wires_0_whas____d43;
  tUInt8 DEF_copFifo_clearReq_wires_0_wget____d44;
  tUInt8 DEF_copFifo_clearReq_ehrReg___d45;
  tUInt8 DEF_copFifo_deqReq_ehrReg___d37;
  tUInt8 DEF_copFifo_enqReq_wires_1_whas____d4;
  tUInt8 DEF_copFifo_enqReq_wires_0_whas____d7;
  tUInt8 DEF_x__h9819;
  tUInt64 DEF_copFifo_enqReq_ehrReg_0_BITS_36_TO_0___d25;
  tUInt32 DEF_IF_statusReg_virtual_reg_1_read__37_THEN_0_ELS_ETC___d138;
  tUInt8 DEF_copFifo_enqReq_ehrReg_0_BIT_37___d11;
  tUInt8 DEF_copFifo_enqReq_wires_0_wget_BIT_37___d9;
  tUInt8 DEF_copFifo_enqReq_wires_1_wget_BIT_37___d6;
  tUInt64 DEF_IF_copFifo_enqReq_wires_1_whas_THEN_copFifo_en_ETC___d27;
  tUInt8 DEF_IF_copFifo_enqReq_wires_1_whas_THEN_NOT_copFif_ETC___d20;
  tUInt32 DEF_IF_statusReg_wires_0_whas__5_THEN_statusReg_wi_ETC___d98;
  tUInt32 DEF_x__h10129;
  tUInt8 DEF_IF_copFifo_deqReq_wires_1_whas__3_THEN_copFifo_ETC___d39;
 
 /* Rules */
 public:
  void RL_copFifo_enqReq_canonicalize();
  void RL_copFifo_deqReq_canonicalize();
  void RL_copFifo_clearReq_canonicalize();
  void RL_copFifo_canonicalize();
  void RL_statusReg_canonicalize();
  void RL_epcReg_canonicalize();
  void RL_count();
 
 /* Methods */
 public:
  void METH_start();
  tUInt8 METH_RDY_start();
  tUInt8 METH_started();
  tUInt8 METH_RDY_started();
  tUInt32 METH_rd(tUInt8 ARG_rd_idx);
  tUInt8 METH_RDY_rd();
  void METH_wr(tUInt32 ARG_wr_idx, tUInt32 ARG_wr_val);
  tUInt8 METH_RDY_wr();
  tUInt64 METH_cpuToHost();
  tUInt8 METH_RDY_cpuToHost();
  void METH_causeException(tUInt32 ARG_causeException_current_pc, tUInt8 ARG_causeException_cause);
  tUInt8 METH_RDY_causeException();
  void METH_returnFromException();
  tUInt8 METH_RDY_returnFromException();
  tUInt32 METH_getEPC();
  tUInt8 METH_RDY_getEPC();
  tUInt8 METH_isUserMode();
  tUInt8 METH_RDY_isUserMode();
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkCop &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkCop &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkCop &backing);
};

#endif /* ifndef __mkCop_h__ */
