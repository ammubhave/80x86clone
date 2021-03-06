/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Wed Nov 26 13:26:23 EST 2014
 * 
 */

/* Generation options: keep-fires */
#ifndef __mkSegRFile_h__
#define __mkSegRFile_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkSegRFile module */
class MOD_mkSegRFile : public Module {
 
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
  MOD_Reg<tUInt32> INST_rfile_0;
  MOD_Reg<tUInt32> INST_rfile_1;
  MOD_Reg<tUInt32> INST_rfile_2;
  MOD_Reg<tUInt32> INST_rfile_3;
 
 /* Constructor */
 public:
  MOD_mkSegRFile(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUInt8 PORT_EN_wr;
  tUInt8 PORT_wr_r;
  tUInt32 PORT_wr_data;
  tUInt8 PORT_rd1_r;
  tUInt8 PORT_rd2_r;
  tUInt8 PORT_rd3_r;
  tUInt8 PORT_RDY_wr;
  tUInt32 PORT_rd1;
  tUInt8 PORT_RDY_rd1;
  tUInt32 PORT_rd2;
  tUInt8 PORT_RDY_rd2;
  tUInt32 PORT_rd3;
  tUInt8 PORT_RDY_rd3;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_WILL_FIRE_wr;
  tUInt8 DEF_CAN_FIRE_wr;
  tUInt8 DEF_CAN_FIRE_rd3;
  tUInt8 DEF_CAN_FIRE_rd2;
  tUInt8 DEF_CAN_FIRE_rd1;
 
 /* Local definitions */
 private:
  tUInt32 DEF__read__h341;
  tUInt32 DEF__read__h301;
  tUInt32 DEF__read__h261;
  tUInt32 DEF__read__h221;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  void METH_wr(tUInt8 ARG_wr_r, tUInt32 ARG_wr_data);
  tUInt8 METH_RDY_wr();
  tUInt32 METH_rd1(tUInt8 ARG_rd1_r);
  tUInt8 METH_RDY_rd1();
  tUInt32 METH_rd2(tUInt8 ARG_rd2_r);
  tUInt8 METH_RDY_rd2();
  tUInt32 METH_rd3(tUInt8 ARG_rd3_r);
  tUInt8 METH_RDY_rd3();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkSegRFile &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkSegRFile &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkSegRFile &backing);
};

#endif /* ifndef __mkSegRFile_h__ */
