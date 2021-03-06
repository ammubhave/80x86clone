/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Wed Nov 26 13:26:23 EST 2014
 * 
 */

/* Generation options: keep-fires */
#ifndef __module_exec_h__
#define __module_exec_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the module_exec module */
class MOD_module_exec : public Module {
 
 /* Clock handles */
 private:
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
 
 /* Constructor */
 public:
  MOD_module_exec(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
 
 /* Port definitions */
 public:
  tUWide PORT_exec_dInst;
  tUInt32 PORT_exec_ip;
  tUInt32 PORT_exec_pc;
  tUWide PORT_exec;
  tUInt8 PORT_RDY_exec;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_CAN_FIRE_exec;
 
 /* Local definitions */
 private:
  tUWide DEF_exec_dInst_BIT_146_CONCAT_IF_exec_dInst_BIT_14_ETC___d724;
  tUWide DEF_exec_dInst_BIT_122_3_CONCAT_IF_exec_dInst_BIT__ETC___d723;
  tUWide DEF_IF_exec_dInst_BITS_251_TO_246_EQ_1_6_OR_exec_d_ETC___d722;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUWide METH_exec(tUWide ARG_exec_dInst, tUInt32 ARG_exec_ip, tUInt32 ARG_exec_pc);
  tUInt8 METH_RDY_exec();
 
 /* Reset routines */
 public:
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_exec &backing);
  void vcd_defs(tVCDDumpType dt, MOD_module_exec &backing);
};

#endif /* ifndef __module_exec_h__ */
