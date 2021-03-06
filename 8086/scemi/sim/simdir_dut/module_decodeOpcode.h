/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Wed Nov 26 13:26:23 EST 2014
 * 
 */

/* Generation options: keep-fires */
#ifndef __module_decodeOpcode_h__
#define __module_decodeOpcode_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the module_decodeOpcode module */
class MOD_module_decodeOpcode : public Module {
 
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
  MOD_module_decodeOpcode(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
 
 /* Port definitions */
 public:
  tUInt8 PORT_decodeOpcode_pInst;
  tUWide PORT_decodeOpcode;
  tUInt8 PORT_RDY_decodeOpcode;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_CAN_FIRE_decodeOpcode;
 
 /* Local definitions */
 private:
  tUWide DEF_IF_decodeOpcode_pInst_EQ_0x0_OR_decodeOpcode_p_ETC___d797;
  tUWide DEF_IF_decodeOpcode_pInst_EQ_0x0_OR_decodeOpcode_p_ETC___d796;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_NOT_decodeOpco_ETC___d795;
  tUWide DEF_NOT_decodeOpcode_pInst_EQ_0x0_84_AND_NOT_decod_ETC___d794;
  tUWide DEF__0_CONCAT_DONTCARE_90_CONCAT_DONTCARE_CONCAT_DO_ETC___d793;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_NOT_decodeOpco_ETC___d792;
  tUWide DEF_NOT_decodeOpcode_pInst_EQ_0x0_84_AND_NOT_decod_ETC___d791;
  tUWide DEF__0_CONCAT_DONTCARE_CONCAT_DONTCARE_CONCAT_DONTC_ETC___d790;
  tUWide DEF_DONTCARE_CONCAT_DONTCARE_CONCAT_DONTCARE_CONCA_ETC___d789;
  tUWide DEF_DONTCARE_CONCAT_decodeOpcode_pInst_EQ_0x0_OR_d_ETC___d788;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUWide METH_decodeOpcode(tUInt8 ARG_decodeOpcode_pInst);
  tUInt8 METH_RDY_decodeOpcode();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_decodeOpcode &backing);
  void vcd_defs(tVCDDumpType dt, MOD_module_decodeOpcode &backing);
};

#endif /* ifndef __module_decodeOpcode_h__ */
