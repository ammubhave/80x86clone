/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Wed Nov 26 13:26:23 EST 2014
 * 
 */

/* Generation options: keep-fires */
#ifndef __module_decodeRFToMemoryFetch_h__
#define __module_decodeRFToMemoryFetch_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the module_decodeRFToMemoryFetch module */
class MOD_module_decodeRFToMemoryFetch : public Module {
 
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
  MOD_module_decodeRFToMemoryFetch(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
 
 /* Port definitions */
 public:
  tUWide PORT_decodeRFToMemoryFetch_pdInst;
  tUWide PORT_decodeRFToMemoryFetch;
  tUInt8 PORT_RDY_decodeRFToMemoryFetch;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_CAN_FIRE_decodeRFToMemoryFetch;
 
 /* Local definitions */
 private:
  tUWide DEF_decodeRFToMemoryFetch_pdInst_BITS_105_TO_0___d264;
  tUWide DEF_decodeRFToMemoryFetch_pdInst_BIT_214_CONCAT_IF_ETC___d269;
  tUWide DEF_IF_decodeRFToMemoryFetch_pdInst_BITS_259_TO_25_ETC___d268;
  tUWide DEF_decodeRFToMemoryFetch_pdInst_BIT_168_22_CONCAT_ETC___d267;
  tUWide DEF_decodeRFToMemoryFetch_pdInst_BIT_146_29_CONCAT_ETC___d266;
  tUWide DEF_decodeRFToMemoryFetch_pdInst_BIT_122_61_CONCAT_ETC___d265;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUWide METH_decodeRFToMemoryFetch(tUWide ARG_decodeRFToMemoryFetch_pdInst);
  tUInt8 METH_RDY_decodeRFToMemoryFetch();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_decodeRFToMemoryFetch &backing);
  void vcd_defs(tVCDDumpType dt, MOD_module_decodeRFToMemoryFetch &backing);
};

#endif /* ifndef __module_decodeRFToMemoryFetch_h__ */
