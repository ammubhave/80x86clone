/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Wed Nov 26 13:26:23 EST 2014
 * 
 */

/* Generation options: keep-fires */
#ifndef __module_decodeAddressingMode_h__
#define __module_decodeAddressingMode_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the module_decodeAddressingMode module */
class MOD_module_decodeAddressingMode : public Module {
 
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
  MOD_module_decodeAddressingMode(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
 
 /* Port definitions */
 public:
  tUWide PORT_decodeAddressingMode_pdInst;
  tUInt8 PORT_decodeAddressingMode_addressMode;
  tUWide PORT_decodeAddressingMode;
  tUInt8 PORT_RDY_decodeAddressingMode;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_CAN_FIRE_decodeAddressingMode;
 
 /* Local definitions */
 private:
  tUWide DEF_decodeAddressingMode_pdInst_BITS_105_TO_4___d394;
  tUWide DEF_decodeAddressingMode_pdInst_BITS_237_TO_220_CO_ETC___d407;
  tUWide DEF_IF_decodeAddressingMode_pdInst_BITS_259_TO_252_ETC___d406;
  tUWide DEF_decodeAddressingMode_pdInst_BITS_204_TO_188_23_ETC___d405;
  tUWide DEF_decodeAddressingMode_pdInst_BIT_168_28_CONCAT__ETC___d404;
  tUWide DEF_IF_decodeAddressingMode_pdInst_BITS_259_TO_252_ETC___d403;
  tUWide DEF_decodeAddressingMode_pdInst_BIT_122_91_CONCAT__ETC___d402;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUWide METH_decodeAddressingMode(tUWide ARG_decodeAddressingMode_pdInst,
				   tUInt8 ARG_decodeAddressingMode_addressMode);
  tUInt8 METH_RDY_decodeAddressingMode();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_module_decodeAddressingMode &backing);
  void vcd_defs(tVCDDumpType dt, MOD_module_decodeAddressingMode &backing);
};

#endif /* ifndef __module_decodeAddressingMode_h__ */
