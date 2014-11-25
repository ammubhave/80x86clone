/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Tue Nov 25 02:07:28 EST 2014
 * 
 */

/* Generation options: keep-fires */
#ifndef __mkSceMiUInt32Parameter_h__
#define __mkSceMiUInt32Parameter_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkSceMiUInt32Parameter module */
class MOD_mkSceMiUInt32Parameter : public Module {
 
 /* Clock handles */
 private:
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
  tUInt32 const PARAM_n;
 
 /* Module state */
 public:
 
 /* Constructor */
 public:
  MOD_mkSceMiUInt32Parameter(tSimStateHdl simHdl, char const *name, Module *parent, tUInt32 ARG_n);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
 
 /* Port definitions */
 public:
  tUInt8 PORT_not_used;
  tUInt8 PORT_RDY_not_used;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_CAN_FIRE_not_used;
 
 /* Local definitions */
 private:
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUInt8 METH_not_used();
  tUInt8 METH_RDY_not_used();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkSceMiUInt32Parameter &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkSceMiUInt32Parameter &backing);
};

#endif /* ifndef __mkSceMiUInt32Parameter_h__ */
