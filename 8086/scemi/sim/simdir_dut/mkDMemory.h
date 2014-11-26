/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Wed Nov 26 13:26:23 EST 2014
 * 
 */

/* Generation options: keep-fires */
#ifndef __mkDMemory_h__
#define __mkDMemory_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkDMemory module */
class MOD_mkDMemory : public Module {
 
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
  MOD_RegFile<tUInt32,tUInt32> INST_mem;
  MOD_Reg<tUInt8> INST_memInit_initialized;
 
 /* Constructor */
 public:
  MOD_mkDMemory(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUInt8 PORT_EN_req;
  tUInt8 PORT_EN_init_request_put;
  tUWide PORT_req_r;
  tUWide PORT_init_request_put;
  tUInt32 PORT_req;
  tUInt8 PORT_RDY_req;
  tUInt8 PORT_RDY_init_request_put;
  tUInt8 PORT_init_done;
  tUInt8 PORT_RDY_init_done;
 
 /* Publicly accessible definitions */
 public:
  tUInt8 DEF_WILL_FIRE_init_request_put;
  tUInt8 DEF_WILL_FIRE_req;
  tUInt8 DEF_CAN_FIRE_init_done;
  tUInt8 DEF_CAN_FIRE_init_request_put;
  tUInt8 DEF_CAN_FIRE_req;
  tUInt8 DEF_memInit_initialized__h311;
 
 /* Local definitions */
 private:
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  tUInt32 METH_req(tUWide ARG_req_r);
  tUInt8 METH_RDY_req();
  void METH_init_request_put(tUWide ARG_init_request_put);
  tUInt8 METH_RDY_init_request_put();
  tUInt8 METH_init_done();
  tUInt8 METH_RDY_init_done();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkDMemory &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkDMemory &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkDMemory &backing);
};

#endif /* ifndef __mkDMemory_h__ */
