/*
 * Generated by Bluespec Compiler, version 2014.06.A (build 33987, 2014-06-24)
 * 
 * On Tue Nov 25 02:07:28 EST 2014
 * 
 */
#include "bluesim_primitives.h"
#include "mkIMemory.h"


/* Constructor */
MOD_mkIMemory::MOD_mkIMemory(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_mem(simHdl, "mem", this, 16u, 32u, 0u, 65535u),
    INST_memInit_initialized(simHdl, "memInit_initialized", this, 1u, (tUInt8)0u, (tUInt8)0u),
    PORT_RST_N((tUInt8)1u)
{
  PORT_EN_init_request_put = false;
  PORT_req_a = 0u;
  PORT_init_request_put.setSize(65u);
  PORT_init_request_put.clear();
  PORT_req = 0u;
  PORT_RDY_req = false;
  PORT_RDY_init_request_put = false;
  PORT_init_done = false;
  PORT_RDY_init_done = false;
  symbol_count = 15u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkIMemory::init_symbols_0()
{
  init_symbol(&symbols[0u], "CAN_FIRE_init_done", SYM_DEF, &DEF_CAN_FIRE_init_done, 1u);
  init_symbol(&symbols[1u], "CAN_FIRE_init_request_put", SYM_DEF, &DEF_CAN_FIRE_init_request_put, 1u);
  init_symbol(&symbols[2u], "CAN_FIRE_req", SYM_DEF, &DEF_CAN_FIRE_req, 1u);
  init_symbol(&symbols[3u], "EN_init_request_put", SYM_PORT, &PORT_EN_init_request_put, 1u);
  init_symbol(&symbols[4u], "init_done", SYM_PORT, &PORT_init_done, 1u);
  init_symbol(&symbols[5u], "init_request_put", SYM_PORT, &PORT_init_request_put, 65u);
  init_symbol(&symbols[6u], "mem", SYM_MODULE, &INST_mem);
  init_symbol(&symbols[7u], "memInit_initialized", SYM_MODULE, &INST_memInit_initialized);
  init_symbol(&symbols[8u], "memInit_initialized__h308", SYM_DEF, &DEF_memInit_initialized__h308, 1u);
  init_symbol(&symbols[9u], "RDY_init_done", SYM_PORT, &PORT_RDY_init_done, 1u);
  init_symbol(&symbols[10u], "RDY_init_request_put", SYM_PORT, &PORT_RDY_init_request_put, 1u);
  init_symbol(&symbols[11u], "RDY_req", SYM_PORT, &PORT_RDY_req, 1u);
  init_symbol(&symbols[12u], "req", SYM_PORT, &PORT_req, 32u);
  init_symbol(&symbols[13u], "req_a", SYM_PORT, &PORT_req_a, 32u);
  init_symbol(&symbols[14u],
	      "WILL_FIRE_init_request_put",
	      SYM_DEF,
	      &DEF_WILL_FIRE_init_request_put,
	      1u);
}


/* Rule actions */


/* Methods */

tUInt32 MOD_mkIMemory::METH_req(tUInt32 ARG_req_a)
{
  tUInt32 DEF_i__h310;
  PORT_req_a = ARG_req_a;
  DEF_i__h310 = (tUInt32)(65535u & (ARG_req_a >> 2u));
  PORT_req = INST_mem.METH_sub(DEF_i__h310);
  return PORT_req;
}

tUInt8 MOD_mkIMemory::METH_RDY_req()
{
  DEF_memInit_initialized__h308 = INST_memInit_initialized.METH_read();
  DEF_CAN_FIRE_req = DEF_memInit_initialized__h308;
  PORT_RDY_req = DEF_CAN_FIRE_req;
  return PORT_RDY_req;
}

void MOD_mkIMemory::METH_init_request_put(tUWide ARG_init_request_put)
{
  tUInt8 DEF_NOT_init_request_put_BIT_64___d4;
  tUInt8 DEF_init_request_put_BIT_64___d3;
  tUInt32 DEF_i__h374;
  tUInt32 DEF_x__h375;
  PORT_EN_init_request_put = (tUInt8)1u;
  DEF_WILL_FIRE_init_request_put = (tUInt8)1u;
  PORT_init_request_put = ARG_init_request_put;
  DEF_x__h375 = ARG_init_request_put.get_whole_word(0u);
  DEF_i__h374 = ARG_init_request_put.get_bits_in_word32(1u, 0u, 16u);
  DEF_init_request_put_BIT_64___d3 = ARG_init_request_put.get_bits_in_word8(2u, 0u, 1u);
  DEF_NOT_init_request_put_BIT_64___d4 = !DEF_init_request_put_BIT_64___d3;
  if (DEF_init_request_put_BIT_64___d3)
    INST_memInit_initialized.METH_write((tUInt8)1u);
  if (DEF_NOT_init_request_put_BIT_64___d4)
    INST_mem.METH_upd(DEF_i__h374, DEF_x__h375);
}

tUInt8 MOD_mkIMemory::METH_RDY_init_request_put()
{
  DEF_memInit_initialized__h308 = INST_memInit_initialized.METH_read();
  DEF_CAN_FIRE_init_request_put = !DEF_memInit_initialized__h308;
  PORT_RDY_init_request_put = DEF_CAN_FIRE_init_request_put;
  return PORT_RDY_init_request_put;
}

tUInt8 MOD_mkIMemory::METH_init_done()
{
  DEF_memInit_initialized__h308 = INST_memInit_initialized.METH_read();
  PORT_init_done = DEF_memInit_initialized__h308;
  return PORT_init_done;
}

tUInt8 MOD_mkIMemory::METH_RDY_init_done()
{
  DEF_CAN_FIRE_init_done = (tUInt8)1u;
  PORT_RDY_init_done = DEF_CAN_FIRE_init_done;
  return PORT_RDY_init_done;
}


/* Reset routines */

void MOD_mkIMemory::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_memInit_initialized.reset_RST(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkIMemory::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkIMemory::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_mem.dump_state(indent + 2u);
  INST_memInit_initialized.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkIMemory::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 16u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "CAN_FIRE_init_done", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "CAN_FIRE_init_request_put", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "CAN_FIRE_req", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "WILL_FIRE_init_request_put", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "memInit_initialized__h308", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "EN_init_request_put", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "RDY_init_done", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "RDY_init_request_put", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "RDY_req", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "init_done", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "init_request_put", 65u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "req", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "req_a", 32u);
  num = INST_mem.dump_VCD_defs(num);
  num = INST_memInit_initialized.dump_VCD_defs(num);
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkIMemory::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkIMemory &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
}

void MOD_mkIMemory::vcd_defs(tVCDDumpType dt, MOD_mkIMemory &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 65u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 32u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.DEF_CAN_FIRE_init_done) != DEF_CAN_FIRE_init_done)
      {
	vcd_write_val(sim_hdl, num, DEF_CAN_FIRE_init_done, 1u);
	backing.DEF_CAN_FIRE_init_done = DEF_CAN_FIRE_init_done;
      }
      ++num;
      if ((backing.DEF_CAN_FIRE_init_request_put) != DEF_CAN_FIRE_init_request_put)
      {
	vcd_write_val(sim_hdl, num, DEF_CAN_FIRE_init_request_put, 1u);
	backing.DEF_CAN_FIRE_init_request_put = DEF_CAN_FIRE_init_request_put;
      }
      ++num;
      if ((backing.DEF_CAN_FIRE_req) != DEF_CAN_FIRE_req)
      {
	vcd_write_val(sim_hdl, num, DEF_CAN_FIRE_req, 1u);
	backing.DEF_CAN_FIRE_req = DEF_CAN_FIRE_req;
      }
      ++num;
      if ((backing.PORT_RST_N) != PORT_RST_N)
      {
	vcd_write_val(sim_hdl, num, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
      }
      ++num;
      if ((backing.DEF_WILL_FIRE_init_request_put) != DEF_WILL_FIRE_init_request_put)
      {
	vcd_write_val(sim_hdl, num, DEF_WILL_FIRE_init_request_put, 1u);
	backing.DEF_WILL_FIRE_init_request_put = DEF_WILL_FIRE_init_request_put;
      }
      ++num;
      if ((backing.DEF_memInit_initialized__h308) != DEF_memInit_initialized__h308)
      {
	vcd_write_val(sim_hdl, num, DEF_memInit_initialized__h308, 1u);
	backing.DEF_memInit_initialized__h308 = DEF_memInit_initialized__h308;
      }
      ++num;
      if ((backing.PORT_EN_init_request_put) != PORT_EN_init_request_put)
      {
	vcd_write_val(sim_hdl, num, PORT_EN_init_request_put, 1u);
	backing.PORT_EN_init_request_put = PORT_EN_init_request_put;
      }
      ++num;
      if ((backing.PORT_RDY_init_done) != PORT_RDY_init_done)
      {
	vcd_write_val(sim_hdl, num, PORT_RDY_init_done, 1u);
	backing.PORT_RDY_init_done = PORT_RDY_init_done;
      }
      ++num;
      if ((backing.PORT_RDY_init_request_put) != PORT_RDY_init_request_put)
      {
	vcd_write_val(sim_hdl, num, PORT_RDY_init_request_put, 1u);
	backing.PORT_RDY_init_request_put = PORT_RDY_init_request_put;
      }
      ++num;
      if ((backing.PORT_RDY_req) != PORT_RDY_req)
      {
	vcd_write_val(sim_hdl, num, PORT_RDY_req, 1u);
	backing.PORT_RDY_req = PORT_RDY_req;
      }
      ++num;
      if ((backing.PORT_init_done) != PORT_init_done)
      {
	vcd_write_val(sim_hdl, num, PORT_init_done, 1u);
	backing.PORT_init_done = PORT_init_done;
      }
      ++num;
      if ((backing.PORT_init_request_put) != PORT_init_request_put)
      {
	vcd_write_val(sim_hdl, num, PORT_init_request_put, 65u);
	backing.PORT_init_request_put = PORT_init_request_put;
      }
      ++num;
      if ((backing.PORT_req) != PORT_req)
      {
	vcd_write_val(sim_hdl, num, PORT_req, 32u);
	backing.PORT_req = PORT_req;
      }
      ++num;
      if ((backing.PORT_req_a) != PORT_req_a)
      {
	vcd_write_val(sim_hdl, num, PORT_req_a, 32u);
	backing.PORT_req_a = PORT_req_a;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, DEF_CAN_FIRE_init_done, 1u);
      backing.DEF_CAN_FIRE_init_done = DEF_CAN_FIRE_init_done;
      vcd_write_val(sim_hdl, num++, DEF_CAN_FIRE_init_request_put, 1u);
      backing.DEF_CAN_FIRE_init_request_put = DEF_CAN_FIRE_init_request_put;
      vcd_write_val(sim_hdl, num++, DEF_CAN_FIRE_req, 1u);
      backing.DEF_CAN_FIRE_req = DEF_CAN_FIRE_req;
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
      vcd_write_val(sim_hdl, num++, DEF_WILL_FIRE_init_request_put, 1u);
      backing.DEF_WILL_FIRE_init_request_put = DEF_WILL_FIRE_init_request_put;
      vcd_write_val(sim_hdl, num++, DEF_memInit_initialized__h308, 1u);
      backing.DEF_memInit_initialized__h308 = DEF_memInit_initialized__h308;
      vcd_write_val(sim_hdl, num++, PORT_EN_init_request_put, 1u);
      backing.PORT_EN_init_request_put = PORT_EN_init_request_put;
      vcd_write_val(sim_hdl, num++, PORT_RDY_init_done, 1u);
      backing.PORT_RDY_init_done = PORT_RDY_init_done;
      vcd_write_val(sim_hdl, num++, PORT_RDY_init_request_put, 1u);
      backing.PORT_RDY_init_request_put = PORT_RDY_init_request_put;
      vcd_write_val(sim_hdl, num++, PORT_RDY_req, 1u);
      backing.PORT_RDY_req = PORT_RDY_req;
      vcd_write_val(sim_hdl, num++, PORT_init_done, 1u);
      backing.PORT_init_done = PORT_init_done;
      vcd_write_val(sim_hdl, num++, PORT_init_request_put, 65u);
      backing.PORT_init_request_put = PORT_init_request_put;
      vcd_write_val(sim_hdl, num++, PORT_req, 32u);
      backing.PORT_req = PORT_req;
      vcd_write_val(sim_hdl, num++, PORT_req_a, 32u);
      backing.PORT_req_a = PORT_req_a;
    }
}

void MOD_mkIMemory::vcd_prims(tVCDDumpType dt, MOD_mkIMemory &backing)
{
  INST_mem.dump_VCD(dt, backing.INST_mem);
  INST_memInit_initialized.dump_VCD(dt, backing.INST_memInit_initialized);
}