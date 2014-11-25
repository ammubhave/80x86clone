# This makefile is used to compile all the example c and assembly smips
# programs. To add a new assembly program, just include the assembly source
# in src/assembly/. To add a new c program, add the source in a folder within
# src/ and add the necessary target and rules to this makefile.
#
# To get the makefile to work, unpack smips-gcc.tar.gz in SMIPS_DIR. The smips
# compiler is a 32-bit executable so you may need 32-bit libraries for your
# computer to get it to work.

## Location of smips compiler in the course locker
# SMIPS_DIR=/mit/6.175/smips-gcc

## Alternate location if you want to install the compiler locally
## The compiler is in /mit/6.175/smips-gcc.tar.gz
# SMIPS_DIR=/home/$(USER)/tools/smips-gcc

CC=bcc -0 -W

# benchmarks:=median multiply_func multiply_inst qsort towers vvadd
# ku_benchmarks:=median multiply_func multiply_inst qsort towers vvadd kfunc
assembly:=$(patsubst src/assembly/%.S,%,$(wildcard src/assembly/*.S))

#assembly_o:=$(patsubst %,build/%.asm.o,$(assembly))

# benchmarks_exe:=$(patsubst %,build/%.bench.exe,$(benchmarks))
# ku_benchmarks_exe:=$(patsubst %,build/%.ku_bench.exe,$(ku_benchmarks))
assembly_exe:=$(patsubst %,build/%.asm.exe,$(assembly))

# benchmarks_dump:=$(patsubst %,build/%.bench.dump,$(benchmarks))
# ku_benchmarks_dump:=$(patsubst %,build/%.ku_bench.dump,$(ku_benchmarks))
assembly_dump:=$(patsubst %,build/%.asm.dump,$(assembly))

#  benchmarks_vmh:=$(patsubst %,build/%.bench.vmh,$(benchmarks))
# ku_benchmarks_vmh:=$(patsubst %,build/%.ku_bench.vmh,$(ku_benchmarks))
assembly_vmh:=$(patsubst %,build/%.asm.vmh,$(assembly))

# all: $(benchmarks_vmh) $(ku_benchmarks_vmh) $(assembly_vmh)
all: $(assembly_vmh)

# build/median.bench.exe: src/median/median.c src/median/median_main.c
# build/multiply_func.bench.exe: src/multiply_func/multiply.c src/multiply_func/multiply_main.c
# build/multiply_inst.bench.exe: src/multiply_inst/multiply_main.c
# build/qsort.bench.exe: src/qsort/qsort_main.c
# build/towers.bench.exe: src/towers/towers_main.c
# build/vvadd.bench.exe: src/vvadd/vvadd_main.c

# build/median.ku_bench.exe: src/median/median.c src/median/median_main.c
# build/multiply_func.ku_bench.exe: src/multiply_func/multiply.c src/multiply_func/multiply_main.c
# build/multiply_inst.ku_bench.exe: src/multiply_inst/multiply_main.c
# build/qsort.ku_bench.exe: src/qsort/qsort_main.c
# build/towers.ku_bench.exe: src/towers/towers_main.c
# build/vvadd.ku_bench.exe: src/vvadd/vvadd_main.c
# build/kfunc.ku_bench.exe: src/kfunc/kfunc_main.c

$(assembly_exe): build/%.asm.exe: src/assembly/%.S
	mkdir -p build
	as86 $^ -b $@

# $(assembly_exe): build/%.asm.exe: build/%.asm.o
#   ld86 -d -T ldscript $^ -o $@

# $(benchmarks_exe): build/%.bench.exe: lib/start.S lib/exceptions.S lib/print.c lib/cop.c
# 	mkdir -p build
# 	$(CC) $^ -o $@

# $(ku_benchmarks_exe): build/%.ku_bench.exe: ku_lib/kstart.S ku_lib/exceptions.S ku_lib/uprint.S ku_lib/ucop.S ku_lib/kprint.c ku_lib/kcop.c
# 	mkdir -p build
# 	$(CC) $^ -o $@

$(assembly_dump): %.dump: %.exe
	objdump --disassemble-all --disassemble-zeroes -Mintel -mi8086 -b binary $< > $@

%.vmh: %.exe
	echo "@400" > $@
	xxd -c 4 -p $^ >> $@

#	xxd -p $^ | sed -e 's/......../\0\n/g' | sed '/^$$/d' >> $@ 

#	./objdump2vmh.pl $< $@

clean:
	-rm -rf build

# .INTERMEDIATE: $(benchmarks_exe) $(ku_benchmarks_exe) $(assembly_exe)
