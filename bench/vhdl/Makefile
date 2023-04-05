GHDL ?= ghdl

OBJF = \
../../rtl/vhdl/T80_Reg.o \
../../rtl/vhdl/T80_MCode.o \
../../rtl/vhdl/T80_ALU.o \
../../rtl/vhdl/T80_Pack.o \
../../rtl/vhdl/T80.o \
../../rtl/vhdl/T80a.o \
../../rtl/vhdl/T16450.o \
../../../puart/ghdl_uart_pkg.o \
../../../puart/ghdl_uart.o \
ROM80.o \
SRAM.o \
AsyncLog.o \
AsyncStim.o \
Interactive.o \
StimLog.o \
TestBench.o
BINF = TestBench

GHDL_OPTS =
#GHDL_OPTS = --std=93

YOSYS_DATDIR = $(shell yosys-config --datdir)

all: $(BINF)
#all: $(BINF).ghw

%.o:%.vhd
	$(GHDL) -a $(GHDL_OPTS) $^

$(BINF): $(OBJF)
	$(GHDL) -e $(GHDL_OPTS) -Wl,ghdl_pty.c $@

$(BINF).ghw: $(BINF)
	$(GHDL) -r $^ --wave=$@

viewer: $(BINF).ghw
	gtkwave -S signals.tcl -f $(BINF).ghw

clean:
	- @rm $(BINF) $(BINF).ghw *.o work-obj93.cf
        
#verilog:
#       ghdl --synth --out=verilog my_and > my_and.v
#       ghdl --synth --out=verilog my_and_tb > my_and_tb.v

#dot:
#       yosys -m $(YOSYS_DATDIR)/plugins/ghdl.so -p 'ghdl my_and; write_json my_and.json'
#       netlistsvg my_and.json -o my_and.svg