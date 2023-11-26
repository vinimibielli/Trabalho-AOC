if {[file isdirectory work]} { vdel -all -lib work }
vlib work
vmap work work

vcom -work work MIPS-MC_SingleEdge.vhd
vcom -work work MIPS-MC_SingleEdge_tb.vhd
vcom -work work mult_div.vhd

vsim -voptargs=+acc=lprn -t ns work.tb

set StdArithNoWarnings 1
set StdVitalGlitchNoWarnings 1

do wave.do 

run 5000 ns