ifndef QUARTUS_ROOTDIR
  $(error Please set QUARTUS_ROOTDIR)
endif

.PHONY: one
one: 
	make -C Src
	qsys-generate -syn SoC.qsys
	quartus_sh --flow compile SoCKitTop.qpf

.PHONY: dse
dse:
	make -C Src
	qsys-generate -syn SoC.qsys
	"../../Temp/copy_bram_files.sh" &
	quartus_dse SoCKitTop.qpf    \
	    --num-seeds 15              \
	    --launcher local            \
	    --num-concurrent 4
.PHONY: report
report:
	quartus_dse SoCKitTop.qpf --report utilization
	quartus_dse SoCKitTop.qpf --report fmax_summary

.PHONY: download-sof
download-sof:
	quartus_pgm -m jtag -o "p;SoCKitTop.sof"

.PHONY: update-mif
update-mif:
	quartus_cdb --update_mif SoCKitTop.qpf
	quartus_asm SoCKitTop.qpf

.PHONY: clean
clean: clean-mif
	rm -f *.rpt *.msg *.summary *.sld *.sopcinfo *.jdi c5_pin_model_dump.txt
	rm -f *.pin *.done *.qws *.sof *.csv *.qws *.smsg *.qdf
	rm -rf dse* db incremental_db SoC .qsys_edit reconfig_mif SoC .qsys-edit

.PHONY: clean-mif
clean-mif:
	rm -f *.mif
	make -C Src clean
