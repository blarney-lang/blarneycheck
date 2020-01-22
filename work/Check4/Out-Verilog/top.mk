all: top
top: *.v *.cpp
	verilator -cc top.v -exe top.cpp -o top -Wno-UNSIGNED -y $(BLARNEY_ROOT)/Verilog --x-assign unique --x-initial unique
	make -C obj_dir -j -f Vtop.mk top
	cp obj_dir/top .
	rm -rf obj_dir
.PHONY: clean clean-top
clean: clean-top
clean-top:
	rm -f top
