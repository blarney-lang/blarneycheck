%.bin: %.hs
	blcc -i"./../../$*" $< -o $@
Top-Verilog/%.v: %.bin
	./$<
	#cp $@ Top-Verilog/Top.v ??

clean:
	rm -rf *.o *.hi *.bin Top-Verilog
