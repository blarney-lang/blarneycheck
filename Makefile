top:

.PHONY: clean
clean:
	rm -f Haskell/*.hi Haskell/*.o
	rm -f Haskell/Blarneycheck/*.hi Haskell/Blarneycheck/*.o
	make -C Examples clean

