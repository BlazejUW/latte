latc_llvm: src/Latte/RunCompile 
	ln -sf src/Latte/RunCompile latc_llvm

src/Latte/RunCompile: 
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean
	rm -f latc_llvm

distclean:
	$(MAKE) -C src distclean
	rm -f latc_llvm