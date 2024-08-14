latc: src/Latte/RunCompile 
	ln -sf src/Latte/RunCompile latc

src/Latte/RunCompile: 
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean
	rm -f latc

distclean:
	$(MAKE) -C src distclean
	rm -f latc

tar: clean
	tar czf bp385954.tgz src/ lib/ Makefile README.MD latc_llvm optymalizacje_przyklady/