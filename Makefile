OCAMLBUILD = ocamlbuild -j 2 -classic-display
LIB_SRC = data/es5-lib.es5
LIB_CACHE = data/es5-lib.cache 
JSC_NATIVE = _build/src/jsc.native

.PHONY: all jsc lib install uninstall clean gen-cache clean-cache test

all: jsc lib gen-cache

jsc:
	$(OCAMLBUILD) src/jsc.native

lib:
	$(OCAMLBUILD) src/ES5.otarget

# must be root
install: lib
	cd _build/src/ && ocamlfind install ES5 ../../src/META ES5.cma ES5.cmxa ES5.a es5_*.cmi exprjs_syntax.cmi javaScript*.cmi prelude.cmi

# must be root
uninstall:
	ocamlfind remove ES5

clean: clean-cache
	$(OCAMLBUILD) -clean

$(LIB_CACHE): jsc $(LIB_SRC)
	$(JSC_NATIVE) -env $(LIB_SRC) -full-desugar -env-wc $@

gen-cache: $(LIB_CACHE)

clean-cache:
	rm -f r $(LIB_CACHE)

test: $(LIB_CACHE)
	cd tests/ES5conform/SimpleTestHarness/ && ./strict-tests.sh
