
.PHONY = clean builddir

BUILDDIR = ../build
OCAMLBUILD = ocamlbuild -build-dir $(BUILDDIR) -j 2 -classic-display

jsc: builddir
	$(OCAMLBUILD) jsc.native

lib: builddir
	$(OCAMLBUILD) ES5.otarget

clean:
	$(OCAMLBUILD) -clean

builddir:
	@mkdir $(BUILDDIR) 2> /dev/null || true

install: lib
	cd $(BUILDDIR) && ocamlfind install ES5 ../src/META ES5.cma ES5.cmxa ES5.a es5_*.cmi exprjs_syntax.cmi javaScript*.cmi prelude.cmi
