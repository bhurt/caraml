#    Caraml compiler
#    Copyright (C) 2012 Brian Hurt (bhurt@spnz.org)
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# The ordering of MLFILES is actually important- wrong orders cause
# link errors.
MLFILES = \
    Utils.ml \
    Info.ml \
    Error.ml \
    Type.ml \
    Common.ml \
    Config.ml \
    AST.ml \
    Parser.ml \
    Lexer.ml \
    Annot.ml \
    Alpha.ml \
    LambdaLift.ml \
    Simplify.ml \
    CallOpt.ml \
    LlvmIntf.ml \
    LlvmUtils.ml \
    Assembly.ml \
    caramlc.ml \
    make_apply.ml

MLIFILES = $(MLFILES:.ml=.mli)

CMIFILES = $(MLFILES:.ml=.cmi)
CMOFILES = $(MLFILES:.ml=.cmo)
CMXFILES = $(MLFILES:.ml=.cmx)
OFILES = $(MLFILES:.ml=.o)

OCAMLFIND = ocamlfind

PACKAGES =  camlp4,sexplib,sexplib.syntax,llvm,llvm.bitwriter,llvm.analysis

SYNTAX = -syntax camlp4o,sexp
OCAML_FLAGS = $(SYNTAX) -package $(PACKAGES) -warn-error +A

OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex
OCAMLC = $(OCAMLFIND) ocamlc $(OCAML_FLAGS)
OCAMLOPT = $(OCAMLFIND) ocamlopt $(OCAML_FLAGS)
OCAMLDEP = $(OCAMLFIND) ocamldep $(OCAML_IDIR) $(SYNTAX) -package $(PACKAGES)

OCAMLC_LIBS = 
OCAMLOPT_LIBS = 

all: caramlc caraml.bc

caramlc: $(CMXFILES)
	$(OCAMLOPT) -linkpkg -o $@ $(CMXFILES)

make_apply: Utils.cmx Type.cmx Common.cmx Config.cmx LlvmIntf.cmx LlvmUtils.cmx make_apply.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

$(CMIFILES): %.cmi: %.mli
	$(OCAMLC) -c $<

$(CMOFILES): %.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

$(CMXFILES): %.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<

$(OFILES): %.o: %.ml %.cmi
	$(OCAMLOPT) -c $<

Parser.ml Parser.mli: Parser.mly
	$(OCAMLYACC) -v $<

Lexer.ml: Lexer.mll
	$(OCAMLLEX) $<

repl:
	$(OCAMLFIND) ocamlmktop -o $@ -package findlib -linkpkg
	@echo "Remember to #use \"topfind\";; when starting the repl!"

%.bc: %.c
	clang -c -emit-llvm -o $@ $<

caraml_apply.bc: make_apply
	./make_apply

caraml.bc: gc.bc builtins.bc caraml_apply.bc
	llvm-ld -r -o $@ $^

-include make.deps

make.deps: Makefile $(MLFILES) $(MLIFILES)
	$(OCAMLDEP) $(MLFILES) $(MLIFILES) > $@

.PHONY: clean
clean:
	rm -f $(CMIFILES) 
	rm -f $(CMOFILES) 
	rm -f $(CMXFILES)
	rm -f $(OFILES)
	rm -f Lexer.ml Parser.ml Parser.mli make.deps repl
	rm -f make_apply caraml_apply.o caraml_apply.bc gc.bc builtins.bc caraml.bc

realclean: clean
	rm -f Parser.output caramlc make_apply

