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

# Compiling with VERBOSE defined shows the actual commands.

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
    LambdaConv.ml \
    FreeBind.ml \
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

DEPFILES = $(MLFILES:.ml=.dep)

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

ifndef VERBOSE

# This turns echoing of all commands off
.SILENT:

# Testing MAKE_RESTARTS prevents the info message from being duplicated
# after we include the dependency files below.
ifndef MAKE_RESTARTS

# This just prints off a message to remind me of how to see the commands.
$(info Compiling in normal mode- run 'make VERBOSE=1' to see the commands)
endif

endif

caramlc: $(CMXFILES)
ifndef VERBOSE
	echo Compiling $@
endif
	$(OCAMLOPT) -linkpkg -o $@ $(CMXFILES)

make_apply: Utils.cmx Type.cmx Common.cmx Config.cmx LlvmIntf.cmx LlvmUtils.cmx make_apply.cmx
ifndef VERBOSE
	echo Linking $@
endif
	$(OCAMLOPT) -linkpkg -o $@ $^

$(CMIFILES): %.cmi: %.mli
ifndef VERBOSE
	echo Compiling $@
endif
	$(OCAMLC) -c $<

$(CMOFILES): %.cmo: %.ml %.cmi
ifndef VERBOSE
	echo Compiling $@
endif
	$(OCAMLC) -c $<

$(CMXFILES): %.cmx: %.ml %.cmi
ifndef VERBOSE
	echo Compiling $@
endif
	$(OCAMLOPT) -c $<

$(OFILES): %.o: %.ml %.cmi
ifndef VERBOSE
	echo Compiling $@
endif
	$(OCAMLOPT) -c $<

Parser.ml Parser.mli: Parser.mly
ifndef VERBOSE
	echo Generating $@
endif
	$(OCAMLYACC) -v $<

Lexer.ml: Lexer.mll
ifndef VERBOSE
	echo Generating $@
endif
	$(OCAMLLEX) $<

repl:
	$(OCAMLFIND) ocamlmktop -o $@ -package findlib -linkpkg
	@echo "Remember to #use \"topfind\";; when starting the repl!"

%.bc: %.c
ifndef VERBOSE
	echo Compiling $@
endif
	clang -c -emit-llvm -o $@ $<

caraml_apply.bc: make_apply
ifndef VERBOSE
	echo Generating $@
endif
	./make_apply

caraml.bc: gc.bc builtins.bc caraml_apply.bc
ifndef VERBOSE
	echo Linking $@
endif
	llvm-ld -r -o $@ $^

-include $(DEPFILES)

$(DEPFILES): %.dep: %.ml %.mli Makefile
ifndef VERBOSE
	echo Generating dependencies for $<
endif
	$(OCAMLDEP) $(@:.dep=.ml) $(@:.dep=.mli) > $@

.PHONY: clean
clean:
ifndef VERBOSE
	echo Cleaning up
endif
	rm -f $(CMIFILES) 
	rm -f $(CMOFILES) 
	rm -f $(CMXFILES)
	rm -f $(OFILES)
	rm -f $(DEPFILES)
	rm -f Lexer.ml Parser.ml Parser.mli repl
	rm -f make_apply caraml_apply.o caraml_apply.bc gc.bc builtins.bc caraml.bc

realclean: clean
	rm -f Parser.output caramlc make_apply

