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
    Info.ml \
    Error.ml \
    Type.ml \
    Common.ml \
    AST.ml \
	Parser.ml \
    Lexer.ml \
    Annot.ml \
    Alpha.ml \
    LambdaLift.ml \
    CallOpt.ml \
    caraml.ml

MLIFILES = $(MLFILES:.ml=.mli)

CMIFILES = $(MLFILES:.ml=.cmi)
CMOFILES = $(MLFILES:.ml=.cmo)
CMXFILES = $(MLFILES:.ml=.cmx)
OFILES = $(MLFILES:.ml=.o)

OCAMLFIND = ocamlfind

PACKAGES =  camlp4,sexplib,sexplib.syntax

OCAML_IDIR = -I /usr/local/lib/ocaml
SYNTAX = -syntax camlp4o
OCAML_FLAGS = -cc g++ $(OCAML_IDIR) $(SYNTAX) -package $(PACKAGES)

OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex
OCAMLC = $(OCAMLFIND) ocamlc $(OCAML_FLAGS)
OCAMLOPT = $(OCAMLFIND) ocamlopt $(OCAML_FLAGS)
OCAMLDEP = $(OCAMLFIND) ocamldep $(OCAML_IDIR) $(SYNTAX) -package $(PACKAGES)

OCAMLC_LIBS = 
OCAMLOPT_LIBS = 

all: caraml

caraml: $(CMOFILES)
	$(OCAMLC) -linkpkg -o $@ $(CMOFILES)

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

realclean: clean
	rm -f Parser.output caraml

