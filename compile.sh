#!/bin/bash
rm -f giacoc functions.cmi functions.cmo main.cmi main.cmo lexer.cmi lexer.cmo lexer.ml parser.cmi parser.cmo parser.ml parser.mli

ocamlc -w -10 -c functions.ml
ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -w -10 -c parser.mli
ocamlc -w -10 -c lexer.ml
ocamlc -w -10 -c parser.ml
ocamlc -w -10 -c main.ml
ocamlc -w -10 -o giacoc functions.cmo str.cma lexer.cmo parser.cmo main.cmo
rm -f functions.cmi functions.cmo main.cmi main.cmo lexer.cmi lexer.cmo lexer.ml parser.cmi parser.cmo parser.ml parser.mli
exit
