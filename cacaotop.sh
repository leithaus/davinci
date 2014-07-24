#! /bin/sh
ocamlmktop -I _build/src/main/ocaml/repl -o cacao Abscacao.cmo BNFC_Util.cmo Lexcacao.cmo Parcacao.cmo Printcacao.cmo Showcacao.cmo Cacao.cmo