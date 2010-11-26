#!/bin/zsh

# ocamlbuild -cflag -annot $1.native
#ocamlbuild -quiet -cflags -I,+glMLite -libs png_loader.cma $1.native
ocamlbuild  $1.native
#ocamlbuild $1.top

#for f in _build/**/*.annot;do mv $f $(basename $f); done