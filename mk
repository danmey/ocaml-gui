#!/bin/zsh

# ocamlbuild -cflag -annot $1.native
ocamlbuild -use-ocamlfind -quiet -cflags -I,+glMLite -lflags -I,+glMLite,GL.cmxa,Glut.cmxa,Glu.cmxa,png_loader.cmxa $1.native
# ocamlbuild  $1.native
#ocamlbuild $1.top

#for f in _build/**/*.annot;do mv $f $(basename $f); done
