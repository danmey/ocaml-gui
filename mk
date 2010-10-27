#!/bin/zsh

ocamlbuild -cflag -annot gui.native
for f in _build/**/*.annot;do mv $f $(basename $f); done