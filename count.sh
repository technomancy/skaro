#!/bin/bash

# sloccount doesn't know about these extensions
cp skaro.clj /tmp/skaro.lisp
cp skaro.rkt /tmp/skaro-rkt.scm

echo "Scheme:" $(sloccount skaro.scm | grep "of Code" | cut -f 2 -d =)
echo "Racket:" $(sloccount /tmp/skaro-rkt.scm | grep "of Code" | cut -f 2 -d =)
echo "Clojure:" $(sloccount /tmp/skaro.lisp | grep "of Code" | cut -f 2 -d =)
echo "Emacs Lisp:" $(sloccount skaro.el | grep "of Code" | cut -f 2 -d =)
echo "OCaml:" $(sloccount skaro.ml | grep "of Code" | cut -f 2 -d =)
