#!/bin/bash
gen() {
    lang="$1"
    gambit rsc.scm -m --target "$lang" repl-max.scm && gambit rsc.scm -m --target "$lang" repl-min.scm
}

gen py
gen js
gen c
gen scm
