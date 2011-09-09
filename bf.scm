#!/usr/bin/env guile
!#
(load "brainscriptlib.scm")
(bf-run (bf-get-program-from-file bf-file) bf-stack-size)
