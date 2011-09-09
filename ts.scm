#!/usr/bin/env guile
!#
(load "brainscriptlib.scm")
(bf-run (ts-get-program-from-file bf-file) bf-stack-size)
