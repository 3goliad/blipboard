#! /usr/bin/env racket
#lang racket/base

(require racket/cmdline)
(require racket/function)
(require "generate.rkt")

(command-line
 #:program "blipboard"
 #:once-any
 [("--size") size "Select the size (small, medium, large)" (gen-size size)])

(run-gen-stages
 (λ (stage-name stage-header stage-data)
   (with-output-to-file (string-append stage-name ".csv")
     (thunk (display stage-header)
            (for ([datum stage-data]) (display datum)))
     #:exists 'replace)))
