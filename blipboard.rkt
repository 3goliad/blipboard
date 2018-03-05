#! /usr/bin/env racket
#lang racket/base

(require racket/cmdline)
(require "generate.rkt")

(define data-dir (make-parameter "./data/"))

(command-line
 #:program "blipboard"
 #:once-each
 [("--data" "-d") folder "Select the data directory (defaults to ./data/)" (data-dir folder)])

(define dummy-data '("Jose" "Miguel" "Antonio"))

(define start-sim (create-simulation #:names dummy-data))
(show-simulation start-sim)
(define end-sim (run-simulation start-sim))
(show-simulation end-sim)
