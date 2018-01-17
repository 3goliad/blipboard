#! /usr/bin/env racket
#lang racket/base

(require racket/cmdline)
(require "generate.rkt")

(define arg
  (command-line
   #:program "blipboard"
   #:args (size)
   size))

(parameterize ([simulation-size arg])
  (generate-organizations)
  (define cycles (* 20 (case (simulation-size)
                         [("small") 10]
                         [("medium") 100]
                         [("large") 1000])))
  (with-output-to-file "out-1.csv"
    (lambda ()
      (print-header)
      (for ([i cycles])
        (print-worker (gen-worker empty-employment))
        (print-worker (gen-worker (gen-employment #f #f)))
        (print-worker (gen-worker (gen-employment #t #f)))))
    #:exists 'replace)
  (with-output-to-file "out-2.csv"
    (lambda ()
      (print-header)
      (for ([i cycles])
        (print-worker (gen-worker (gen-employment #f #t)))
        (print-worker (gen-worker (gen-employment #t #t)))))
    #:exists 'replace))
