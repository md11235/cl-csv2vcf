;;;; package.lisp

(defpackage #:csv2vcf
  (:use #:cl #:split-sequence)
  (:export
   #:csv->vcf))

