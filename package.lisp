;;;; package.lisp

(defpackage #:csv2vcf
  (:use #:cl
        #:split-sequence
        #:flexi-streams
        #:cl-csv)
  (:export
   #:csv->vcf))

