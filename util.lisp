(in-package #:csv2vcf)

(defun qp-encode (string)
  (with-output-to-string (result-string)
    (let ((octets (flexi-streams:string-to-octets string :external-format :utf-8)))
      (loop for index from 0 below (array-dimension octets 0)
         do (format result-string "=~X" (aref octets index))))))