csv2vcf
=======

A Common Lisp program which converts contacts info recorded in CSV file to VCard file.

Currently only Simplified Chinese Names are supported.

Usage Example
=============

Prepare a CSV file
------------------

say there is a CSV file at e:/tmp/test.csv whose content is like:

>王小明,13912345678 13366778899,xiaoming.wang@example.com,贵州

Multiple phone numbers and email accounts separated by one space are supported.

Convert CSV to VCF
------------------

+ using quicklisp to load this system:

    (ql:quickload 'csv2vcf)

+ convert

    (csv2vcf:csv->vcf '(full-name mobile email work-address) "e:/tmp/test.csv")

+ the output VCF is under the same directory with the CSV file.

+ supported fields

    * supported fileds:
    * full-name
    * org
    * mobile
    * work-fixed-line
    * email
    * work-address
    * home-address
    * note
