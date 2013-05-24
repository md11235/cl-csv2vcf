;;;; csv2vcf.lisp

(in-package #:csv2vcf)

;;; "csv2vcf" goes here. Hacks and glory await!

;; "xing" is the PRC pinyin of the Hanzi for "family name".

(define-condition could-not-find-pinyin-of-hanzi (error)
  ((hanzi :initarg :hanzi :reader hanzi)))

(define-condition unsupported-field (error)
  ((name :initarg :name :reader name)))

(defvar *supported-vcard-fields* '(full-name
                                   org
                                   mobile
                                   work-fixed-line
                                   email
                                   work-address
                                   home-address
                                   note))

(defvar *hanzi->pinyin-alist-filename* nil "Chinese family name to pinyin mapping file")
(setq *hanzi->pinyin-alist-filename* "family-name-pinyin.txt")

(defvar *hash-xing->pinyin* nil "Chinese family name to pinyin mapping hash")

(defun load-xing-pinyin-pairs ()
  (setq *hash-xing->pinyin*
        (with-open-file (in (asdf:system-relative-pathname
                             'csv2vcf
                             *hanzi->pinyin-alist-filename*)
                            :direction :input
                            :external-format :utf-8)
          (let ((hash-xing->pinyin (make-hash-table))
                (alist-xing->pinyin (read in nil)))
            (mapcar #'(lambda (pair)
                        (setf (gethash (intern (car pair))
                                       hash-xing->pinyin)
                              (cdr pair)))
                    alist-xing->pinyin)
            hash-xing->pinyin))))

(defun xing->pinyin (hanzi)
  (gethash (intern hanzi) *hash-xing->pinyin*))

(defun parse-csv-line->alist (fields csv-line &key note org)
  (let ((result (list (cons 'org org)
                      (cons 'note note))))
    (mapcar #'(lambda (field value)
                (or (member field *supported-vcard-fields*)
                    (error 'unsupported-field
                           :name field))
                (if (> (length value) 0)
                    (case field
                      ((mobile work-fixed-line email) (push (cons field
                                                             (split-sequence:split-sequence #\  value))
                                                        result))
                      (t (push (cons field value)
                               result)))))
            fields
            (split-sequence:split-sequence #\, csv-line))
    result))

(defun parse-csv-file->alists (fields csv-filepath section-mark &key org note)
  (let ((section-mark-length (length section-mark))
        (results '())
        note-2)
    (with-open-file (input-stream csv-filepath
                                  :direction :input
                                  :external-format :utf-8)
      (loop for line = (read-line input-stream nil 'foo)
         until (eq line 'foo)
         do (if (and section-mark
                     (string-equal (subseq line 0 section-mark-length)
                                   section-mark))
                (setq note-2 (string-right-trim ","
                                                (subseq line section-mark-length)))
                (push (parse-csv-line->alist fields
                                             line
                                             :note (or note-2
                                                       note)
                                             :org org)
                      results))))
    (reverse results)))

(defun get-vcf-field-value (field alist)
  (cdr (assoc field alist)))


(defun collect-xing-pinyin-pairs ()
  (let ((existing-pairs '()))
    (maphash #'(lambda (key value)
                 (push (cons key value)
                       existing-pairs))
             *hash-xing->pinyin*)
    (reverse existing-pairs)))

(defun add-xing-pinyin-pair (xing pinyin)
  (if (null (assoc (intern xing) (collect-xing-pinyin-pairs)))
      (setf (gethash (intern xing)
                     *hash-xing->pinyin*)
            pinyin)))

(defun dump-xing-pinyin-pairs ()
  (with-open-file (dumped-file (asdf:system-relative-pathname
                                'csv2vcf
                                *hanzi->pinyin-alist-filename*)
                               :direction :output
                               :external-format :utf-8
                               :if-exists :overwrite)
    (let ((pairs (collect-xing-pinyin-pairs)))
      (format dumped-file "~A~%" "(")
      (loop for pair in pairs
         do (format dumped-file "(\"~A\" . \"~A\")~%" (car pair) (cdr pair)))
      (format dumped-file "~A~%" ")")
      (length pairs))))

;; todo: move the xing->pinyin into parse-csv-line->alist
;; to separate logic from presentation.
(defun alist->vcf3.0-format (alist out-stream)
  (format t "Processing: ~A~%" (get-vcf-field-value 'full-name alist))
  
  (format out-stream
          "BEGIN:VCARD~%VERSION:3.0")
  (let ((full-name (get-vcf-field-value 'full-name alist)))
    (let ((family-name-2 (subseq full-name 0 2))
          (family-name-1 (subseq full-name 0 1)))
      (let ((code (char-code (char family-name-1 0)
                             ;; (coerce family-name-1
                             ;;         'character)
                             )))
        (if (> code 255)
            (let ((pinyin-2 (xing->pinyin family-name-2))
                  (pinyin-1 (xing->pinyin family-name-1)))
              (restart-case
                  (progn
                    (if pinyin-2
                        (format out-stream
                                "~%N;CHARSET=UTF-8:~A;~A;;;"
                                family-name-2
                                (subseq full-name 2))
                        (if pinyin-1
                            (format out-stream
                                    "~%N;CHARSET=UTF-8:~A;~A;;;"
                                    family-name-1
                                    (subseq full-name 1))
                            (error 'could-not-find-pinyin-of-hanzi
                                   :hanzi (list family-name-1 family-name-2))))
                    (format out-stream "~%X-PHONETIC-LAST-NAME:~A" (string-upcase (or pinyin-2 pinyin-1))))
                (use-manual-value (family-name given-name pinyin)
                  :interactive read-manual-value
                  (format out-stream
                            "~%N;CHARSET=UTF-8:~A;~A;;;"
                            family-name
                            given-name)
                  (format out-stream "~%X-PHONETIC-LAST-NAME:~A" (string-upcase pinyin))
                  (add-xing-pinyin-pair family-name pinyin)))
              
              (format out-stream "~%FN;CHARSET=UTF-8:~A" full-name))
            ;; todo: support English full names
            ))))
  
  (let ((phone-numbers (get-vcf-field-value 'mobile alist)))
       (loop for number in phone-numbers
          do (format out-stream "~%TEL;TYPE=CELL:~A" number)))

  (let ((phone-numbers (get-vcf-field-value 'work-fixed-line alist)))
       (loop for number in phone-numbers
          do (format out-stream "~%TEL;TYPE=WORK:~A" number)))
  
  (let ((org (get-vcf-field-value 'org alist)))
    (if (> (length org) 0)
        (format out-stream "~%ORG;CHARSET=UTF-8:~A" org)))
  
  (let ((emails (get-vcf-field-value 'email
                                     alist)))
    (loop for email in emails
       do (format out-stream
                  "~%item1.EMAIL;TYPE=pref;TYPE=INTERNET:~A~%item1.X-ABLABEL:email"
                  email)))
  (let ((address (get-vcf-field-value 'work-address alist)))
    (if (> (length address) 0)
        (format out-stream "~%ADR;TYPE=WORK;CHARSET=UTF-8:;;~A" address)))

  (let ((address (get-vcf-field-value 'home-address alist)))
    (if (> (length address) 0)
        (format out-stream "~%ADR;TYPE=HOME;CHARSET=UTF-8:;;~A" address)))
  
  (let ((note (get-vcf-field-value 'note alist)))
    (if note
        (format out-stream "~%NOTE;CHARSET=UTF-8:~A" note)))
  (format out-stream "~%END:VCARD~%~%"))

(defun read-manual-value ()
  (let (family-name given-name pinyin)
    (format t "Input Family Name:")
    (setq family-name (read-line))
    (format t "Input Given Name:")
    (setq given-name (read-line))
    (format t "Input Pinyin for Family Name:")
    (setq pinyin (read-line))
    (list family-name
          given-name
          pinyin)))

;; the driver function
;; take /path/to/contacts.csv and output /path/to/contacts.vcf
;; supported fileds:
;; full-name
;; org
;; mobile
;; work-fixed-line
;; email
;; work-address
;; home-address
;; note
(defun csv->vcf (csv-filepath &key note org)
  (load-xing-pinyin-pairs)
  (let ((output-filepath (make-pathname :type "vcf"
                                        :defaults csv-filepath)))
    (with-open-file (output output-filepath
                            :direction :output
                            :external-format :utf-8
                            :if-exists :overwrite
                            )
      (let ((records (parse-csv-file->alists '(full-name mobile work-address)
                                             csv-filepath
                                             nil
                                             :note note
                                             :org org)))
        (loop for alist in records
           do (alist->vcf3.0-format alist output)))))
  (dump-xing-pinyin-pairs))
