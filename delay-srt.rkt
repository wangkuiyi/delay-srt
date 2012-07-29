;; Author: Yi Wang (yi.wang.2005@gmail.com)
;;
;; This program parses an .SRT subtitle file and increases/decreases delay.

#lang racket

(require racket/cmdline)

(provide parse-timestamp
         format-timestamp
         add-timestamp
         sub-timestamp
         parse-time-interval
         format-time-interval
         make-timestamp-change
         change-delay-of-srt-file)

;; Parse a string in the format of "hh:mm:ss:mil" into a timestamp.
;; A timestamp value is a list: '(milliseconds, seconds, minutes, hours).
(define (parse-timestamp string)
  (let* ((pattern "(\\d+):(\\d+):(\\d+),(\\d+)")
         (parsed (regexp-match (pregexp pattern) string)))
    (cond ((false? parsed) (raise "Failed parsing the delay time."))
          (else (reverse (map string->number (cdr parsed)))))))

;; Write a timestamp into SRT format string.
(define (format-timestamp s)
  (format "~a:~a:~a,~a"
          (list-ref s 3)
          (list-ref s 2)
          (list-ref s 1)
          (list-ref s 0)))

;; Add two timestamps.
(define (add-timestamp s1 s2)
  (let ((carry '(1000 60 60))) ; hours do not carry
    (let add-rec
        ((l1 s1) (l2 s2) (carry carry) (given 0) (result '()))
      (if (= (length l1) 1) ; we are processing hours
          (reverse (cons (+ (car l1) (car l2) given) result))
          (let ((cur-value (+ (car l1) (car l2) given)))
            (add-rec (cdr l1) (cdr l2) (cdr carry)
                     (quotient cur-value (car carry))
                     (cons (remainder cur-value (car carry)) result)))))))

;; Substract two timestamps.
(define (sub-timestamp s1 s2)
  (let ((carry '(1000 60 60)))
    (let sub-rec
        ((l1 s1) (l2 s2) (carry carry) (result '()))
      (if (= (length l1) 1)
          (reverse (cons (- (car l1) (car l2)) result))
          (let ((cur-value (- (car l1) (car l2))))
            (if (>= cur-value 0)
                (sub-rec (cdr l1) (cdr l2) (cdr carry) (cons cur-value result))
                (sub-rec (cons (- (cadr l1) 1) (cddr l1)) (cdr l2) (cdr carry)
                         (cons (+ cur-value (car carry)) result))))))))

;; Parse time interval as two timestamps.
;; Returns either a list of two timestamps or #f.
(define (parse-time-interval line)
  (let* ((pattern (string-append "^"
                                 "(\\d+:\\d+:\\d+,\\d+)"
                                 " +--> +"
                                 "(\\d+:\\d+:\\d+,\\d+)"
                                 "$"))
         (matched (regexp-match (pregexp pattern) line)))
    (cond ((false? matched) #f)
          (else (list (parse-timestamp (list-ref matched 1))
                      (parse-timestamp (list-ref matched 2)))))))

;; Write a time interval into SRT format string.
(define (format-time-interval interval)
  (format "~a --> ~a"
          (format-timestamp (car interval))
          (format-timestamp (cadr interval))))

;; Given a symbol of 'increase or 'decrease, returns a lambda which
;; increase or decrease delay to its timestamp parameter.
(define (make-timestamp-change change delay)
  (cond ((symbol=? change 'increase)
         (lambda (s) (add-timestamp s delay)))
        ((symbol=? change 'decrease)
         (lambda (s) (sub-timestamp s delay)))
        (else (raise "Unknown change keyword."))))

;; Parse an SRT file, change its time interval lines, outputs to port.
;; The file MUST be UTF-8 encoded.
(define (change-delay-of-srt-file in out change)
  (for ((line (in-lines in)))
    (let ((interval (parse-time-interval line)))
      (fprintf out "~a\n"
               (if (false? interval)
                   line
                   (format-time-interval
                    (map change interval)))))))

;; Define command line flags as parameters.
(define make-delay (make-parameter
                    (make-timestamp-change
                     'increase (parse-timestamp "0:0:0,0"))))

;; Using the racket/cmdline package to parse command line flags.
(define srt-file-to-delay
  (command-line
   #:program "compiler"
   #:once-any
   (("-i" "--increase-delay") inc
    "Increase the delay to the given SRT file, with format hh:mm:ss,mill"
    (make-delay (make-timestamp-change 'increase (parse-timestamp inc))))
   (("-d" "--decrease-delay") dec
    "Decrease the delay to the given SRT file"
    (make-delay (make-timestamp-change 'decrease (parse-timestamp dec))))
   #:args (filename)    ; expect one command-line argument: <filename>
                        ; return the argument as a filename to compile
   filename))

;; Parse the process the SRT file.
(call-with-input-file srt-file-to-delay
  (lambda (in)
    (change-delay-of-srt-file in (current-output-port) (make-delay))))
