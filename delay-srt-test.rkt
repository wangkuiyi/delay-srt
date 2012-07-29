;; Author: Yi Wang (yi.wang.2005@gmail.com)
;;

#lang racket/base

(require rackunit
         "sa.rkt")

(check-equal? (parse-timestamp "12:34:56,999")
              '(999 56 34 12))

(check-equal? (format-timestamp (parse-timestamp "12:34:56,999"))
              "12:34:56,999")

(check-equal? (add-timestamp (parse-timestamp "59:59:59,999")
                             (parse-timestamp "10000:1:1,1"))
              (parse-timestamp "10060:1:1,0"))

(check-equal? (sub-timestamp (parse-timestamp "10000:1:1,1")
                             (parse-timestamp "99:59:59,999"))
              (parse-timestamp "9900:1:1,2"))

(check-equal? (parse-time-interval "00:00:32,620 --> 00:00:35,490")
              (list (parse-timestamp "00:00:32,620")
                    (parse-timestamp "00:00:35,490")))

(check-equal? (parse-time-interval "whatever") #f)

(check-equal? (format-time-interval
               (parse-time-interval "00:00:32,620 --> 00:00:35,490"))
              "0:0:32,620 --> 0:0:35,490")

(check-equal? (let ((out (open-output-string)))
                (change-delay-of-srt-file
                 (open-input-string
                  (string-append "1\n"
                                 "00:00:00,000 --> 00:00:03,000\n"
                                 "■\n"
                                 "\n"
                                 "2\n"
                                 "00:00:04,640 --> 00:00:06,840\n"
                                 "本字幕由 YYeTs人人影视 原创翻译制作\n"
                                 "仅供学习 禁止用于任何商业盈利行为\n"
                                 " 更多影视更新 请登陆 www.YYeTs.net\n"))
                 out
                 (make-timestamp-change 'increase (parse-timestamp "1:1:1,999")))
                (get-output-string out))

              (string-append "1\n"
                             "1:1:1,999 --> 1:1:4,999\n"
                             "■\n"
                             "\n"
                             "2\n"
                             "1:1:6,639 --> 1:1:8,839\n"
                             "本字幕由 YYeTs人人影视 原创翻译制作\n"
                             "仅供学习 禁止用于任何商业盈利行为\n"
                             " 更多影视更新 请登陆 www.YYeTs.net\n"))
