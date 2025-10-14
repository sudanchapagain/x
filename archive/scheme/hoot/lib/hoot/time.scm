;;; Hoot date/time API
;;; Copyright (C) 2025 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Timestamps and dates.
;;;
;;; Timezone and locale handling is lacking, currently offering only
;;; the current locale's timezone.
;;;
;;; Code:

(library (hoot time)
  (export now
          utc-seconds->timestamp
          timestamp?
          timestamp-epoch-nanoseconds
          timestamp-epoch-microseconds
          timestamp-epoch-milliseconds
          timestamp-epoch-seconds

          timestamp->local-date-time
          date-time->string
          date-time?
          date-time-year
          date-time-month
          date-time-day
          date-time-weekday
          date-time-hour
          date-time-minute
          date-time-second
          date-time-millisecond
          date-time-zone-offset)
  (import (hoot errors)
          (hoot ffi)
          (hoot inline-wasm)
          (hoot match)
          (hoot numbers)
          (hoot ports)
          (hoot records)
          (hoot strings)
          (hoot syntax)
          (hoot values)
          (hoot write))

  ;; Kind of like Temporal.Instant.
  (define-record-type <timestamp>
    (make-timestamp epoch-nanoseconds)
    timestamp?
    (epoch-nanoseconds timestamp-epoch-nanoseconds))
  (define (timestamp-epoch-microseconds timestamp)
    (quotient (timestamp-epoch-nanoseconds timestamp) (expt 10 3)))
  (define (timestamp-epoch-milliseconds timestamp)
    (quotient (timestamp-epoch-nanoseconds timestamp) (expt 10 6)))
  (define (timestamp-epoch-seconds timestamp)
    (quotient (timestamp-epoch-nanoseconds timestamp) (expt 10 9)))
  (define (utc-seconds->timestamp seconds)
    (make-timestamp (exact (truncate (* seconds 1000000000)))))
  (define (now)
    (utc-seconds->timestamp
     (%inline-wasm '(func (result f64) (call $current-second)))))

  ;; This is an attempt to approximate the Temporal.ZonedDateTime type
  ;; in JS.  However, we're currently wrapping Date because that's
  ;; what's available in all browsers as the Temporal proposal is
  ;; still experimental.  One consequence of this is that we only get
  ;; millisecond resolution.
  (define-foreign %make-date
    "rt" "make_date"
    i32 i32 i32 i32 i32 i32 i32 -> (ref extern))
  (define-foreign %ms->date "rt" "ms_utc_to_date" f64 -> (ref extern))
  (define-foreign %date-year "rt" "date_year" (ref extern) -> i32)
  (define-foreign %date-month "rt" "date_month" (ref extern) -> i32)
  (define-foreign %date-day "rt" "date_day" (ref extern) -> i32)
  (define-foreign %date-weekday "rt" "date_weekday" (ref extern) -> i32)
  (define-foreign %date-hour "rt" "date_hour" (ref extern) -> i32)
  (define-foreign %date-min "rt" "date_min" (ref extern) -> i32)
  (define-foreign %date-sec "rt" "date_sec" (ref extern) -> i32)
  (define-foreign %date-ms "rt" "date_ms" (ref extern) -> i32)
  (define-foreign %date-timezone-offset
    "rt" "date_timezone_offset"
    (ref extern) -> i32)
  (define-foreign %date-locale
    "rt" "date_locale"
    (ref extern) -> (ref string))
  (define-foreign %date-locale-date
    "rt" "date_locale_date"
    (ref extern) -> (ref string))
  (define-foreign %date-locale-time
    "rt" "date_locale_time"
    (ref extern) -> (ref string))

  (define-external-type <date-time>
    date-time? wrap-date-time unwrap-date-time
    (lambda (dt port)
      (let ((d (unwrap-date-time dt)))
        (display "#<date-time year: " port)
        (display (%date-year d) port)
        (display " month: " port)
        (display (%date-month d) port)
        (display " day: " port)
        (display (%date-day d) port)
        (display " hour: " port)
        (display (%date-hour d) port)
        (display " minute: " port)
        (display (%date-min d) port)
        (display " second: " port)
        (display (%date-sec d) port)
        (display " millisecond: " port)
        (display (%date-ms d) port)
        (display " zone-offset: " port)
        (display (%date-timezone-offset d) port)
        (display ">" port))))

  (define-syntax-rule (define-date-time-getter name ref)
    (define (name date-time) (ref (unwrap-date-time date-time))))
  (define-date-time-getter date-time-year %date-year)
  (define-date-time-getter date-time-month %date-month)
  (define-date-time-getter date-time-day %date-day)
  (define-date-time-getter date-time-weekday %date-weekday)
  (define-date-time-getter date-time-hour %date-hour)
  (define-date-time-getter date-time-minute %date-min)
  (define-date-time-getter date-time-second %date-sec)
  (define-date-time-getter date-time-millisecond %date-ms)
  (define-date-time-getter date-time-zone-offset %date-timezone-offset)

  (define (timestamp->local-date-time timestamp)
    (wrap-date-time (%ms->date (timestamp-epoch-milliseconds timestamp))))

  ;; Taking a sexp approach for formatting, like irregex, rather than
  ;; string templating, like strftime.
  (define* (date-time->string date-time #:optional
                              (format '(locale-date " " locale-time)))
    (let ((port (open-output-string)))
      (define (print-string str)
        (display str port))
      ;; TODO: Support padding more than a single digit and with other
      ;; characters than #\0 such as #\space.
      (define* (print-int x #:optional pad?)
        (when (and pad? (< x 10))
          (display "0" port))
        (display x port))
      ;; TODO: 12h vs. 24h formatting.
      (let lp ((exp format) (pad? #f))
        (match exp
          (() (values))
          (('pad exp)
           (lp exp #t))
          (('join . exps)
           (let seq ((exps exps))
             (match exps
               (() (values))
               ((exp . rest)
                (lp exp pad?)
                (seq rest)))))
          ('locale
           (print-string (%date-locale (unwrap-date-time date-time))))
          ('locale-date
           (print-string (%date-locale-date (unwrap-date-time date-time))))
          ('locale-time
           (print-string (%date-locale-time (unwrap-date-time date-time))))
          ('year
           (print-int (date-time-year date-time)))
          ('month
           (print-int (1+ (date-time-month date-time)) pad?))
          ('day
           (print-int (date-time-day date-time) pad?))
          ('hour
           (print-int (date-time-hour date-time) pad?))
          ('minute
           (print-int (date-time-minute date-time) pad?))
          ('second
           (print-int (date-time-second date-time) pad?))
          ('millisecond
           (print-int (date-time-millisecond date-time)))
          ((? string? str)
           (print-string str))))
      (get-output-string port))))
