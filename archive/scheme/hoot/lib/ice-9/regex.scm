;;; Regular expressions
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
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
;;; Regular expressions.
;;;
;;; Code:

(define-module (ice-9 regex)
  #:use-module ((hoot regexps)
                #:select (regexp-match?
                          regexp-match-string
                          regexp-match-start
                          regexp-match-end
                          regexp-match-count
                          regexp-match-substring
                          regexp-match-prefix
                          regexp-match-suffix))
  #:export (string-match)
  #:re-export (regexp-match?
               (regexp-match-string . match:string)
               (regexp-match-start . match:start)
               (regexp-match-end . match:end)
               (regexp-match-count . match:count)
               (regexp-match-substring . match:substring)
               (regexp-match-prefix . match:prefix)
               (regexp-match-suffix . match:suffix)))

(define* (string-match pattern str #:optional (start 0))
  (regexp-exec (make-regexp pattern) str start))
