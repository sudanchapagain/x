;;; R7RS (scheme cxr) implementation
;;; Copyright (C) 2024 Igalia, S.L.
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
;;; R7RS (scheme cxr) implementation
;;;
;;; Code:

(library (scheme cxr)
  (export caaar cadar caadr caddr cdaar cddar cdadr cdddr
          caaaar caadar caaadr caaddr cadaar caddar cadadr cadddr
          cdaaar cdadar cdaadr cdaddr cddaar cdddar cddadr cddddr)
  (import (hoot pairs)))
