;; 
;; Calico - Scripting Environment
;; 
;; Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;; $Id: $

(define-syntax time
  [(time ?exp) (let ((start (current-time)))
      ?exp
      (printf "~s\n" (- (current-time) start)))])

(define fact1
    "Factorial for Scheme"
    (lambda (n accum)
        (if (= n 1)
            accum
            (fact1 (- n 1) (* n accum)))))

(define fact2
    "Factorial for Scheme"
    (lambda (n)
        (if (= n 1)
            1
            (* (fact2 (- n 1)) n))))

(define fact3
    (lambda (n)
        (apply * (range 1 (+ n 1)))))

(time (fact1 1000 1))
(time (fact2 1000))
(time (fact3 1000))
