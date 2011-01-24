;; 
;; Pyjama - Scripting Environment
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

(define test
   (lambda ()
     (using "Graphics")
     (define win (Graphics.Window "Hello"))
     (define line (Graphics.Line (Graphics.Point 0 0)
                                 (Graphics.Point 100 100)))
     (line.draw win)
     (line.rotate 10)))

(define test2
   (lambda ()
      (using "Graphics")
      (let (win (Graphics.Window "Hello"))
           (line (Graphics.Line (Graphics.Point 0 0)
                                (Graphics.Point 100 100)))
         (line.draw win)
         (line.rotate 90))))
 