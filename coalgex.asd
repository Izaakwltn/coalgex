;;;; This file is part of Coalgex.
;;;;
;;;; coalgex.asd
;;;;
;;;; Copyright Izaak Walton (c) 2023
;;;; 
;;;; Coalgex is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; Coalgex is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License along with Coalgex. If not, see <https://www.gnu.org/licenses/>. 
;;;;

(asdf:defsystem :coalgex
  :author "Izaak Walton <iwalton.ven@hrl.com"
  :description "A Simple Implementation of a Regex NFA in Coalton."
  :defsystem-depends-on (#:coalton)
  :depends-on (#:fiasco)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:coalton-file "parse")
                             #+ig(:coalton-file "nfa")
                             #+ig(:file "package")))))
