;;;; This file is part of Coalgex.
;;;;
;;;; tests.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2023
;;;; 
;;;; Coalgex is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; Coalgex is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License along with Coalgex. If not, see <https://www.gnu.org/licenses/>. 
;;;;

(fiasco:define-test-package #:coalgex-tests
  (:use #:fiasco
        #:coalgex))

(in-package #:coalgex-tests)

(deftest char-tests ()
  (is (rmatch (RChar "a") "a"))

  (is (not (rmatch (RChar "a") "b")))
)

(deftest cat-tests ()
    
  (is (rmatch (RCat (RChar "a") (RChar "b")) "ab"))

  (is (not (rmatch (RCat (RChar "a") (RChar "b")) "ac")))

  (is (rmatch (RCat (RStar "a") (RChar "b")) "aab")))

(deftest alt-tests ()

  (is (rmatch (RAlt (RChar "a") (RChar "b")) "a"))

  (is (rmatch (RAlt (RChar "a") (RChar "b")) "b"))

  (is (not (rmatch (RAlt (RChar "a") (RChar "b")) "c")))

  (is (rmatch (RAlt (RStar "a") (RStar "b")) "aaaa"))
  
  (is (rmatch (RAlt (RStar "a") (RStar "b")) "bbbbbbbbbbbbb"))
  
)

(deftest star-tests ()

  (is (rmatch (RStar "a") "a"))

  (is (rmatch (RStar "a") "aaaaaaaaaaaaaa"))

  )
