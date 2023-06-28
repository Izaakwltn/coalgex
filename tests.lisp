;;;; tests.lisp
;;;;
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
