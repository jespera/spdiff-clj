(ns spdiff.core-test
  (:use clojure.test
        spdiff.core))

(def i-term [:int 42])
(def x-term [:var "x"])
(def f-term [:var "f"])
(def fx-term [:call f-term x-term])

(def meta1-term (make-meta 1))
(def meta2-term (make-meta 2))

(def fm1-term [:call f-term meta1-term])
(def fm2-term [:call f-term meta2-term])

(deftest merge-term-tests
  (is (eq i-term (merge-terms i-term)))
  (is (eq i-term (merge-terms i-term i-term)))
  (is (eq fx-term (merge-terms fx-term fx-term)))  
)

(deftest alpha-eq-tests
  (is (alpha-eq fx-term fx-term))
  (is (alpha-eq fm1-term fm2-term))
)


(deftest sub-terms-tests
  (is (= (set (sub-terms meta1-term)) #{meta1-term 1}))
  (is (= (set (sub-terms i-term)) #{i-term 42}))
  (is (= (set (sub-terms f-term)) #{f-term "f"}))
  (is (= (set (sub-terms fx-term)) #{fx-term f-term "f" x-term "x"}))
  (is (= (set (sub-terms fm1-term)) #{fm1-term f-term meta1-term}))
)