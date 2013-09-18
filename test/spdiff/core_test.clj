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
  (is (= (set (sub-terms meta1-term)) #{meta1-term :meta 1}))
  (is (= (set (sub-terms i-term)) #{i-term :int 42 }))
  (is (= (set (sub-terms f-term)) #{f-term :var "f"}))
  (is (= (set (sub-terms fx-term)) #{fx-term f-term "f" x-term "x" :var :call}))
  (is (= (set (sub-terms fm1-term)) #{fm1-term f-term meta1-term :call :var "f" :meta  1 }))
)

(deftest tree-diff-tests
  (is (= (tree-diff i-term i-term) #{}))
  (is (= (tree-diff i-term f-term) #{(mk-diff i-term f-term)
                                     (mk-diff :int :var)
                                     (mk-diff 42 "f")}))
  (is (= (tree-diff i-term fx-term) #{(mk-diff i-term fx-term)}))
  (is (= (tree-diff meta1-term meta2-term)
         #{(mk-diff 1 2) (mk-diff meta1-term meta2-term)}))
  (is (= (tree-diff fm1-term fm2-term) (conj (tree-diff meta1-term meta2-term) 
                                             (mk-diff fm1-term fm2-term))))
)