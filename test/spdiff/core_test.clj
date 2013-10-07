(ns spdiff.core-test
  (:use clojure.test
        spdiff.core))

(def i-term [:int 42])
(def x-term [:var "x"])
(def f-term [:var "f"])
(def fx-term [:call f-term x-term])
(def g-term [:var "g"])
(def fgx-term [:call f-term g-term x-term])
(def gfx-term [:call g-term fx-term])

(def meta1-term (make-meta 1))
(def meta2-term (make-meta 2))

(def add-term [:add i-term f-term x-term])
(def addmm-term [:add meta1-term f-term meta1-term])

(def fm1-term [:call f-term meta1-term])
(def fm2-term [:call f-term meta2-term])
(def fmn-term [:call f-term meta1-term meta2-term])
(def fxn-term [:call f-term x-term meta2-term])
(def fmm-term [:call f-term meta1-term meta1-term])
(def fxx-term [:call f-term x-term x-term])
(def fxi-term [:call f-term x-term i-term])

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
  (is (= (set (sub-terms meta1-term)) #{meta1-term}))
  (is (= (set (sub-terms i-term)) #{i-term :int 42 }))
  (is (= (set (sub-terms f-term)) #{f-term :var "f"}))
  (is (= (set (sub-terms fx-term)) #{fx-term f-term "f" x-term "x" :var :call}))
  (is (= (set (sub-terms fm1-term)) #{fm1-term f-term meta1-term :call :var "f" }))
)

(deftest tree-diff-tests
  (is (= (tree-diff i-term i-term) #{}))
  (is (= (tree-diff i-term f-term) #{(mk-diff i-term f-term)
                                     (mk-diff :int :var)
                                     (mk-diff 42 "f")}))
  (is (= (tree-diff i-term fx-term) #{(mk-diff i-term fx-term)}))
  (is (= (tree-diff meta1-term meta2-term) #{(mk-diff meta1-term meta2-term)}))
  (is (= (tree-diff fm1-term fm2-term) (conj (tree-diff meta1-term meta2-term) 
                                             (mk-diff fm1-term fm2-term))))
)

(deftest match-tree-tests
  (is (not (nil? (match-pattern-tree i-term i-term))))
  (is (not (nil? (match-pattern-tree fx-term fx-term))))
  (is (not (nil? (match-pattern-tree fm1-term fx-term))))
  (is (not (nil? (match-pattern-tree fm2-term fx-term))))
  (is (not (nil? (match-pattern-tree fm1-term fm1-term))))
  (is (not (nil? (match-pattern-tree fm1-term fm2-term))))
  (is (not (nil? (match-pattern-tree fmn-term fgx-term))))
  (is (nil? (match-pattern-tree fmm-term fgx-term)))
)

(def env1 {meta1-term x-term})
(def envE {})

(deftest apply-bindings-test
  (is (= (apply-bindings env1 meta1-term) x-term))
  (is (= (apply-bindings env1 fm1-term) fx-term))
  (is (= (apply-bindings env1 fmm-term) fxx-term))
  (is (= (apply-bindings env1 fmn-term) fxn-term))
  (is (= (apply-bindings envE fmn-term) fmn-term))
)

(deftest anti-unify-eq
  (is (eq (anti-unify i-term i-term) i-term))
  (is (eq (anti-unify fx-term fx-term) fx-term))
  (is (alpha-eq (anti-unify i-term fx-term) meta1-term))
  (is (alpha-eq (anti-unify fmn-term fmm-term) fmn-term))
)