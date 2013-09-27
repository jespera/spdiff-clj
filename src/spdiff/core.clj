(ns spdiff.core
  (:require [clojure.zip :as zip])
  (:require [clojure.set :as set]))
;  (:gen-class))

; 1) representation of term: hiccup-style: [:type arguments]

(defn make-meta [name] [:meta name])

(defn meta? [term] (and (vector? term)
                        (= (first term) :meta)))

(defn eq [t1 t2] (= t1 t2))


(defn arity? [t1] (vector? t1))

(defn arity [t1] (count t1))

(defn lookup [env key] (get env key))

(defn add [env key val] (assoc env key val))

(defn zip2 [l1 l2] (partition 2 (interleave l1 l2)))

(defn- -merge-terms
  "Walk t1 and t2 in parallel and perform anti-unification"
  [env t1 t2]
  (cond (eq t1 t2) [t1 env]
        (and (arity? t1) (arity? t2)
             (= (arity t1) (arity t2))) (reduce (fn [[ts env'] [t1' t2']] 
                                                  (let [[tt env''] (-merge-terms env' t1' t2')]
                                                    [(conj ts tt) env'']))  
                                                [[] env] 
                                                (zip2 t1 t2))
        :else (if-let [meta-num (lookup env #{t1 t2})]
                [(make-meta meta-num) env]
                (let [next-id (inc (count env))
                      new-env (add env #{t1 t2} next-id)]
                  [(make-meta next-id) new-env]))))

;TODO: consider using parallel version of reduce?
(defn merge-terms
  "Generalization of -merge-terms to any number of terms"
  [torig & ts]
  (reduce (fn [tacc t] (first (-merge-terms {} tacc t))) torig ts))


;TODO: there must be a faster way to decide alpha-equivalence
(defn alpha-eq [t1 t2] (eq t1 t2)
  (let [merged (merge-terms t1 t2)]
    (and (eq merged (merge-terms t1 merged))
         (eq merged (merge-terms t2 merged)))))
    


; 2) given two terms, we need to be able to find the common patterns

(defn sub-terms
  [t]
  (let [zipper (zip/vector-zip t)]
    (loop [loc zipper
           acc-subs #{}]
      (if (zip/end? loc)
        acc-subs
        (recur (zip/next loc) (conj acc-subs (zip/node loc)))))))


(defn mk-diff [lhs rhs] {:lhs lhs :rhs rhs})
(defn get-lhs [diff] (diff :lhs))
(defn get-rhs [diff] (diff :rhs))

(defn comparable?
  [t1 t2]
  (and (arity? t1) (arity? t2)
       (= (arity t1) (arity t2))))

(defn tree-diff
  "Compute set of differences found in the given two trees"
  [orig-lhs orig-rhs]
  (loop [lhs (zip/vector-zip orig-lhs)
         rhs (zip/vector-zip orig-rhs)
         diffs #{}]
    (if (or (nil? lhs) (nil? rhs) 
            (zip/end? lhs) (zip/end? rhs))
      diffs
      (if (eq (zip/node lhs) (zip/node rhs))
        (recur (zip/right lhs) (zip/right rhs) diffs)
        (let [cur-diff (mk-diff (zip/node lhs) (zip/node rhs))
              lhs-node (zip/node lhs)
              rhs-node (zip/node rhs)]
          (if (comparable? lhs-node rhs-node)
            (->> (map tree-diff lhs-node rhs-node)
                 (reduce set/union (conj diffs cur-diff)))
            (recur (zip/right lhs) (zip/right rhs) 
                   (conj diffs cur-diff)))))))
)


(defn zip-end [zipper]
  (loop [loc zipper]
    (if (zip/end? loc)
      loc
      (recur (zip/next loc)))))

(defn next-or-up 
  [orig-zipper]
  (loop [loc orig-zipper]
    (if-let [r (zip/right loc)]
      r
      (if-let [up (zip/up loc)]
        (recur up)
        (zip-end loc)))))
      

(defn match-pattern-tree 
  "Match a given pattern with a given tree returning nil if no match
  and a binding environment if the pattern matched" 
  [orig-pattern orig-tree]
  (loop [pattern (zip/vector-zip orig-pattern)
         tree (zip/vector-zip orig-tree)
         env {}]
    (if (or (nil? pattern) (nil? tree)
            (zip/end? pattern) (zip/end? tree))
      env
      (let [pattern-node (zip/node pattern)
            tree-node (zip/node tree)]
        (if (meta? pattern-node)
          (if-let [bound (get env pattern-node)]
            (if (eq tree-node bound)
              (recur (next-or-up pattern) (next-or-up tree) env)
              nil)
            (recur (zip/right pattern) (zip/right tree) 
                   (assoc env pattern-node tree-node)))
          ; not meta
          (if (comparable? pattern-node tree-node)
            ; compare
            (recur (zip/next pattern) (zip/next tree) env)
            (if (eq pattern-node tree-node)
              (recur (next-or-up pattern) (next-or-up tree) env)
              nil)
            ))))))

(defn apply-bindings
  "Replace meta-vars in given term using bindings in given
  environment"
  [env t]
  (loop [loc (zip/vector-zip t)]
    (if (zip/end? loc)
      (zip/root loc)
      (let [node (zip/node loc)]
        (if (meta? node)
          (if-let [tree (get env node)]
            (recur (next-or-up (zip/replace loc tree))) 
            (recur (next-or-up loc)))
          (recur (zip/next loc)))))))
  

(defn tree-diff-apply
  "Apply a tree-diff to a given tree. Does not do replacements on inserted pieces."
  [diff t]
  (let [lhs (get-lhs diff)]
    (loop [loc (zip/vector-zip t)]
      (if (zip/end? loc)
        (zip/root loc)
        (if-let [bindings (match-pattern-tree lhs (zip/node loc))]
          (recur (next-or-up (zip/replace loc
                                          (apply-bindings bindings (get-rhs diff)))))
          (recur (zip/next loc)))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
