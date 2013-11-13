(ns spdiff.core
  (:require [clojure.zip :as zip])
  (:require [clojure.set :as set])
  (:require [instaparse.core :as insta])
  (:require [clojure.math.combinatorics :as combo])
)

;  (:gen-class))

; 1) representation of term: hiccup-style: [:type arguments]

; TODO: we need to make sure a meta-term is atomic, or special case
; everywhere we traverse a tree to make sure we do not compare
; "contents" of meta var

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


(defn make-meta [name] {:meta name})

(defn meta? [term] (and (map? term)
                        (term :meta)))

(defn meta-val [term]
  (term :meta))

(defn metas
  "Get meta-vars used in term"
  [term]
  (loop [loc (zip/vector-zip term)
         res #{}]
    (cond (zip/end? loc) res
          (meta? (zip/node loc)) (recur (next-or-up loc) 
                                        (conj res (meta-val (zip/node loc))))
          :else (recur (zip/next loc) res))))

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


(defn fresh-meta
  "Return a fresh meta-term not bound in given env"
  [env]
  (->> (vals env)
       (map #(% :meta))
       (reduce max 0)
       (#(make-meta (inc %)))))


(defn remove-metas
  [env]
  (reduce (fn [acc [k v]] 
            (if (and (meta? k) (meta? v)) acc (assoc acc k v)))
          {}
          env))

(defn anti-unify
  "Construct anti-unifier for given two terms"
  [lhs-term rhs-term]
  (let [metas1 (metas lhs-term)
        metas2 (metas rhs-term)
        pre-metas (set/union metas1 metas2)] 
    (loop [lhs-loc (zip/vector-zip lhs-term)
           rhs-loc (zip/vector-zip rhs-term)
           env (reduce (fn [acc-env num] 
                         (assoc acc-env (make-meta num) (make-meta num)))
                       {} pre-metas)]
      (if (and (zip/end? lhs-loc)
               (zip/end? rhs-loc))
        [(zip/root rhs-loc) (remove-metas env)]
        (let [lhs-node (zip/node lhs-loc)
              rhs-node (zip/node rhs-loc)]
          (if (eq lhs-node rhs-node)
            (recur (next-or-up lhs-loc)
                   (next-or-up rhs-loc)
                   env)
            (if (comparable? lhs-node rhs-node)
              (recur (zip/next lhs-loc)
                     (zip/next rhs-loc)
                     env)
              (if-let [bound-meta (get env #{lhs-node rhs-node})]
                (recur (next-or-up lhs-loc)
                       (next-or-up (zip/replace rhs-loc bound-meta))
                       env)
                (let [new-meta (fresh-meta env)]
                  (recur (next-or-up lhs-loc)
                         (next-or-up (zip/replace rhs-loc new-meta))
                         (assoc env #{lhs-node rhs-node} new-meta)))))))))))
  

(defn merge-diffs
  "pt1 * pt2"
  [pt1 pt2]
  (let [l1 (get-lhs pt1)
        r1 (get-rhs pt1)
        l2 (get-lhs pt2)
        r2 (get-rhs pt2)
        [lhs lhs-env] (anti-unify l1 l2)
        [rhs rhs-env] (anti-unify r1 r2)]
    (mk-diff lhs (apply-bindings 
                  (keep (fn [[support lhs-meta]]
                          (when-let [rhs-meta (get rhs-env support)]
                            [rhs-meta lhs-meta])) lhs-env)
                  rhs))))
  ; if
  ; pt1 * pt2 = pt3
  ; then 
  ; a) pt1 ≼ (t1,t1') implies pt3 ≼ (t1,t1')
  ; b) pt2 ≼ (t2,t2') implies pt3 ≼ (t2,t2')
  ; and forall other pt4 satisfying a) and b)
  ; then pt3 ≼ pt4
  ;
  ; question: for term-diffs, can we implement pt1 * pt2 purely
  ; syntactically?
  ; f(a) -> f(a,a)
  ; f(b) -> f(a,b)
  ;   X{a,b}    Y{a,b}
  ; f(c) -> f(c,c)
  ; 
  ; f(X) -> f(a,X)
  ; Idea:
  ; given p1l -> p1r and p2l -> p2r construct
  ; p1l*p2l = p12l and
  ; p1r*p2r = p12r
  ; (postulate: for all X in metas(p12l) the support set is unique
  ; thus, provided that |metas(p12r)| <= |metas(p12l)| we should
  ; be able to construct p12r' where we 'reverse-substitute' the
  ; bindings found for p12l
  ; then we should satisfy the above!?!


; S[t,t'] = [{ pt | pt in diff(t,t'), pt ≼ (t,t')}, {[t,t']}]
; S1 * S2 = let  S1 = [pts1, pairs1]
;                S2 = [pts2, pairs2]
;                pts= {pt | pt1 ∈ pts1, pt2 ∈ pts2, pt = pt1*pt2, pt ≼ pairs1 u pairs2}
;           in   [pts, pairs1 u pairs2]

(def grammar 
  "exp  ::= num | var | call | exp '*' exp | exp '+' exp
   var  ::= #'[a-zA-Z]+[a-zA-Z0-9]*'
   num  ::= #'[0-9]+'
   call ::= var <'('> expCommaList? <')'>
   <expCommaList> ::= exp | exp <','> expCommaList
"
)

; metric, dist func:
; dist(x,y) >= 0   [non-negativity]
; dist(x,y) = 0 iff x = y
; dist(x,y) = dist(y,x)
; dist(x,z) <= d(x,y) + d(y,z)


(defn edit-dist-f
  [distf sizef l r]
  (cond (empty? l) (reduce + 0 (map sizef r))
        (empty? r) (reduce + 0 (map sizef l))
        :else (if (= (first l) (first r))
                (min (+ (sizef (first l))
                        (edit-dist-f distf sizef (rest l) r)) 
                     (+ (sizef (first r))
                        (edit-dist-f distf sizef l (rest r))) 
                     (edit-dist-f distf sizef (rest l) (rest r)))
                (min (+ (sizef (first l))
                        (edit-dist-f distf sizef (rest l) r)) 
                     (+ (sizef (first r))
                        (edit-dist-f distf sizef l (rest r)))
                     (+ (distf (first l) (first r)) 
                        (edit-dist-f distf sizef (rest l) (rest r)))
                     )
                )))

(defn edit-dist
  [l r] (edit-dist-f (fn [a b] 1) (fn [a] 1) l r))

(defn mk-eq [e] {:eq e})
(defn mk-rm [e] {:rm e})
(defn mk-add [e] {:add e})

(defn smallest-op
  [eq-cost rm-cost add-cost]
  (cond (and (<= eq-cost rm-cost) (<= eq-cost add-cost)) :eq
        (and (<= rm-cost add-cost)) :rm
        :else :add))


(defn edit-script
  [l r]
  (cond (empty? l) (map mk-add r)
        (empty? r) (map mk-rm l)
        :else (let [rm-cost  (edit-dist (rest l) r)
                    add-cost (edit-dist l (rest r))]
                (if (= (first l) (first r))
                  (let [eq-cost (edit-dist (rest l) (rest r))
                        small-op (smallest-op eq-cost rm-cost add-cost)]
                    (cond (= small-op :eq) (cons (mk-eq (first l))
                                                 (edit-script (rest l)
                                                              (rest r)))
                          (= small-op :rm) (cons (mk-rm (first l))
                                                 (edit-script (rest l) r))
                          (= small-op :add) (cons (mk-add l) 
                                                  (edit-script l (rest r)))))
                  (if (<= rm-cost add-cost)
                    (cons (mk-rm (first l)) 
                          (edit-script (rest l) r))
                    (cons (mk-add(first r)) 
                          (edit-script l (rest r))))))))

(defmacro defmemo
  [name & fdecl]
  `(def ~name
     (memoize (fn ~fdecl))))
                
(defn tree-size
  "Compute size of tree"
  [tree]
  (loop [t (zip/vector-zip tree)
         size 0]
    (if (zip/end? t) 
      size
      (recur (zip/next t) (inc size)))))
        
(declare tree-dist)

(defmemo tree-dist
  [old new]
  (cond (and (arity? old) (arity? new)) 
          (edit-dist-f tree-dist tree-size old new)
        (arity? old)
          (inc (tree-size old))
        (arity? new)
          (inc (tree-size new))
        :else (if (= old new) 0 2)))

(defn safe-for
  "Decide pt≼(old,new)"
  [pt old new]
  (when-let [mid (tree-diff-apply pt old)]
    (= (tree-dist old new)
       (+ (tree-dist old mid)
          (tree-dist mid new)))))

;; 
; given C = {(t1,t1'),...,(tn,tn')}
; B is a function from (t,t') to concrete simple safe patches
; let Bs = map B C
; let Ps = cartesian-product Bs
; let Ts = reduce merge-diffs
; (filter (safe-for C) Ts)


(def parser (insta/parser grammar))

      
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
