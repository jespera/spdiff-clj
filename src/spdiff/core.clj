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
  "Get list of subterms of given term"
  [t]
  (if (vector? t)
    (reduce (fn [acc t'] 
              (set/union (conj acc t')
                         (sub-terms t'))) 
            #{t}
            t)
    #{t}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
