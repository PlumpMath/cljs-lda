(ns golem.lda
  (:use [golem.repl :only [connect]]
        [golem.numeric :only [pow gamma digamma sum prod exp log]]
        [golem.probabilities :only [normal-cdf poisson-pmf dirichlet-pdf multinomial-pmf]]))


; LDA
(def NUM 10) ; dummy

(defn discrete-unit-vector [n m]
  (assoc (into [] (take m (repeat 0))) (dec n) 1))

#_(discrete-unit-vector 5 10)

(def dim NUM)

; representation of words
(defn word [v] (discrete-unit-vector v dim))

(defn document [& words] (into [] words))

(defn corpus [& docs] docs)

#_(defn mixture-model-generative [xi pi beta]
  (let [N 1])) ; TODO


; k is fixed, beta is k x V - matrix
; beta[i,j] = p( w^i | z^j )
; lim alpha against 0 => mixture model with single topics (sparsity)
(defn lda-generative-process [xi alpha k beta]
  (let [N (poisson-pmf xi 42) ; poisson not important, other possible
        theta (dirichlet-pdf alpha k)]
    (map #(let [topic (multinomial-pmf theta (rand))
                word (multinomial-pmf topic beta)]
            word) N)))

; EM-algorithm
; expectation update rules
(comment
  (= [gamma* phi*]
     (arg-min gamma phi (D (q [theta z] [gamma phi])
                           '||
                           (p [theta z] [w alpha beta])))))

(defn E-log-theta [gam i] ; proven analytically
  (- (digamma (gamma i)) (digamma (sum gam))))

(defn update-phi-prop [n i {{beta-ni i} n} theta gamma]
  (* beta-ni (exp (E-log-theta gamma i))))

(defn update-gamma [i {alpha-i i} phi] ; N implicit in (count phi)
  (+ alpha-i (sum (map #(% i) phi))))

;; update alpha and beta

#_(defn update-beta)

(defn perplexity [doc wprobs N]
  (exp (- (/ (sum (map #(log (wprobs %)) doc))
             (sum (map N doc))))))



; port Blei LDA 2003 var. inference

(def settings {:var-max-iter 20
               :var-convergence 1E-6
               :em-max-iter 100
               :em-convergence 1E-4
               :alpha :estimate})


(def num-topics 10)

(def init-alpha 1)
