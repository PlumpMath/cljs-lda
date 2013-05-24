(ns lda.probabilities
  (:use [lda.numeric :only [sign sqrt pow log exp sigmoid erf fac inner rand rand-seq]]))

(defn- log-likelihood
  "Now we look at this function from a different perspective by
  considering the observed values x1, x2, ..., xn to be
  fixed \"parameters\" of this function est, whereas θ will be the function's
  variable and allowed to vary freely; this function will be called the
  likelihood: "
  [est theta smpls]
  (reduce + (map #(log (est % theta)) smpls)))

(defn- max-likelihood
  "Maximize this!"
  [est theta smpls]
  (/ (log-likelihood est theta smpls)
     (count smpls)))


; for relational topic models
(defn phi-sig [z z* eta mean]
  (sigmoid  (+ (inner z z* eta)
               mean)))

(defn phi-exp [z z* eta mean]
  (exp (+ (inner z z* eta)
          mean)))

(defn normal-cdf [x mean sd]
  (let [z (/ (- x mean)
             (sqrt (* 2 sd sd)))]
    (/ (+ 1 (* (sign z) (erf z)))
       2)))

#_(normal-cdf 1.4241 30 25)

(defn phi-phi [z z* eta mean]
  (normal-cdf (+ (inner z z* eta)
                 mean)
              mean 1))

(defn phi-n ; TODO eta and mean restricted so 0 <= mean <= 1
  "Multivariate Gaussian density function with mean 0 and diagonal
   covariance characterized by eta."
  [z z* eta mean]
  (let [diff (map - z z*)
        sq-dist (map * diff diff)]
    (exp (+ (- (reduce + (map * eta sq-dist)))
            (- mean)))))


(defn poisson-pmf [lambda k]
  (if (< x 0) 0
      (* (/ (pow lambda k)
            (fac k))
         (exp (- lambda)))))

#_(poisson-pmf 10 10)
#_(map (fn [r] [r (poisson-pmf 10 r)]) (map int (take 10 (rand-seq 20))))

(defn multinomial-pmf [theta x]
  (let [n (reduce + x)]
    (* (/ (fac n)
          (reduce * (map fac x)))
       (reduce * (map pow theta x)))))

#_(multinomial-pmf [0.2 0.3 0.5] [1 2 3])


(def unit-exp (comp exp -))

; taken from Rubenstein and Melamed
; found on http://iew3.technion.ac.il/~onn/Selected/AOR11.pdf
(defn rand-simplex [dim]
  (let [unit-exps (map unit-exp (take dim (rand-seq)))
        sum (reduce + unit-exps)]
    (map #(/ % sum) unit-exps)))

#_(reduce + (rand-simplex 5))
