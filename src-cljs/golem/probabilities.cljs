(ns golem.probabilities
  (:use [golem.numeric :only [sum prod sign sqrt pow log exp gamma sigmoid erf fac inner rand-seq]]))

(def unit-exp (comp exp -))

; taken from Rubenstein and Melamed
; found on http://iew3.technion.ac.il/~onn/Selected/AOR11.pdf
(defn rand-simplex [dim]
  (let [unit-exps (map unit-exp (take dim (rand-seq)))
        sum (sum unit-exps)]
    (map #(/ % sum) unit-exps)))

#_(= 1 (sum (rand-simplex 5)))

(defn- log-likelihood
  "Now we look at this function from a different perspective by
  considering the observed values x1, x2, ..., xn to be
  fixed \"parameters\" of this function est, whereas θ will be the function's
  variable and allowed to vary freely; this function will be called the
  likelihood: "
  [est theta smpls]
  (sum (map #(log (est % theta)) smpls)))

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
    (/ (+ 1 (erf z))
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
    (exp (+ (- (sum (map * eta sq-dist)))
            (- mean)))))

(defn bernoulli-pmf [mu x]
  (* (pow mu x)
     (pow (- 1 mu) (- 1 x))))

(defn poisson-pmf [lambda k]
  (if (< k 0) 0
      (* (/ (pow lambda k)
            (fac k))
         (exp (- lambda)))))


#_(poisson-pmf 10 (int (rand 10)))
#_(map (fn [r] [r (poisson-pmf 10 r)]) (map int (take 10 (rand-seq 20))))

(defn multinomial-pmf [theta x]
  (let [n (sum x)]
    (* (/ (fac n)
          (prod (map fac x)))
       (prod (map pow theta x)))))

#_(multinomial-pmf [0.2 0.3 0.5] [1 2 3])

(defn dirichlet-pdf [alpha smpl]
  (* (/ (gamma (reduce + alpha))
        (prod (map gamma alpha)))
     (prod (map #(pow %1 (dec %2)) smpl alpha))))

(defn uniform-dirichlet-pdf [smpl dim] (dirichlet-pdf smpl (repeat 1 dim)))

(defn alpha-scale [alpha] (reduce + alpha))

(defn alpha-mean [alpha] (map #(/ % (sum alpha)) alpha))

#_(uniform-dirichlet-pdf (rand-simplex) 5) ; always 1
#_(alpha-scale [5 4 1])
#_(alpha-mean [5 4 1])
#_(dirichlet-pdf [0.3 0.3 0.4] (repeat 3 2.3))
#_(dirichlet-pdf [0.5 0.4 0.1] [5 4 1])
