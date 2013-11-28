(ns golem.numeric)

; maybe just use def like with rand
(defn sign [x] (cond (> x 0) 1
                     (= x 0) 0
                     :else -1))

(defn sqrt [x] (.sqrt js/Math x))

(defn exp [x] (.exp js/Math x))

(defn log [x] (.log js/Math x))

(defn sin [x] (.sin js/Math x))

(defn cos [x] (.cos js/Math x))

(defn abs [x] (.abs js/Math x))

(defn pow [b e] (.pow js/Math b e))


(defn round [fl] (.round js/Math fl))

(def sum (partial reduce +))

(def prod (partial reduce *))

(defn avg [& nums] (/ (sum nums) (count nums)))

(defn rand-seq
  ([] (rand-seq 1))
  ([k]
     ((fn inner []
        (lazy-seq (cons (rand k)
                        (inner)))))))

; simpler composition
(defn rand-seq2
  ([] (rand-seq2 1))
  ([k] (repeatedly (partial rand k))))


#_(take 5 (rand-seq 100))
#_(take 10 (rand-seq2 5))

(def pi (.-PI js/Math))

(defn erf
  "Error funciton, taken from
   https://en.wikipedia.org/wiki/Error_function Maximum error is
   1.5*10^7"
  [x]
  (let [t (/ 1
             (+ 1 (* 0.3275911 (abs x))))
        a1 0.254829592
        a2 -0.284496736
        a3 1.421413741
        a4 -1.453152027
        a5 1.061405429
        poly-horner (* t (+ a1 (* t (+ a2 (* t (+ a3 (* t (+ a4 (* t a5)))))))))]
    (* (sign x) (- 1 (* poly-horner (exp (- (* x x))))))))
#_(erf -1) ; => -0.8427006897475899
; Wolfram Alpha -0.84270079294971486934122063508260925929606699796630

(defn sigmoid [x] (/ 1
                     (+ 1 (exp (- x)))))

(defn inner [z z* eta]
  (sum (map * eta (map * z z*))))


; TODO move or remove
(deftype complex [^Number real ^Number imag])


(defn plus [^complex z1 ^complex z2]
  (let [x1 (.-real z1)
        y1 (.-imag z1)
        x2 (.-real z2)
        y2 (.-imag z2)]
    (complex. (+ x1 x2) (+ y1 y2))))

(defn times [^complex z1 ^complex z2]
  (let [x1 (.-real z1)
        y1 (.-imag z1)
        x2 (.-real z2)
        y2 (.-imag z2)]
    (complex. (- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* y1 x2)))))

#_(.-real (plus (complex. 1 0) (complex. -2 0)))
#_(.-real (times (complex. 1 0) (complex. -2 0)))


#_(time (dorun (repeatedly 100000 #(plus (complex. 1 0) (complex. 0 1)))))

(defn gamma
  "Only for real z for now.
   Taken from https://en.wikipedia.org/wiki/Lanczos_approximation,
   Factors by GNU Scientific Library"
  [z]
  (let [g 7
        p [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
           771.32342877765313, -176.61502916214059, 12.507343278686905,
           -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]]
    (if (< z 0.5)
      (/ pi (sin (* pi z)))
      (let [z* (dec z)
            x (reduce #(+ %1 (/ (get p %2)
                                (+ z* %2)))
                      (get p 0)
                      (range 1 (+ g 2)))
            t (+ z* g 0.5)]
        (* (sqrt (* 2 pi)) (pow t (+ z* 0.5)) (exp (- t)) x)))))



#_(gamma pi)
#_(gamma 5)
#_(gamma 0.1)
#_(- pi (pow (gamma (/ 1 2)) 2)) ; 0 with err 5E-16

(def fac (comp gamma inc))


(defn digamma
  "Only for positive real x.
   Implemented after Fortran version: http://www.uv.es/~bernardo/1976AppStatist.pdf
   err < 1E-10"
  [x]
  (let [S 0.00001
        C 8.5
        gamma 0.5772156649 ; = (- (digamma 1))
        ]
    (cond (< x 0) 0 ; todo throw exception?
          (< x S) (- (- gamma)
                     (/ 1 x))
          (< x C) (- (digamma (inc x)) (/ 1 x)) ; reduce to stirling
          :else (+ (log x) ; Stirling approximation
                   (- (/ 1 (* 2 x)))
                   (- (/ 1 (* 12 (* x x))))
                   (+ (/ 1 (* 120 (pow x 4))))
                   (- (/ 1 (* 254 (pow x 6))))))))

; quick comparison vs. WolframAlpha, all errors in bound
#_(digamma 0.0000001) ; -1.00000006 × 10^7 (a bit unprecise)
#_(digamma 1) ; -0.57721566490153286060651209008240243104215933593992
#_(digamma 2) ; 0.4227843350984671393934879099175975689578406640600764
#_(digamma 3) ; 0.9227843350984671393934879099175975689578406640600764
#_(digamma 5) ; 1.5061176684318004727268212432509309022911739973934097
#_(digamma 10) ; 2.2517525890667211076474561638858515372118089180283303
#_(digamma 1000) ; 6.9072551956488120520500061142514977454795198337688800
#_(digamma 100000) ; 11.512920464961895086756707273421817069751539605842277
