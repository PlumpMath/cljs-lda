(ns lda.core
  (:use [lda.repl :only [connect]]
        [lda.numeric :only [pow gamma]]
        [lda.probabilities :only [normal-cdf poisson-pmf dirichlet-pdf multinomial-pmf]]))

; fire up server-side javascript repl
#_(do (ns cljs-lda.clojure.start)
       (cemerick.piggieback/cljs-repl))
; fire up a repl for the browser and eval namespace on top once connected
#_(do (ns lda.clojure.start)
      (require 'cljs.repl.browser)
      (cemerick.piggieback/cljs-repl
       :repl-env (doto (cljs.repl.browser/repl-env :port 9009)
                   cljs.repl/-setup)))


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


; k is fixed, beta is k x V - matrix
; beta[i,j] = p( w^i | z^j )
(defn lda-generative-process [xchi alpha k beta doc]
  (let [N (poisson-pmf xchi) ; poisson not important, other possible
        theta (dirichlet-pdf alpha k)]
    (map #(let [topic (multinomial-pmf theta (rand))
                word (multinomial-pmf topic beta)]
            word) doc)))














                                        ; nur boilerplate zum kopieren:jojo ich werd das noch aufpolieren
                                        ; ok ich räume kurz auf und dann kannst du dein zeug einpflegen? später eher, ok
; ui
(defn draw-state! [key ref old new]
  (doall (map #(set! (.-innerHTML (.getElementById js/document (name %)))
                     (apply str (map (fn [e]
                                       (.-outerHTML (crate/html [:div {:style {:width (str (* 20 e) "px")
                                                                               :height "10px"
                                                                               :margin "0 auto"
                                                                               :background-color "red"}}])))
                                     (% new))))
              (keys new))))


; state management
(def some-towers (atom {}))

(defn init []
  (swap! some-towers (fn [] {:A '(1 2 3 4 5 6)
                            :B '()
                            :C '()})))

(defn move-plate! [towers from to]
  (swap! towers #(assoc % to (cons (first (from %)) (to %))
                        from (rest (from %)))))

(add-watch some-towers :draw draw-state!)


; algorithm using hanoi.stack delayed scheduling
(defn thanoi [height towers A B C]
  (if (== height 1)
    (sched #(move-plate! towers A C))
    (sched #(thanoi (dec height) towers A C B)
           #(move-plate! towers A C)
           #(thanoi (dec height) towers B A C))))


#_(set! (.-innerHTML (.getElementById js/document "A")) "<h1>Salut</h1>")


#_(init)
#_(simulation-loop #(thanoi (count (:A @some-towers)) some-towers :A :B :C))
