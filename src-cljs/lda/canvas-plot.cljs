(ns lda.canvas-plot
  (:use [lda.numeric :only [sin pi abs exp log erf pow round avg]]))


(def canvas (.getElementById js/document "plot"))

(defn- get-context [cvs] (.getContext cvs "2d"))

(defn- clear! [cvs]
  (.clearRect (get-context cvs) 0 0 (.-clientWidth cvs) (.-clientHeight cvs)))

#_(do
  (set! (.-fillStyle (get-context canvas)) (name "red"))
  (.fillRect (get-context canvas) 40 40 10 10))


(defn normalize
  ([s] (normalize s (reduce min s) (reduce max s)))
  ([s min max]
     (let [dist (abs (- max min))]
       (map #(/ (- % min) dist) s))))

(defn box-plot! [c x-start y-start x-end y-end x-dat y-dat]
  (let [width (- x-end x-start)
        height (- y-start y-end)        ; 0 is top
        count (count x-dat)
        ]
    (doall (map
            (fn [x y]
              (.beginPath c)
              (.rect c
                    (+ x-start (* x width))
                    y-start
                    (/ width count)
                    (- (* height y)))
              (.stroke c)
              (.fill c))
            x-dat
            y-dat))))


(defn dot-plot! [c x-start y-start x-end y-end x-dat y-dat]
  (let [width (- x-end x-start)
        height (- y-start y-end)        ; 0 is top
        ]
    (doall (map
            (fn [x y]
              (.beginPath c)
              (.arc c
                    (+ x-start (* x width))
                    (- y-start (* height y))
                    2
                    (* 2 pi)
                    0)
              (.stroke c)
              (.fill c))
            x-dat
            y-dat))))


(defn cont-plot! [c x-start y-start x-end y-end x-dat y-dat]
  (let [width (- x-end x-start)
        height (- y-start y-end) ; 0 is top
        ]
    (.beginPath c)
    (.moveTo c x-start y-start)
    (doall (map
            (fn [x y] (.lineTo c (+ x-start (* x width)) (- y-start (* height y))))
            x-dat
            y-dat))
    ; close area on the ground
    (.lineTo c (+ x-start width) y-start)
    (.lineTo c x-start y-start)
    (.stroke c)
    (.fill c)))

(defn create-range [start end steps]
  (let [dist (- end start)
        step (/ dist steps)]
    (map #(+ start (* % step)) (range 0 (inc steps)))))


(defn scientific-float [fl prec]
  (let [scale (if (= fl 0) 0 (round (/ (log (abs fl)) (log 10))))
        mant #(format (str "%." prec "f") (/ % (pow 10 scale)))]
    (cond (= scale 0) (mant fl)
          (= scale 1) (mant (* 10 fl))
          (= scale 2) (mant (* 100 fl))
          :else (str (mant fl) "E" (if (> scale 0) (str "+" scale) scale)))))

#_(scientific-float 1.348 3)


#_(map #(scientific-float % 0) (create-range 0.1 1.9 9))

(def wish-plot {:x-range (create-range 0 5 100)
                :fns [{:fn sin :color "rgba(0,0,255,0.5)" :label "sin"}
                      {:fn exp :color "rgba(255,0,0,0.5)" :label "exp"}]})

(def sampled-plot {:x-range (create-range 0 5 100)
                   :fns [{:fn sin :color "rgba(0,0,255,0.5)" :label "sin" :normalized-data {} :min -1 :max 1 :dist 2}
                         {:fn exp :color "rgba(255,0,0,0.5)" :label "exp" :normalized-data {} :min 0 :max 149 :dist 149}]
                   :x-frame {0.18 0, 0.36 1, 0.54 2, 0.72 3, 0.9 4, 1 5}
                   :y-frame {}
                   })

(defn draw-line! [c x-start y-start x-end y-end]
    (.beginPath c)
    (.moveTo c x-start y-start)
    (.lineTo c x-end y-end)
    (.stroke c))

(defn draw-text! [c x-offset y-offset text ]
  (set! (.-textBaseline c) "top")
  (.fillText c text x-offset y-offset))

(defn draw-metric-x! [c x-offset y-offset len rng]
    (.beginPath c)
    (doall (map #(draw-line! c
                             (+ x-offset %) (+ y-offset (/ len 2))
                             (+ x-offset %) (- y-offset (/ len 2)))
                rng))
    (.stroke c))

(defn draw-metric-y! [c x-offset y-offset len rng]
    (.beginPath c)
    (doall (map #(draw-line! c
                             (- x-offset (/ len 2)) (- y-offset %)
                             (+ x-offset (/ len 2)) (- y-offset %))
                rng))
    (.stroke c))


(defn create-scale-x! [c x-offset y-offset x-end start end]
    (draw-line! c x-offset y-offset x-end y-offset)

    (draw-text! c x-offset (+ y-offset 10) (scientific-float start 2))
    (draw-text! c (avg x-offset x-end) (+ y-offset 10) (scientific-float (avg start end) 2))
    (draw-text! c (- x-end 70) (+ y-offset 10) (scientific-float end 2))

    (draw-metric-x! c x-offset y-offset 10 (create-range 0 (- x-end x-offset) 10)))

(defn create-scale-y! [c x-offset y-offset y-end start end]
  (draw-line! c x-offset y-offset x-offset y-end)

  (draw-text! c 0 (- y-offset 15) (scientific-float start 2))
  (draw-text! c 0 (avg 0 y-offset) (scientific-float (avg start end) 2))
  (draw-text! c 0 0 (scientific-float end 2))

  (draw-metric-y! c x-offset y-offset 10 (create-range 0 (- y-offset y-end) 10)))



#_(do (clear! canvas)
      (let [c (get-context canvas)
            x-start -5
            x-end 5
            y-start -10
            y-end 10
            rng (create-range x-start x-end 100)]
        (set! (.-fillStyle c) "rgba(0,0,255,0.5)")
        (box-plot! c 75 370 400 0
                    (normalize rng x-start x-end)
                    (normalize (map #(/ 1 %) rng) y-start y-end))

        (set! (.-fillStyle c) "rgba(0,255,0,0.5)")
        (cont-plot! c 75 370 400 0
                    (normalize rng x-start x-end)
                    (normalize (map sin rng) y-start y-end))

        (set! (.-fillStyle c) "rgba(255,0,0,0.5)")
        (dot-plot! c 75 370 400 0
                    (normalize rng x-start x-end)
                    (normalize (map exp rng) y-start y-end))
        (set! (.-strokeStyle c) "black")
        (set! (.-fillStyle c) "black")
        (set! (.-font c) "normal 15px sans-serif")
        (create-scale-x! c 75 370 400 x-start x-end)
        (create-scale-y! c 75 370 0 y-start y-end)))
