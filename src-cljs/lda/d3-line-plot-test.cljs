(ns lda.d3-line-plot-test)

(def margins {:top 20
              :right 20
              :bottom 30
              :left 50})

(def totalwidth 960)

(def totalheight 500)

(def width (- totalwidth (margins :left) (margins :right)))

(def height (- totalheight (margins :top) (margins :bottom)))

(def x (-> js/d3.time
           .scale
           (.range (clj->js [0 width]))))

(def y (-> js/d3.scale
           .linear
           (.range (clj->js [height 0]))))

(def xAxis (-> js/d3.svg
               .axis
               (.scale x)
               (.orient "bottom")))

(def yAxis (-> js/d3.svg
               .axis
               (.scale y)
               (.orient "left")))


(def line (-> js/d3.svg
              .line
              (.x #(x (.-date %)))
              (.y #(y (.-close %)))))

(def svg (-> (js/d3.select "body")
             (.append "svg")
             (.attr "width" totalwidth)
             (.attr "height" totalheight)
             (.append "g")
             (.attr "transform" (str "translate(" (margins :left) "," (margins :top) ")"))))

(def parse-date (.-parse (.format js/d3.time "%d-%b-%y")))


(defn load [err data]
  (doall (map #(do (set! (.-date %) (parse-date (.-date %)))
                   (set! (.-close %) (js/parseFloat (.-close %)))) data))
  (.domain x (.extent js/d3 data #(.-date %)))
  (.domain y (.extent js/d3 data #(.-close %)))
  (-> svg
      (.append "g")
      (.attr "class" "x axis")
      (.attr "transform" (str "translate(0," height ")"))
      (.call xAxis))
  (-> svg
      (.append "g")
      (.attr "class" "y axis")
      (.call yAxis)
      (.append "text")
      (.attr "transform" "rotate(-90)")
      (.attr "y" 6)
      (.attr "dy" ".71em")
      (.style "text-anchor" "end")
      (.text "Price ($)"))
  (-> svg
      (.append "path")
      (.datum data)
      (.attr "class" "line")
      (.attr "d" line)))


#_(.tsv js/d3 "line-plot-data.tsv" load)
