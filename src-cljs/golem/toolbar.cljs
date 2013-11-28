(ns golem.toolbar
  (:use-macros [dommy.macros :only [sel sel1 node deftemplate]])
  (:use [dommy.core :only [append! listen! show! hide! text]]
        [golem.analytics :only [traverse]]))


(deftemplate gstage []
  [:div {:id "golem-stage"
         :style {:position "absolute"
                 :display "none"
                 :top "-400px"
                 :margin-left "auto"
                 :margin-right "auto"
                 :width "400px"
                 :height "400px"
                 :background-color "#999999"}}
   [:canvas {:id "golem-plot-canvas"
             :width "400px"
             :height "400px"} "Canvas"]])


(deftemplate gtoolbar [name]
  [:div {:id "golem-toolbar"
         :style {:position "fixed"
                 :bottom "0px"
                 :width "100%"
                 :height "30px"
                 :color "#CC9966"
                 :z-index "20"
                 :background-color "#111111"}}
   (or name "Not configured.")])


(defn start-toolbar! [state]
  (let [{:keys [user]} @state
        {:keys [name]} user
        stage (gstage)
        toolbar (gtoolbar name)]
    (-> (sel1 "body")
        (append! toolbar))
    (-> (sel1 :#golem-toolbar)
        (append! stage))
    (listen! toolbar :mouseover #(show! stage))
    (listen! toolbar :mouseout #(hide! stage))))




#_(listen! (sel1 :#golem-console) :click #(js/alert "Hihi!"))

#_(show! (sel1 :#golem-stage))
