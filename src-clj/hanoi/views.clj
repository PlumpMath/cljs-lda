(ns hanoi.views
  (:require
    [hiccup
      [page :refer [html5]]
      [element :refer [javascript-tag]]
      [page :refer [include-js]]]))


(defn style [& info]
  (.trim (apply str (map #(let [[kwd val] %]
                            (str (name kwd) ": " val "; "))
                         (apply hash-map info)))))


(defn- run-clojurescript [path init]
  (list
    (include-js path)
    (javascript-tag init)))


(defn index-page []
  (html5
    [:head
     [:title "Hanoi for ClojureScript"]]
    [:body
      [:h1 "Hanoi for ClojureScript"]
     [:div {:id "hanoi"}
      [:div {:id "A" :style (style :display "inline-block"
                                   :height "50px"
                                   :width "120px")} "A"]
      [:div {:id "B" :style (style :display "inline-block"
                                   :height "50px"
                                   :width "120px")} "B"]
      [:div {:id "C" :style (style :display "inline-block"
                                   :height "50px"
                                   :width "120px")} "C"]
      ]
      (run-clojurescript
        "/js/main-debug.js"
        "hanoi.repl.connect()")]))
