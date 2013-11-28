(ns golem.analytics
  (:use-macros [dommy.macros :only [sel sel1]])
  (:use [dommy.core :only [text]]
        [clojure.string :only [replace]]))

; should not alter dom, only analyze it

(defn child-nodes [node]
  (let [child-nodes (.-childNodes node)]
    (for [i (range 0 (.-length child-nodes))]
      (aget child-nodes i))))

(defn traverse
  "Traverses a DOM tree, given root node."
  [pred node]
  (if (and (pred node) (.hasChildNodes node))
    (apply str (map #(traverse pred %) (child-nodes node)))
    (if (= (.-nodeType node) (.-TEXT_NODE js/Node))
      (str " " (text node)))))

(defn- nname [n]
  (.-nodeName n))

(defn- content-node? [node]
  (not (or (= (nname node) "SCRIPT")
           (= (nname node) "STYLE"))))

(defn stringify [node]
  (replace (traverse content-node? node) #"\s+" " "))

(defn start-analytics! [state]
  (let [content (stringify (sel1 :body))]
    content))
