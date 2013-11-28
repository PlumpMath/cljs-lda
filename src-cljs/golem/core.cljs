(ns golem.core
  (:use [golem.repl :only [connect]]
        [golem.backend :only [send-msg backend-db!]]
        [golem.persist :only [start-db! trans! curse! add!]]
        [golem.toolbar :only [start-toolbar!]]
        [golem.analytics :only [start-analytics!]]
        [golem.canvas-plot :only [plot-fns!]]))

; fire up server-side javascript repl
#_(do (ns golem.clojure.start)
       (cemerick.piggieback/cljs-repl))
; fire up a repl for the browser and eval namespace on top once connected
#_(do (ns golem.clojure.start)
      (require 'cljs.repl.browser)
      (cemerick.piggieback/cljs-repl
       :repl-env (doto (cljs.repl.browser/repl-env :port 9010)
                   cljs.repl/-setup)))


(def dev-state (atom {:user {:name "Golem"}
                      :db {:type :indexeddb
                           :name "golem-xiii-test3"
                           :version 1}}))

(defn system [& [state]]
  (let[state (or state
                 (atom {:user {:name "Golem"}}))]
    #_(start-db! state)
    (backend-db! state)
    (start-toolbar! state)
    (start-analytics! state)))

(set! (.-onload js/window) #(system dev-state))

; force late loading
(system dev-state)

#_(let [db (@dev-state :db)
        {h :handle} db
        cb (fn [e] (js/alert (str "Trans-Result: " (.. e -target -result))))
        t (trans! h ["page-content"] :readwrite)]
    #_(add! t "page-content" [{:id "nums" :numbers [1 2 3 4]}])
    (curse! t "page-content" #(js/alert
                               (let [cur (.. % -target -result)]
                                 (loop [res []]
                                   (if cur (let [k (js->clj (.-key cur))
                                                 v (js->clj (.-value cur))
                                                 e {(keyword k) v}]
                                             (.continue cur)
                                             (recur (conj res e)))
                                       res)))))
    #_(.getName h))
