(ns golem.backend
  (:use [golem.persist :only [start-db!]]))

(defn dispatch [request sender send-response]
  (let [state (js->clj (.-type request))]
    (case (.-type request)
      "start-db" (send-response (start-db! state))
      nil (.log js/console
                (str "WARNING: dispatch value of request <"
                     request "> is null.")))))

(defn send-message
  "Sending message msg with callback cb(res) for result."
  [msg cb]
  (.sendMessage (.runtime js/chrome) (clj->js msg) cb))

(defn start-backend
  "Start listening for messages."
  []
  (let [msg-listener (.. js/chrome -runtime -onMessage)]
    (.addListener msg-listener dispatch)))
