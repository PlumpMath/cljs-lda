(ns golem.repl
  (:require
    [clojure.browser.repl :as repl]))

(defn ^:export connect []
  (repl/connect "http://localhost:9010/repl"))
