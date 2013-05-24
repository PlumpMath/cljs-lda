(ns hanoi.crossover.shared
  (:require;*CLJSBUILD-REMOVE*;-macros
    [hanoi.crossover.macros :as macros]))

(defn make-hanoi-text []
  (macros/reverse-eval
    ("code" "shared " "from the " "Hello " str)))
