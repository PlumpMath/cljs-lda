(ns lda.crossover.shared
  (:require;*CLJSBUILD-REMOVE*;-macros
    [lda.crossover.macros :as macros]))

(defn make-hanoi-text []
  (macros/reverse-eval
    ("code" "shared " "from the " "Hello " str)))
