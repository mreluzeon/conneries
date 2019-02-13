(ns conneries.core
  (:gen-class)
  (:require [conneries.executer :as exec]
            [conneries.parser :as parser]
            [clojure.java.io :as io]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [file (nth args 0)]
    ;;(print
     (exec/read-ast
      (-> file
          slurp
          parser/get-ast
          :value))))
  ;;)
