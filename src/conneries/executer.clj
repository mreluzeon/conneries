(ns conneries.executer
  )

(defn create-built-in [name]
  (vector name
          (hash-map :type :fn :value {:name name :type :built-in})))

(def built-ins [:+ :- :if := :print :define! :set! :do])

(def state
  (atom (apply hash-map
               (apply concat
                      (map create-built-in built-ins)))))

(declare execute-function)

(defmulti read-ast :type)

;; usual values

(defmethod read-ast :int [obj] obj)

(defmethod read-ast :float [obj] obj)

(defmethod read-ast :number [obj] obj)

(defmethod read-ast :ratio [obj] obj)

(defmethod read-ast :bool [obj] obj)

(defmethod read-ast :string [obj] obj)

(defmethod read-ast :quotedlist [obj] obj)

(defmethod read-ast :keyword [obj] obj)

(defmethod read-ast :fn [obj] obj)

;; magic starts here

;; word returns ^^^ these values or function
;; word ALWAYS RESOLVES from the memory
(defmethod read-ast :word [obj]
  (@state (:value obj)))

;; list ALWAYS RESOLVES!
(defmethod read-ast :list [obj]
  (let [func (first (:value obj))
        args (rest (:value obj))]
    (execute-function (read-ast func) args)))

(defmethod read-ast :default [obj]
  (throw (new Exception "ERR")))

(defn my-do [[x & xs]]
  (if xs
    (do (read-ast x) (recur xs))
    (read-ast x)))

(defn execute-built-in-function [name args]
  (case name
    :+ {:value (reduce #(+ %1 (:value (read-ast %2))) 0 args) :type :number}
    :- {:value (reduce #(- %1 (:value (read-ast %2)))
                       (:value (read-ast (first args)))
                       (rest args)) :type :number}
    := {:value (apply = (map read-ast args)) :type :bool}
    :print (let [{:keys [value type]} (read-ast (first args))]
             {:value (do (println value) value)
              :type type})
    :if (if (:value (read-ast (first args)))
          (read-ast (nth args 1 {:value nil :type :nil}))
          (read-ast (nth args 2 {:value nil :type :nil})))
    :define! (swap! state assoc (:value (nth args 0)) (nth args 1))
    :set! (if (contains? @state (:value (nth args 0)))
            (swap! state assoc (:value (nth args 0)) (read-ast (nth args 1)))
            (throw (new Exception "YOU CANNOT SET! BEFORE DEFINE")))
    :do (my-do args)
    ;; DEFAULT
    {:value nil :type :nil}))

(defn execute-function [fun args]
  (if (= (get-in fun [:value :type]) :built-in)
    (execute-built-in-function (get-in fun [:value :name]) args)
    (vector (get-in fun [:value :type]))))

(comment (read-ast
          {:type :list
           :value [{:type :word :value :if}
                   {:type :list :value [{:type :word :value :=}
                                        {:type :string :value "asd"}
                                        {:type :string :value "asd"}]}
                   {:type :string :value "asd"}
                   {:type :string :value "dsa"}]}))

(comment (read-ast
          {:type :list
           :value [{:type :word :value :=}
                   {:type :word :value :salutor}
                   {:type :number :value 3}]}))

(comment
  (require '[conneries.parser :refer [get-ast]])
  (read-ast
   (:value
    (get-ast
     "(= salutor 3)"))))

(comment (read-ast
          {:type :list
           :value [{:type :word :value :=}
                   {:type :string :value "a"}
                   {:type :string :value "b"}]}))

