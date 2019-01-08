(ns conneries.executer
  )

(defn create-built-in [name]
  (vector name
          (hash-map :type :fn :value {:name name :type :built-in})))

(def built-ins ["+" "-" "if" "=" "print"])

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

(defmethod read-ast :srting [obj] obj)

(defmethod read-ast :quotedlist [obj] obj)

(defmethod read-ast :keyword [obj] obj)

(defmethod read-ast :fn [obj] obj)

;; magic starts here

;; word returns ^^^ these values or function
;; word always resolves from the memory
(defmethod read-ast :word [obj]
  (@state (:value obj)))

;; it just returns something
(defmethod read-ast :list [obj]
  (let [func (first (:value obj))
        args (rest (:value obj))]
    (execute-function (read-ast func) args)))

(defmethod read-ast :default [obj]
  (throw (java.lang.IllegalArgument. "OHOHOOO MOYA OBORONA")))

(defn execute-built-in-function [name args]
  (case name
    "+" {:value (reduce #(+ %1 (:value (read-ast %2))) 0 args) :type :number}
    "-" {:value (reduce #(- %1 (:value (read-ast %2)))
                        (:value (read-ast (first args)))
                        (rest args)) :type :number}
    "=" {:value (reduce #(= %1 %2) (first args) (rest args)) :type :bool}
    "print" {:value (do (print (:value (first args))) (:value (first args)))
             :type (:type (first args))}
    "if" (if (:value (read-ast (first args)))
           (nth args 1)
           (nth args 2))
    {:value 1 :type :nil}))

(defn execute-function [fun args]
  (if (= (get-in fun [:value :type]) :built-in)
    (execute-built-in-function (get-in fun [:value :name]) args)
    (vector (get-in fun [:value :type]))))

(comment (read-ast
          {:type :list
           :value [{:type :word :value "if"}
                   {:type :list :value [{:type :word :value "="}
                                        {:type :string :value "asd"}
                                        {:type :string :value "asd"}]}
                   {:type :string :value "asd"}
                   {:type :string :value "dsa"}]}))

