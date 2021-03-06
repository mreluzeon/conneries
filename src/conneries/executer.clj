(ns conneries.executer
  )

(defn create-built-in [name]
  (vector name
          (hash-map :type :fn :value {:name name :type :built-in})))

(def built-ins [:+ :- :if := :print :define! :set! :do :fn])

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
(defmethod read-ast :hashmap [obj] obj)
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
    :fn {:type :fn
         :value {:args (nth args 0 nil)
                 :body (nth args 1 nil)
                 :type :user}}
    ;; DEFAULT
    {:value nil :type :nil}))

(defn nested-replace [elem rep coll]
  {:type (:type coll)
   :value
   (apply vector
          (map (fn [i]
                 (if (or (= (:type i) :list) (= (:type i) :quotedlist) (= (:type i) :hashmap))
                   (nested-replace elem rep i)
                   (if (= elem i)
                     rep
                     i))) (:value coll)))})

(comment (nested-replace {:type :word :value :bad}
                {:type :word :value :awesome}
                (:value (get-ast "'(owo '() owo)"))))

(defn execute-user [fun args]
  (let [val-args (:value (nth (:value fun) 1 nil))
        body (nth (:value fun) 2 nil)]
    (read-ast
     (reduce #(nested-replace (first %2) (second %2) %1)
             body (map vector val-args args)))))

;; (defn execute-user [fun args]
;;   (nth (:value fun) 2 nil))

;; (defn call-function [fun args]
;;   (let [fun-id (rand 10000)]
;;     ))

(comment (read-ast (:value (get-ast "(define! plus-one (fn '(a) (+ a 1)))"))))
(comment (read-ast (:value (get-ast "(plus-one 4)"))))

(defn execute-function [fun args]
  (if (= (get-in fun [:value :type]) :built-in)
    (execute-built-in-function (get-in fun [:value :name]) args)
    (execute-user fun args)))

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
     "kek"))))

(comment (read-ast
          {:type :list
           :value [{:type :word :value :=}
                   {:type :string :value "a"}
                   {:type :string :value "b"}]}))
