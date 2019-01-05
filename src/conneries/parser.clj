(ns conneries.parser
  (:use [blancas.kern.core]))

(def int-parser
  (bind [number (<+> (many1 (one-of* "1234567890-")))]
        (return {:type :int
                 :value (Integer/parseInt number)})))

(def float-parser
  (bind [number (<+> (many1 (one-of* "1234567890-")) (sym* \.) (many1 (one-of* "1234567890")))]
        (return {:type :float
                 :value (Float/parseFloat number)})))

(def ratio-parser
  (bind [enumerator (many1 (one-of* "1234567890-"))
         _ (sym* \/)
         denumerator (many1 (one-of* "1234567890"))]
        (return {:type :ratio
                 :value (/ (Integer/parseInt (apply str enumerator))
                           (Integer/parseInt (apply str denumerator)))})))

(def bool-parser
  (bind [bool (<|> (token* "#t") (token* "#f"))]
        (return {:type :bool
                 :value (if (= bool "#t") true false)})))

(def string-parser
  (bind [val (between (sym* \") (sym* \") (many1 (none-of* "\"")))]
        (return {:type :string
                 :value (apply str val)})))

(def keyword-parser
  (bind [val (>> (sym* \') (many1 (none-of* " \n()")))]
        (return {:type :keyword
                 :value (keyword (apply str val))})))

(def word-parser
  (bind [val (many1 (none-of* " \n()"))]
        (return {:type :word
                 :value (keyword (apply str val))})))

(declare quotedlist-parser)
(declare list-parser)

(def sth-parser
  (<|> (<:> list-parser)
       (<:> quotedlist-parser)
       (<:> float-parser)
       (<:> ratio-parser)
       (<:> int-parser)
       (<:> bool-parser)
       (<:> string-parser)
       (<:> keyword-parser)
       (<:> word-parser)))

(def quotedlist-parser
  (bind [_ (sym* \()
         vals (sep-by (many1 (one-of* " \n,\t")) sth-parser)
         _ (sym* \))]
        (return {:type :quotedlist
                 :value vals})))

(def list-parser
  (bind [_ (sym* \()
         vals (sep-by1 (many1 (one-of* " \n,\t")) sth-parser)
         _ (sym* \))]
        (return {:type :list
                 :value vals})))
