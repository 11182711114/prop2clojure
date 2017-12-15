(ns assign1.core
  (:gen-class))  
  
(defmacro safe 
  ([form]
   `(try 
      ~form
      (catch Exception formException# (str (.toString formException#)))))
  ([vector form]
   `(try
     (let ~vector 
       (try      
         ~form
         (catch Exception formException# (str (.toString formException#)))
         (finally 
           (if (instance? java.io.Closeable ~(first vector)) 
             (.close ~(first vector))))))
     (catch Exception bindingException# (str (.toString bindingException#))))))



;;; FOR SQL TESTING
(def persons '({:id 1 :name "olle" :length 156} {:id 2 :name "anna" :length 201} {:id 3 :name"isak" :length 190} {:id 4 :name "beatrice" :length 175}))

;;; SQL

(defmacro select
  "Acts like an SQL statement (e.g. 'SELECT [:name :id] from persons where [:id = 2] orderby :name').
  Due to how it is constucted the where clause can use any clojure function that returns boolean and looks like [column op value] -> '(op column value)'"
  [columns _ from _ where _ orderby]
  `(map
    #(select-keys % ~columns)
    (sort-by 
     ~orderby 
     (filter 
      #(~(second where) 
        (~(first where) %) 
        ~@(nnext where)) 
      ~from))))


;;; Non working restricted solution, cant figure out how to test predicate symbol(e.g. >) against a list. Tried making them strings but that is not allowed, tried to (symbol >) them, didnt work
(defn <> 
  [x y]
  (not= x y))

(defmacro select-restricted
  "Acts like an SQL statement (e.g. 'SELECT [:name :id] from persons where [:id = 2] orderby :name').
  Due to how it is constucted the where clause can use any clojure function that returns boolean and looks like [column op value] -> '(op column value)'"
  [columns _ from _ where _ orderby]
  `(map
    #(select-keys % ~columns)
    (sort-by 
     ~orderby 
     (filter 
      #(~(if (true? (some (fn [x] (= (str x) (str (second where)))) [> < = <>]))
           ((second where) 
            `(~(first where) %) 
             (nnext where))
           (throw (IllegalArgumentException. (str "Illegal operator used in where predicate: " (str (second where))))))) 
      ~from))))


;;; working non-restricted, infinite where predicates version
(defmacro select-preds
  "Acts like an SQL statement (e.g. 'SELECT [:name :id] from persons where [:id = 2] orderby :name').
  Due to how it is constucted the where clause can use any clojure function that returns boolean and looks like [column op value] -> '(op column value)'
  Works for any number of predicates in the where clause as long as they are in groups of 3, e.g. [:id > 1 :id < 4 :name not= 'isak']
  Note: this could be in the macro above as an alternative (if (> (count where) 3)) but is done separate for any automated grading/testing, also leaves out whitelisting operators"
  [columns _ from _ where _ orderby]
  `(map
    #(select-keys % ~columns)
    (sort-by 
     ~orderby
     (filter 
      #(every? true? (for [[colu# op# val#] (partition 3 ~where)] (op# (colu# %) val#))) 
      ~from))))
 






