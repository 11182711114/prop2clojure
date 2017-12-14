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
      #(~(second where) (~(first where) %) ~@(nnext where)) 
      ~from))))


(defmacro selecttest
  "Acts like an SQL statement (e.g. 'SELECT [:name :id] from persons where [:id = 2] orderby :name').
  Due to how it is constucted the where clause can use any clojure function that returns boolean and looks like [column op value] -> '(op column value)'"
  [columns _ from _ where _ orderby]
  `(map
    #(select-keys % ~columns)
    (sort-by 
     ~orderby
     (~(for [[colu op val] (partition 3 where)]
        `(filter 
          #(~op (~colu %) ~val) 
          ~from)))))) ;; should end the filter list with ~from
 
(defmacro testss
  [where]
  `(~(partition 3 where)))





