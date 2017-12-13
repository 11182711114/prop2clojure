(ns nca2.core
  (:gen-class))  
(import java.io.File)
(import java.io.FileReader)
  
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



;;; SQL TESTING

(def persons '({:id 1 :name "olle"} {:id 2 :name "anna"} {:id 3 :name"isak"} {:id 4 :name "beatrice"}))

;;; SQL

(defmacro select
  "Acts like an SQL statement (e.g. 'SELECT :name from persons where [:id = 2] orderby :name').
  Due to how it is constucted the where clause can use any clojure function that returns boolean and looks like [column op value] -> '(op column value)'"
  [columns _ from _ where _ orderby]
  `(map
    #(select-keys % ~columns)
    (sort-by 
      ~orderby 
      (filter 
        (fn 
          [item#] 
          (~(second where) (get item# ~(first where)) ~@(nnext where))) 
        ~from))))
