(ns nca2.core
  (:gen-class))
(comment "
(defn -main []
  (println "Hello, World!"))

(defmacro test1
  [vector form]
  `(let [~(first vector) ~(second vector)] ~(first vector))) 

(defmacro test2
  ([vector form] (println "first"))
  ([vector] (println "second")))



  (defn closeable 
  [vector form] 
  (`(let [~(first vector) ~(second vector)] 
  ((~form
  (. ~(first vector) close))))))
  
  
  
  (defn not-closable 
  [vector form]
  (println [vector form]))
  
  (defmacro safes 
  [vector form]
  (if (instance? java.io.Closeable (second vector))
  (closeable [vector form])
  (not-closable [vector form])))
  
  
  (defmacro closeable
  [vector form]
  `(try
  (let ~vector 
  (try      
  ~form
  (catch Exception e# (str (.getMessage e##)))
  (finally (.close ~(first vector)))))
  (catch Exception ee# (str (.getMessage e##)))))
  
  (defmacro uncloseable
  [vector form]
  `(try
  (let ~vector    
  ~form)
  (catch Exception ee# (str (.getMessage ee#)))))
  ")

  
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

(defn sortSQL
  [coll orderby]
  `(sort-by 
    orderby
    coll))

(defn filterSQL
  [coll pred]
  `(filter 
     (fn test
       [item#] 
       (~(second pred) (get item# ~(first pred)) ~(nnext pred))
       ~(coll))))

(defmacro select
  [columns _ from _ where _ orderby]
  `(filterSQL ~from ~where))
   
