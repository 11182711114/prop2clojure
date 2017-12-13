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



;;; SQL

(defn sortSQL
  [coll orderby]
  (sort-by 
    ()
    coll))

(defn filterSQL
  [from where]
  (filter 
    (where) 
    from))

(defn printpersons
  [coll]
  (clojure.pprint/print-table 
    (filter 
      (fn 
        [item] 
        (> (get item :id) 2))
      coll)))

(defmacro select
  [columns _ from _ where _ orderby]
  ((do 
     (println columns)
     (println from)
     (println where)
     (println orderby))))
   
