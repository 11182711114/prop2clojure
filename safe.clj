; Exceptions are returned as their toString string, 
; if the exception itself is returned (i.e. catch Exception formException) 
; the entire stacktrace is printed in the repl
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


