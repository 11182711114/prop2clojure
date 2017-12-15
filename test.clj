(defn safeFuncOne 
  ([form]
   (try 
    (eval form) 
    (catch Exception e# 
      (.toString e#)))))
  

(defn safeFuncTwo
  [input form]
  (try
    (eval (apply form (eval (first input))))
    (catch Exception e#
      (.toString e#))))
