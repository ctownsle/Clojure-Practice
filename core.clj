(ns clojuretest.core
  (:gen-class))

  (defn containssymbol [l]
    (cond 
      (= true (symbol? (nth l 1))) true 
      (= true (symbol? (nth l 2))) true
      :else false) 
  )
  (defn potentialMatch [l]
    (cond
        (= '+ (first l)) 
            (if (= 0 (nth l 1)) (nth l 2) 
                (do (if (= 0 (nth l 2)) (nth l 1) l)))
          (= '* (first l)) (if (= 0 (nth l 1)) 0 (do (if (= 0 (nth l 2)) 0 
              (do (if (= 1 (nth l 1)) (nth l 2) (do (if (= 1 (nth l 2)) (nth l 1) l)))))))
          (= '- (first l)) (if (seq? (nth l 1))
            (do (def mejff (nth l 1))
            (if (= (first mejff) '-) (nth mejff 1) l)) l)
                                                              ))
  (defn simplify [l]
    (if (containssymbol l) 
      (cond
        (= true (seq? (nth l 1 nil))) (simplify (nth l 1))
        (= true (seq? (nth l 2 nil))) (simplify (nth l 2))
        :else (potentialMatch l))
      (cond 
        (= true (seq? (nth l 1 nil))) (simplify (nth l 1))
        (= true (seq? (nth l 2 nil))) (simplify (nth l 2))
        :else  (eval l)
        )))
(defn basic-simplify [l]
  (if (and (number? (nth l 1))(number? (nth l 2)))
    (eval l)
  (potentialMatch l)))
    
(defn containslist [l]
  (if (empty? l) 
    false 
    (do 
      (if (seq? (first l)) 
        true (containslist (rest l)))))
)

(defn dobindings [m v]
  (map (fn [i] 
    (if (or (vector? i) (seq? i))
      (dobindings m i)
       (m i i))) 
      v))
(defn goodsimplify [l]
  (if (not (seq? l))
    l 
    (if (containslist l)
      (basic-simplify (map goodsimplify l))
      (basic-simplify l))))
    ;; end
  

(defn transform [m v]
  (vector (goodsimplify (list  '+ 
    (goodsimplify (list '* (nth (nth m 0) 0) (nth v 0)))
    (goodsimplify (list '* (nth (nth m 0) 1) (nth v 1)))
    ))
    (goodsimplify (list  '+ 
      (goodsimplify (list ' * (nth (nth m 1) 0) (nth v 0)))
      (goodsimplify (list '* (nth (nth m 1) 1) (nth v 1)))
      
 ))))
  
(defn eval-exp [exp bindings]
  (def newexp (dobindings bindings exp))
  (if (= 'transform (nth (nth newexp 2) 0))
   (do
      (def mejeff (nth (nth newexp 2 nil) 2 nil))
      (def mejeffhalf (transform (nth mejeff 1) (nth mejeff 2)))
      (def mejeff2 (transform ( nth (nth newexp 2 ) 1)  mejeffhalf))
      (transform (nth newexp 1) mejeff2)
   ) 
  (transform (nth newexp 1) (nth newexp 2))
   )
)

(defn listtovec [l]
  (into [] l))


(def p1 '(transform [[a 3] [0 0]] [x y]))
(def p2 '(transform [[1 0] [0 (+ x 3)]] [(* x 2) y]))
(def p3 '(transform [[0 0] [1 1]] 
  (transform [[2 0] [0 2]] 
    (transform [[-1 0] [0 -1]] [x 2]))))
(def hello (dobindings '{} p3))
(eval-exp p2 '{x 1})

