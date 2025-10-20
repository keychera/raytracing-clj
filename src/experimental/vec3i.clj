(ns experimental.vec3i)

(defmacro x ^double [^doubles realm i] `(aget ~realm (* ~i 3)))
(defmacro y ^double [^doubles realm i] `(aget ~realm (+ 1 (* ~i 3))))
(defmacro z ^double [^doubles realm i] `(aget ~realm (+ 2 (* ~i 3))))

(defmacro create! [^doubles realm i x y z]
  `(doto ~realm
     (aset (* ~i 3) ~x)
     (aset (+ 1 (* ~i 3)) ~y)
     (aset (+ 2 (* ~i 3)) ~z)))

(defmacro operator [op ^doubles realm v1 v2 target]
  `(let [x1# (x ~realm ~v1) y1# (y ~realm ~v1) z1# (z ~realm ~v1)
         x2# (x ~realm ~v2) y2# (y ~realm ~v2) z2# (z ~realm ~v2)]
     (create! ~realm ~target (~op x1# x2#) (~op y1# y2#) (~op z1# z2#))))

(defn add! [^doubles realm v1 v2 target]
  (operator + realm v1 v2 target))

(defn subtract! [^doubles realm v1 v2 target]
  (operator - realm v1 v2 target))

(defn mult-vec3! [^doubles realm v1 v2 target]
  (operator * realm v1 v2 target))

(defn read! [^doubles realm i]
  [(x realm i) (y realm i) (z realm i)])


(comment
  (def realm (make-array Double/TYPE 9))

  (-> realm
      (create! 1 1.0 -1.0 0.5)
      (add! 0 1 2)
      (subtract! 1 0 2)
      (mult-vec3! 1 2 0)
      (read! 2)))


