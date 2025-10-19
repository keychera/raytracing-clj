(ns vec3a)

;; vec3a is ^doubles [x y z], initially there is a class named vec3 using clojure vec but it is slow
;; we wanted float but functions type hints are only limited to long and double

(set! *warn-on-reflection* true)

(defn x ^double [^doubles v] (aget v 0))
(defn y ^double [^doubles v] (aget v 1))
(defn z ^double [^doubles v] (aget v 2))

(defn make
  (^doubles [] (make-array Double/TYPE 3))
  (^doubles [^double x ^double y ^double z]
   (doto (make) (aset 0 x) (aset 1 y) (aset 2 z))))

(defn add ^doubles [^doubles v1 ^doubles v2]
  (make (+ (x v1) (x v2))
        (+ (y v1) (y v2))
        (+ (z v1) (z v2))))

(defn subtract ^doubles [^doubles v1 ^doubles v2]
  (make (- (x v1) (x v2))
        (- (y v1) (y v2))
        (- (z v1) (z v2))))

(defn negative ^doubles [^doubles v]
  (make (- (x v)) (- (y v)) (- (z v))))

(defn mult-vec3 ^doubles [^doubles v1 ^doubles v2]
  (make (* (x v1) (x v2)) (* (y v1) (y v2)) (* (z v1) (z v2))))

(defn mult-scalar ^doubles [^doubles v ^double s]
  (make (* (x v) s) (* (y v) s) (* (z v) s)))

(defn divide ^doubles [^doubles v ^double s]
  (make (/ (x v) s) (/ (y v) s) (/ (z v) s)))

(defn length-squared ^double [^doubles v]
  (+ (* (x v) (x v)) (* (y v) (y v)) (* (z v) (z v))))

(defn length ^double [v] (Math/sqrt (length-squared v)))

(defn dot ^double [^doubles v1 ^doubles v2]
  (+ (* (x v1) (x v2))
     (* (y v1) (y v2))
     (* (z v1) (z v2))))

(defn cross ^doubles [^doubles u ^doubles v]
  (let [u0 (x u) u1 (y u) u2 (z u)
        v0 (x v) v1 (y v) v2 (z v)]
    (make (- (* u1 v2) (* u2 v1))
          (- (* u2 v0) (* u0 v2))
          (- (* u0 v1) (* u1 v0)))))

(defn unit ^doubles [^doubles v] (divide v (length v)))

(defn rand-double ^double [^double vmin ^double vmax]
  (+ vmin (* (- vmax vmin) (rand))))

(defn random-unit-vec3 ^doubles []
  (loop [x (rand-double -1.0 1.0)
         y (rand-double -1.0 1.0)
         z (rand-double -1.0 1.0)]
    (let [lensq (+ (* x x) (* y y) (* z z))]
      (if (and (> lensq 1e-160) (<= lensq 1.0))
        (divide (make x y z) (Math/sqrt lensq))
        (recur (rand-double -1.0 1.0) (rand-double -1.0 1.0) (rand-double -1.0 1.0))))))

(defn random-in-unit-disk ^doubles []
  (loop [x (rand-double -1.0 1.0) y (rand-double -1.0 1.0)]
    (let [lensq (+ (* x x) (* y y))]
      (if (< lensq 1.0)
        (make x y 0.0)
        (recur (rand-double -1.0 1.0) (rand-double -1.0 1.0))))))

(defn reflect ^doubles [v n]
  (subtract v (mult-scalar n (* 2 (dot v n)))))

(defn refract ^doubles [^doubles uv ^doubles n ^double eta-per-eta']
  (let [cos-theta   (min (dot (negative uv) n) 1.0)
        r'-perpen   (mult-scalar (add uv (mult-scalar n cos-theta)) eta-per-eta')
        r'-parallel (mult-scalar n (- (Math/sqrt (Math/abs (- 1.0 (length-squared r'-perpen))))))]
    (add r'-perpen r'-parallel)))
