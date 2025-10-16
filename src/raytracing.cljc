(ns raytracing
  (:require
   #?@(:bb  [[clojure.java.io :as io]]
       :clj [[clojure.java.io :as io]
             [ppm2png :refer [ppm->png]]])
   [body :as body]
   [models :refer [sphere]]
   [ray :as ray] ;; this is weird, calva repl is fine with just [ray] but `bb -m raytracing` fails and need [ray :as ray]
   [vec3a :as vec3a]))

;; following https://raytracing.github.io/books/RayTracingInOneWeekend.html

#?(:clj (set! *warn-on-reflection* true))

(defn clamp [x min-v max-v] (min max-v (max x min-v)))

(defn write-color! [^java.io.BufferedWriter out color]
  (let [[r g b] (mapv #(int (* 256 (clamp % 0.0 0.999))) color)]
    (.write out (str r " " g " " b "\n"))))

(def hittables
  [(sphere (vec3a/make 0.0 0.0 -1.0) 0.5)
   (sphere (vec3a/make 0.0 -100.5 -1.0) 100)])

(defn hit-anything [ray bodies t-min t-max]
  (loop [[body & remaining] bodies
         closest-so-far     t-max
         hit-record         nil]
    (if (some? body)
      (let [hit-fn  (::body/hit-fn body)
            got-hit (hit-fn ray t-min closest-so-far)]
        (if (some? got-hit)
          (recur remaining (::body/t got-hit) got-hit)
          (recur remaining closest-so-far     hit-record)))
      hit-record)))

(defn ray-color [{::ray/keys [^doubles direction] :as ray} depth world]
  (if (<= depth 0)
    (vec3a/make)
    (if-let [hit-record (hit-anything ray world 1e-3 ##Inf)]
      (let [normal    (::body/normal hit-record)
            point     (::body/point hit-record)
            direction (vec3a/add normal (vec3a/random-unit-vec3))]
        (vec3a/multiply (ray-color #::ray{:origin point :direction direction} (dec depth) world) 0.5))
      (let [y (vec3a/y (vec3a/unit direction))
            a (* 0.5 (+ y 1.0))]
        (vec3a/add (vec3a/multiply (vec3a/make 1.0 1.0 1.0) (- 1.0 a))
                   (vec3a/multiply (vec3a/make 0.5 0.7 1.0) a))))))

(defn -main []
  (time
   (let [;; image
         aspect-ratio    16/9
         image-width     400
         image-height    (int (/ image-width aspect-ratio))

         ;; camera
         focal-length    1.0
         viewport-height 2.0
         ;; not using aspect-ratio is deliberate here
         viewport-width  (* viewport-height (/ image-width image-height))
         camera-center   (vec3a/make)
         viewport-u      (vec3a/make viewport-width 0.0 0.0)
         viewport-v      (vec3a/make 0.0 (- viewport-height) 0.0) ;; negative because we want upper-left to be zero and increases at we scan down
         pixel-du        (vec3a/divide viewport-u image-width)
         pixel-dv        (vec3a/divide viewport-v image-height)
         upper-left      (-> camera-center
                             (vec3a/subtract (vec3a/make 0.0 0.0 focal-length))
                             (vec3a/subtract (vec3a/divide viewport-u 2))
                             (vec3a/subtract (vec3a/divide viewport-v 2)))
         pixel-00-loc    (vec3a/add upper-left (vec3a/multiply (vec3a/add pixel-du pixel-dv) 0.5)) 
         samples-per-px  100
         colors          (for [j (range image-height)
                               i (range image-width)]
                           (->> #(let [pixel-sample  (-> pixel-00-loc
                                                         (vec3a/add (vec3a/multiply pixel-du (+ i (- (rand) 0.5))))
                                                         (vec3a/add (vec3a/multiply pixel-dv (+ j (- (rand) 0.5)))))
                                       ray-direction (vec3a/subtract pixel-sample camera-center)
                                       a-ray         #::ray{:origin camera-center :direction ray-direction}]
                                   (ray-color a-ray 10 hittables))
                                (repeatedly samples-per-px)
                                (reduce vec3a/add)
                                ((fn [color] (vec3a/divide color samples-per-px)))))]
     #?(:clj
        (with-open [out (io/writer "scene.ppm")]
          (.write out (str "P3\n" image-width " " image-height "\n255\n"))
          (doseq [color colors]
            (write-color! out color))))
     #?(:bb  :noop
        :clj (ppm->png "scene.ppm" "scene.png")))))
