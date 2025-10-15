(ns raytracing
  (:require
   [body :as body]
   [clojure.java.io :as io]
   [models :refer [sphere]]
   [ray :as ray] ;; this is weird, calva repl is fine with just [ray] but `bb -m raytracing` fails and need [ray :as ray]
   [vec3]))

;; following https://raytracing.github.io/books/RayTracingInOneWeekend.html

(set! *warn-on-reflection* true)

(defn clamp [x min-v max-v] (min max-v (max x min-v)))

(defn write-color! [^java.io.BufferedWriter out color]
  (let [[r g b] (mapv #(int (* 256 (clamp % 0.0 0.999))) color)]
    (.write out (str r " " g " " b "\n"))))

(def hittables
  [(sphere [0 0 -1] 0.5)
   (sphere [0 -100.5 -1] 100)])

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

(defn color [{::ray/keys [direction] :as ray} bodies]
  (if-let [hit-record (hit-anything ray bodies 0 10)]
    (let [normal (::body/normal hit-record)]
      (vec3/multiply (vec3/add normal [1 1 1]) 0.5))
    (let [[_x y _z] (vec3/unit direction)
          a         (* 0.5 (+ y 1.0))]
      (vec3/add (vec3/multiply [1.0 1.0 1.0] (- 1.0 a))
                (vec3/multiply [0.5 0.7 1] a)))))

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
         camera-center   [0 0 0]
         viewport-u      [viewport-width 0 0]
         viewport-v      [0 (- viewport-height) 0] ;; negative because we want upper-left to be zero and increases at we scan down
         pixel-du        (vec3/divide viewport-u image-width)
         pixel-dv        (vec3/divide viewport-v image-height)
         upper-left      (-> camera-center
                             (vec3/subtract [0 0 focal-length])
                             (vec3/subtract (vec3/divide viewport-u 2))
                             (vec3/subtract (vec3/divide viewport-v 2)))
         pixel-00-loc    (vec3/add upper-left (vec3/multiply (vec3/add pixel-du pixel-dv) 0.5))

         samples-per-px  10
         colors          (for [j (range image-height)
                               i (range image-width)]
                           (->> #(let [pixel-sample  (-> pixel-00-loc
                                                         (vec3/add (vec3/multiply pixel-du (+ i (- (rand) 0.5))))
                                                         (vec3/add (vec3/multiply pixel-dv (+ j (- (rand) 0.5)))))
                                       ray-direction (vec3/subtract pixel-sample camera-center)
                                       a-ray         #::ray{:origin camera-center :direction ray-direction}]
                                   (color a-ray hittables))
                                (repeatedly samples-per-px)
                                (reduce vec3/add)
                                ((fn [color] (vec3/divide color samples-per-px)))))]
     (with-open [out (io/writer "scene.ppm")]
       (.write out (str "P3\n" image-width " " image-height "\n255\n"))
       (doseq [color colors]
         (write-color! out color)))
     :done)))
