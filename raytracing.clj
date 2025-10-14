(ns raytracing 
  (:require
   [clojure.java.io :as io]))

(let [width 256 height 256]
  (with-open [out (io/writer "scene.ppm")]
    (.write out (str "P3\n" width " " height "\n255\n"))
    (doall
     (for [j (range height) i (range width)]
       (let [r (int (* 255 (/ i width)))
             g (int (* 255 (/ j height)))
             b 0]
         (.write out (str r " " g " " b "\n"))))))
  :done)