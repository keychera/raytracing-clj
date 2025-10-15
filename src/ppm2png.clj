; ppm2png.clj - Clojure translation of Java PPM2PNG
; Copyright (C) 2022 Paulo Pinto
; This file is a translation of:
; https://github.com/pjmlp/ppm2png/blob/f443ef59b266eac3615afc00f41a8ec766cd19a7/java/src/main/java/org/progtools/ppm2png/PPM2PNG.java
; Commit: f443ef59b266eac3615afc00f41a8ec766cd19a7
;
; Translated by GitHub Copilot (@copilot), an AI language model assistant.
; - Model role: code-translation assistant (LLM) operating as GitHub Copilot.
; - Translation date (UTC): 2025-10-15
; - Notes: This file is an automated language translation from the original Java source with some manual human fixes. 
;   It is a derivative work of the original file and therefore is distributed under the same license as the original.
;
; Licensed under the GNU General Public License v3.0 or later (GPL-3.0-or-later).
; See: https://www.gnu.org/licenses/gpl-3.0.html
; SPDX-License-Identifier: GPL-3.0-or-later

(ns ppm2png
  (:import [java.io BufferedReader FileReader File]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

;; vibed by github copilot with manual fixes (they are still bad at balanced parens)
;; from https://github.com/pjmlp/ppm2png/blob/main/java/src/main/java/org/progtools/ppm2png/PPM2PNG.java

(defn- parse-int
  "Parse integer from string, trimming whitespace."
  [s]
  (Integer/parseInt (.trim s)))

(defn- err-println
  "Print a formatted message to stderr."
  [fmt & args]
  (.println System/err (apply format fmt args)))

(defn ppm->png [source dest]
  (try
    (with-open [r (BufferedReader. (FileReader. source))]
      ;; header
      (let [expected-header "P3"
            header (.readLine r)]
        (if  (not= expected-header header)
          (err-println "Failed to read %s, bad header" source)
          (let [dim-line (.readLine r)]
            (if (nil? dim-line)
              (do (err-println "Failed to read %s, bad dimensions" source)
                  (System/exit 1))
              (let [[w-str h-str] (.split dim-line "\\s+")
                    width (parse-int w-str)
                    height (parse-int h-str)]

                ;; bpp / max colour value (validate)
                (let [bpp-line (.readLine r)]
                  (if (nil? bpp-line)
                    (do (err-println "Failed to read %s, bad colour size" source)
                        (System/exit 1))
                    (let [bpp (parse-int bpp-line)]
                      (when (or (< bpp 0) (> bpp 255))
                        (err-println "Failed to read %s, bad colour size" source)
                        (System/exit 1)))))

                ;; prepare image buffer
                (let [image-size (* width height)
                      image-data (int-array image-size)
                      expected-components 3]
                  ;; read pixel lines, expecting one pixel per line like the Java code
                  (loop [row 0 line (.readLine r)]
                    (if (nil? line)
                      ;; finished reading; write PNG
                      (let [buffer-img (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
                        (.setRGB buffer-img 0 0 width height image-data 0 width)
                        (ImageIO/write buffer-img "png" (File. dest)))
                      (let [vals (.split line "\\s+")]
                        (if (not= (count vals) expected-components)
                          (do (err-println "Failed to read %s at line %d, bad colour size" source row)
                              (System/exit 1))
                          (let [r-val (parse-int (nth vals 0))
                                g-val (parse-int (nth vals 1))
                                b-val (parse-int (nth vals 2))
                                pixel (bit-or (bit-shift-left r-val 16)
                                              (bit-shift-left g-val 8)
                                              b-val)]
                            (aset-int image-data row pixel)
                            (recur (inc row) (.readLine r))))))))))))))
    (catch Exception e
      (err-println "Failed to process file %s" source)
      (.printStackTrace e)))
  (println (format "Processed %s into %s" source dest)))
