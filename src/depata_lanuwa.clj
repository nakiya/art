(ns depata_lanuwa
  (:require [quil.core :as q]
            [quil.helpers.drawing :refer [line-join-points]]
            [quil.helpers.seqs :refer [range-incl]]))

(def x-limit 1200)

(defn draw-limited-line [x1 y1 x2 y2]
  (cond
    (<= x2 x-limit) (q/line x1 y1 x2 y2)
    (< x1 x-limit)
    (do
      (q/line x1 y1 x-limit y2)
      (q/line 0 y1 (- x2 x-limit) y2))
    :else (q/line (- x1 x-limit) y1 (- x2 x-limit) y2)
    ))

(defn rand-walk-coords
  [seed]
  (lazy-seq (cons seed (rand-walk-coords (+ seed (/ (- (rand 5) 1) 20))))))

(defn slightly-natural-horizontal-line
  [x-start x-end y]
  (let [step      5
        xs        (range-incl x-start x-end step)
        ys        (rand-walk-coords y)
        line-args (line-join-points xs ys)]
    (dorun (map #(apply draw-limited-line %) line-args))))

(defn slightly-natural-vertical-line
  [y-start y-end x]
  (let [step      5
        ys        (range-incl y-start y-end step)
        xs        (rand-walk-coords x)
        line-args (line-join-points xs ys)]
    (dorun (map #(apply draw-limited-line %) line-args))))

(defn n-natural-lines [c1-start c1-end c2 n c2-dist line-fn]
  (doseq [c
          (->> (range-incl n)
               (map (comp (partial + c2) (partial * c2-dist))))]
    (line-fn c1-start c1-end c)))

(defn single-block [weight stroke x n d x-lim]
  (q/stroke-weight weight)
  (q/smooth 8)
  (apply q/stroke stroke)
  ;(q/stroke 30 100 100)
  (let [nd (* n d)]
    (n-natural-lines x (+ x nd) 10 n d slightly-natural-horizontal-line)
    (n-natural-lines 10 (+ 10 nd) (+ x nd) n d slightly-natural-vertical-line)
    (n-natural-lines (+ 10 nd) (+ 10 (* 2 nd)) x n d slightly-natural-vertical-line)
    (n-natural-lines (+ x nd) (+ x (* 2 nd)) (+ 10 nd) n d slightly-natural-horizontal-line)
    )
  )

(def x-offset (atom 0))

(comment

  (reset! x-offset 0)

#__)

(def color (atom [0 0 0]))

(defn cycle-color [[r g b]]
  (let [b (inc b)
        g (mod (+ g (quot b 256)) 256)
        b (mod b 256)
        r (+ r (quot g 256))]
    (if (= r 256)
      [0 0 0]
      [r g b])))


(quot 256 256)

(mod 5 3)
(quot 5 3)

(defn draw []
  (q/background 255)
  (q/stroke 0 0 0)
  (q/frame-rate 60)

  ; Top border
  (q/stroke-weight 3)
  (q/stroke 0 0 0)
  (q/line [0 9] [x-limit 9])

  ; Bottom border
  (q/stroke-weight 3)
  (q/stroke 0 0 0)
  (q/line [0 109] [x-limit 109])

  (doseq [x (range-incl 0 (- x-limit 100) 100)]
    (single-block 2 @color (+ @x-offset x) 5 10 x-limit))

  (swap! x-offset inc)
  (if (> @x-offset x-limit) (reset! x-offset 0))

  (q/fill 0)
  (q/text (str (q/current-frame-rate)) 30 30)

  (swap! color cycle-color)
  )

(q/defsketch depata_lanuwa
  :title ""
  :settings #(q/smooth 2)
  :draw draw
  :size [x-limit 120])
