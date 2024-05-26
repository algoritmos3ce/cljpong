(ns cljpong.core
  (:require
   [cljpong.pong :as pong]
   [cljs.math :refer [PI]]
   [goog.events :as gevent]
   [goog.events.KeyCodes :as keycodes]
   [goog.string :as gstring]))

(enable-console-print!)

(def canvas (.getElementById js/document "canvas"))
(def ctx (.getContext canvas "2d"))

(defn draw-rect [x y w h color]
  (set! (.-fillStyle ctx) color)
  (.beginPath ctx)
  (.rect ctx x y w h)
  (.fill ctx))

(defn draw-circle [x y r color]
  (set! (.-fillStyle ctx) color)
  (.beginPath ctx)
  (.arc ctx x y r 0 (* 2 PI))
  (.fill ctx))

(defn draw-text [x y s color]
  (set! (.-fillStyle ctx) color)
  (set! (.-textAlign ctx) "center")
  (set! (.-font ctx) "20px sans-serif")
  (.fillText ctx s x y))

(defn draw-paddle [state paddle]
  (let [[x y] (pong/paddle-pos state paddle)]
    (draw-rect (- x (/ pong/PADDLE_WIDTH 2.0))
               (- y (/ pong/PADDLE_HEIGHT 2.0))
               pong/PADDLE_WIDTH
               pong/PADDLE_HEIGHT
               "#ffffff")))

(defn draw-ball [state]
  (let [[x y] (pong/ball-pos state)]
    (draw-circle x y pong/BALL_RADIUS "#ffffff")))

(defn draw-score [state]
  (let [[s1 s2] (pong/score state)]
    (draw-text (/ pong/W 2.0) 30 (gstring/format "%d - %d" s1 s2) "#ffffff")))

(defn draw [state]
  (.clearRect ctx 0 0 pong/W pong/H)
  (draw-rect 0 0 pong/W pong/H "#000000")
  (draw-paddle state pong/PADDLE_LEFT)
  (draw-paddle state pong/PADDLE_RIGHT)
  (draw-ball state)
  (draw-score state))

(def pressed-keys* (atom #{}))

(gevent/listen js/document "keydown"
  (fn [ev] (swap! pressed-keys* #(conj % (.-keyCode ev)))))

(gevent/listen js/document "keyup"
  (fn [ev] (swap! pressed-keys* #(disj % (.-keyCode ev)))))

(def actions {keycodes/Q [:move-paddle pong/PADDLE_LEFT -1]
              keycodes/A [:move-paddle pong/PADDLE_LEFT 1]
              keycodes/UP [:move-paddle pong/PADDLE_RIGHT -1]
              keycodes/DOWN [:move-paddle pong/PADDLE_RIGHT 1]
              keycodes/R [:reset]})

(def pong-state* (atom (pong/initial-state)))

(defn update-render []
  (swap! pong-state*
         #(pong/update-state % (filter some? (map actions @pressed-keys*))))
  (draw @pong-state*))

(js/setInterval update-render pong/MS_PER_FRAME)

