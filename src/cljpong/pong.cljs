(ns cljpong.pong
  (:require
   [cljs.math :refer [sqrt]]))

(def map-width 300)
(def map-height 300)
(def map-center (mapv #(/ % 2.0) [map-width map-height]))

(def paddle-gap 10)
(def paddle-size [10 60])

(def ball-radius 5)

(def fps 30.0)
(def ms-per-frame (/ 1000.0 fps))
(def velocity (/ 150.0 fps))

(defn random-ball-velocity! []
  (let [vx (if (> (rand) 0.5) 1 -1)
        vy (rand)
        norm (sqrt (+ (* vx vx) (* vy vy)))]
    [(/ vx norm) (/ vy norm)]))

(defn ball-restart! [] {:pos map-center
                        :vel (random-ball-velocity!)})

(defn initial-state! [] {:paddles {:left (map-center 1) :right (map-center 1)}
                         :score {:left 0 :right 0}
                         :ball (ball-restart!)})

(def paddle-x {:left paddle-gap
               :right (- map-height paddle-gap) })

(defn paddle-y [state paddle] (get-in state [:paddles paddle]))

(defn paddle-pos [state paddle] [(paddle-x paddle) (paddle-y state paddle)])

(defn ball-pos [state] (get-in state [:ball :pos]))

(defn score [state] (state :score))

(defn clamp [x x- x+]
  (cond
    (< x x-) x-
    (> x x+) x+
    :else x))

(defn move-paddle [state [paddle dy]]
  (update-in state [:paddles paddle]
             #(-> %
                  (+ (* velocity dy 1.5))
                  (clamp 0 map-height))))

(defn apply-action! [state [action & rest]]
  (case action
    :move-paddle (move-paddle state rest)
    :reset (initial-state!)))

(defn move-ball [state]
  (update state :ball
          (fn [{[x y] :pos [vx vy] :vel}]
            (let [x' (+ x (* vx velocity))
                  y' (+ y (* vy velocity))]
              {:pos [x' y'] :vel [vx vy]}))))

(defn check-wall-collision [state]
  (update state :ball
          (fn [{[x y] :pos [vx vy] :vel}]
            (let [vy' (if (or (< y ball-radius) (> y (- map-height ball-radius)))
                        (- vy)
                        vy)]
              {:pos [x y] :vel [vx vy']}))))

(defn paddle-collision? [state paddle]
  (let [[bx by] (ball-pos state)
        [px py] (paddle-pos state paddle)
        [pw ph] paddle-size]
    (and (<= (abs (- bx px)) (+ (/ pw 2.0) ball-radius))
         (<= (abs (- by py)) (+ (/ ph 2.0) ball-radius)))))

(defn check-paddle-collision! [state]
  (update state :ball
          (fn [{[x y] :pos [vx vy] :vel}]
            (let [[vx' vy'] (if (some true? (map #(paddle-collision? state %) [:left :right]))
                              [(- vx) (+ vy (* 2.0 (- (rand) 0.5)))]
                              [vx vy])]
              {:pos [x y] :vel [vx' vy']}))))

(defn score-goal! [state paddle]
  (-> state
      (assoc :ball (ball-restart!))
      (update-in [:score paddle] inc)))

(defn check-goal! [state]
  (let [x (get-in state [:ball :pos 0])
        vx (get-in state [:ball :vel 0])]
    (cond
      (and (pos? vx) (> x map-width)) (score-goal! state :left)
      (and (neg? vx) (neg? x)) (score-goal! state :right)
      :else state)))

(defn update-state! [state actions]
  (-> state
      (#(reduce apply-action! % actions))
      move-ball
      check-wall-collision
      check-paddle-collision!
      check-goal!))
