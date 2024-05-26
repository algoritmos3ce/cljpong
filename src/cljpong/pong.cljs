(ns cljpong.pong
  (:require
   [cljs.math :refer [sqrt]]))

(def W 300)
(def H 300)

(def PADDLE_LEFT 0)
(def PADDLE_RIGHT 1)

(def PADDLE_GAP 10)
(def PADDLE_WIDTH 10)
(def PADDLE_HEIGHT 60)

(def BALL_RADIUS 5)

(def FPS 30.0)
(def MS_PER_FRAME (/ 1000.0 FPS))
(def VELOCITY (/ 150.0 FPS))

(defn random-ball-velocity []
  (let [vx (if (> (rand) 0.5) 1 -1)
        vy (rand)
        norm (sqrt (+ (* vx vx) (* vy vy)))]
    [(/ vx norm) (/ vy norm)]))

(defn ball-restart [] {:pos [(/ W 2.0) (/ H 2.0)]
                       :vel (random-ball-velocity)})

(defn initial-state [] {:paddles [(/ H 2.0) (/ H 2.0)]
                        :score [0 0]
                        :ball (ball-restart)})

(def paddle-x {PADDLE_LEFT PADDLE_GAP
               PADDLE_RIGHT (- H PADDLE_GAP) })

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
                  (+ (* VELOCITY dy 1.5))
                  (clamp 0 H))))

(defn apply-action [state [action & rest]]
  (case action
    :move-paddle (move-paddle state rest)
    :reset (initial-state)))

(defn move-ball [state]
  (update state :ball
          (fn [{[x y] :pos [vx vy] :vel}]
            (let [x' (+ x (* vx VELOCITY))
                  y' (+ y (* vy VELOCITY))]
              {:pos [x' y'] :vel [vx vy]}))))

(defn check-wall-collision [state]
  (update state :ball
          (fn [{[x y] :pos [vx vy] :vel}]
            (let [vy' (if (or (< y BALL_RADIUS) (> y (- H BALL_RADIUS)))
                        (- vy)
                        vy)]
              {:pos [x y] :vel [vx vy']}))))

(defn paddle-collision? [state paddle]
  (let [[bx by] (ball-pos state)
        [px py] (paddle-pos state paddle)]
    (and (<= (abs (- bx px)) (+ (/ PADDLE_WIDTH 2.0) BALL_RADIUS))
         (<= (abs (- by py)) (+ (/ PADDLE_HEIGHT 2.0) BALL_RADIUS)))))

(defn check-paddle-collision [state]
  (update state :ball
          (fn [{[x y] :pos [vx vy] :vel}]
            (let [[vx' vy'] (if (some true? (map #(paddle-collision? state %) [PADDLE_LEFT PADDLE_RIGHT]))
                              [(- vx) (+ vy (* 2.0 (- (rand) 0.5)))]
                              [vx vy])]
              {:pos [x y] :vel [vx' vy']}))))

(defn score-goal [state paddle]
  (-> state
      (assoc :ball (ball-restart))
      (update-in [:score paddle] inc)))

(defn check-goal [state]
  (let [x (get-in state [:ball :pos 0])
        vx (get-in state [:ball :vel 0])]
    (cond
      (and (pos? vx) (> x W)) (score-goal state PADDLE_LEFT)
      (and (neg? vx) (neg? x)) (score-goal state PADDLE_RIGHT)
      :else state)))

(defn update-state [state actions]
  (-> state
      (#(reduce apply-action % actions))
      move-ball
      check-wall-collision
      check-paddle-collision
      check-goal))
