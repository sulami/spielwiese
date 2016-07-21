#lang racket

(require 2htdp/universe 2htdp/image)
(require lang/posn)

(define WIDTH 640)
(define HEIGHT 480)
(define UFO-SIZE 20)
(define GRAVITY 1/10)
(define POWER 1)
(define FATAL-SPEED 3)

(define UFO
  (triangle UFO-SIZE "solid" "gray"))
(define GROUND
  (rectangle WIDTH 20 "solid" "brown"))
(define BACKGROUND
  (rectangle WIDTH HEIGHT "solid" "black"))

(struct game-state
        (y-pos y-speed)
        #:mutable)

(define INITIAL-STATE
  (game-state 20 0))

(define (draw-frame current-state)
  (place-images
    (list GROUND UFO)
    (list (make-posn (/ WIDTH 2) (- HEIGHT 10))
          (make-posn (/ WIDTH 2) (game-state-y-pos current-state)))
    BACKGROUND))

(define (next-frame current-state)
  (set-game-state-y-speed! current-state
                           (+ GRAVITY (game-state-y-speed current-state)))
  (set-game-state-y-pos! current-state
                         (+ (game-state-y-speed current-state)
                            (game-state-y-pos current-state)))
  current-state)

(define (accelerate current-state)
  (set-game-state-y-speed! current-state
                           (- (game-state-y-speed current-state) POWER))
  current-state)

(define (handle-input current-state key)
  (cond [(key=? key "up") (accelerate current-state)]
        [else current-state]))
  
(define (reached-ground current-state)
  (>= (+ (game-state-y-pos current-state) (/ UFO-SIZE 2))
      (- HEIGHT 20)))

(define (check-landing end-state)
  (cond [(<= (game-state-y-speed end-state) FATAL-SPEED)
         (overlay (text "You made it!" 30 "white") BACKGROUND)]
        [else (overlay (text "Wasted!" 30 "red") BACKGROUND)]))

(big-bang INITIAL-STATE
          (on-tick next-frame)
          (on-key handle-input)
          (to-draw draw-frame)
          (stop-when reached-ground check-landing))
