;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snakegame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; data definitions

(define-struct snake-game [snake food])
; A SnakeGame is a [ListOfPoints Point]
; It consists of a ListOfPoints that describes a snake and another point that
; indicates the location of food
#;
(define (fn-on-snake-game snake-game)
  ... (fn-on-snake snake-game-snake) ... (fn-on-point snake-game-food))

(define-struct point [x y])
; A Point is a [N N]
; It consists of two natural numbers that correspond to x and y coordunates
#;
(define (fn-on-point p)
  ... (point-x p) ... (point-y p))

; constants

(define SNAKECOLOR "red")
(define BACKGROUNDCOLOR "white")
(define UNITCELLSIZE 16)
(define SPACER 1)
(define SEGMENTSIZE (- UNITCELLSIZE SPACER))
(define CURVATURE 4)
(define PULLBACK (- SEGMENTSIZE CURVATURE CURVATURE))

(define CANVASWIDTH (* 80 UNITCELLSIZE))
(define CANVASHEIGHT (* 45 UNITCELLSIZE))
(define SNAKESEGMENT
  (beside
   (rectangle SPACER SEGMENTSIZE "solid" BACKGROUNDCOLOR)
   (above
    (rectangle SEGMENTSIZE SPACER "solid" BACKGROUNDCOLOR)
    (overlay/align
     "left" "bottom" (circle CURVATURE "solid" SNAKECOLOR)
     (overlay/align
      "right" "bottom" (circle CURVATURE "solid" SNAKECOLOR)
      (overlay/align
       "right" "top" (circle CURVATURE "solid" SNAKECOLOR)
       (overlay/align
        "left" "top" (circle CURVATURE "solid" SNAKECOLOR)
        (overlay
         (rectangle SEGMENTSIZE PULLBACK "solid" SNAKECOLOR)
         (rectangle PULLBACK SEGMENTSIZE "solid" SNAKECOLOR)))))))))
(define CANVAS (empty-scene CANVASWIDTH CANVASHEIGHT BACKGROUNDCOLOR))



; functions


(define (main sg)
  ; SnakeGame -> SnakeGame
  ; run the game
  (big-bang sg
    [on-tick update-game]
    [to-draw render-game]
    [on-key turn-snake]
    [stop-when crashed? render-game]))


(define (update-game sg)
  ; !!!
  ; SnakeGame -> SnakeGame
  ; move the snake and make food appear
  sg)


(define (render-game sg)
  ; !!!
  ; SnakeGame -> SnakeGame
  ; render the state of the game on screen
  (place-image SNAKESEGMENT
               (point-x (snake-game-snake sg))
               (point-y (snake-game-snake sg))
               CANVAS))


(define (turn-snake sg ke)
  ; SnakeGame -> SnakeGame
  ; turns the snake using the keyboard
  (make-snake-game
   (cond
     [(key=? "up" ke) (make-point
                    (point-x (snake-game-snake sg))
                    (- (point-y (snake-game-snake sg)) UNITCELLSIZE))]
     [(key=? "down" ke) (make-point
                      (point-x (snake-game-snake sg))
                      (+ (point-y (snake-game-snake sg)) UNITCELLSIZE))]
     [(key=? "left" ke) (make-point
                      (- (point-x (snake-game-snake sg)) UNITCELLSIZE)
                      (point-y (snake-game-snake sg)))]
     [(key=? "right" ke) (make-point
                      (+ (point-x (snake-game-snake sg)) UNITCELLSIZE)
                      (point-y (snake-game-snake sg)))])
     (snake-game-food sg)))


  (define (crashed? sg)
    ; !!!
    ; SnakeGame -> Boolean
    ; returns #t when the snake crashes out
    #f)



  (define PLAYSNAKE (make-snake-game (make-point 400 400) (make-point 400 400)))
  (main PLAYSNAKE)

  