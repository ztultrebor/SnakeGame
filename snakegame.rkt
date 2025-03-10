;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snakegame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A game of Snake, or Worms as they call it sometimes


; ============================
; data definitions

(define-struct snake-game [snake mvmt food])
; A SnakeGame is a [ListOfPoints 1String Point]
; It consists of a ListOfPoints that describes a snake,
;    a 1String to describe the direction the snake's moving,
;    and a second point that indicates the location of food
#;
(define (fn-on-snake-game snake-game)
  ... (fn-on-snake snake-game-snake) ... (fn-on-snake snake-game-mvmt)
  ... (fn-on-point snake-game-food))


; A Snake is a [ListOf Point]
; Each point describes the location of segment of the snake
#;
(define (fn-on-snake sn)
  (cond
    [(empty? sn) ...]
    [(empty? (rest sn)) ...]
    [else ... (fn-on-point (first sn)) ... (fn-on-snake (rest sn))]))


(define-struct point [x y])
; A Point is a [N N]
; It consists of two natural numbers that correspond to x and y coordinates
#;
(define (fn-on-point p)
  ... (point-x p) ... (point-y p))


; ===============================
; constants

(define SNAKECOLOR "red")
(define BACKGROUNDCOLOR "white")
(define UNITCELLSIZE 16)
(define NCELLSHORIZ 48)
(define NCELLSVERT 30)
(define CANVASWIDTH (* NCELLSHORIZ UNITCELLSIZE))
(define CANVASHEIGHT (* NCELLSVERT UNITCELLSIZE))
(define SNAKESTARTPT (make-point
                      (* (+ (quotient NCELLSHORIZ 2) 1/4) UNITCELLSIZE)
                      (* (+ (quotient NCELLSVERT 2) 1/4) UNITCELLSIZE)))
(define SNAKESTARTDIR "right")
(define SPACER 1)
(define SEGMENTSIZE (- UNITCELLSIZE (* 2 SPACER)))
(define CURVATURE 3)
(define PULLBACK (- SEGMENTSIZE CURVATURE CURVATURE))
(define CORNER (circle CURVATURE "solid" SNAKECOLOR))
(define SNAKESEGMENT
  (beside
   (rectangle SPACER SEGMENTSIZE "solid" BACKGROUNDCOLOR)
   (above
    (rectangle SEGMENTSIZE SPACER "solid" BACKGROUNDCOLOR)
    (overlay/align
     "left" "bottom" CORNER
     (overlay/align
      "right" "bottom" CORNER
      (overlay/align
       "right" "top" CORNER
       (overlay/align
        "left" "top" CORNER
        (overlay
         (rectangle SEGMENTSIZE PULLBACK "solid" SNAKECOLOR)
         (rectangle PULLBACK SEGMENTSIZE "solid" SNAKECOLOR))))))
    (rectangle SEGMENTSIZE SPACER "solid" BACKGROUNDCOLOR))
   (rectangle SPACER SEGMENTSIZE "solid" BACKGROUNDCOLOR)))
(define FOOD (circle 7 "solid" "green"))
(define CANVAS  (empty-scene CANVASWIDTH CANVASHEIGHT BACKGROUNDCOLOR))
(define FIELDOFPLAY
  (beside
   (rectangle (/ UNITCELLSIZE 4)
              (+ CANVASHEIGHT (/ UNITCELLSIZE 2))  "solid" "blue")
   (above
    (rectangle CANVASWIDTH (/ UNITCELLSIZE 4) "solid" "blue")
    CANVAS
    (rectangle CANVASWIDTH (/ UNITCELLSIZE 4) "solid" "blue"))
   (rectangle (/ UNITCELLSIZE 4)
              (+ CANVASHEIGHT (/ UNITCELLSIZE 2)) "solid" "blue")))



; ===============================
; functions


(define (main sg)
  ; SnakeGame -> SnakeGame
  ; run the game
  (big-bang sg
    [on-tick update-game 1/14]
    [to-draw render-game]
    [on-key turn-snake]
    [stop-when crashed? game-over]))


(define (update-game sg)
  ; SnakeGame -> SnakeGame
  ; move the snake and make food appear
  (make-snake-game
   (update-snake-model (snake-game-snake sg)
                       (snake-game-mvmt sg) (snake-game-food sg))
   (snake-game-mvmt sg)
   (teleport-food (snake-game-food sg) (snake-game-snake sg))))


(define (render-game sg)
  ; SnakeGame -> SnakeGame
  ; render the state of the game on screen
  (render-snake (snake-game-snake sg)
                (render-food (snake-game-food sg))))


(define (turn-snake sg ke)
  ; SnakeGame -> SnakeGame
  ; turns the snake using the keyboard
  (local (
          (define dir (snake-game-mvmt sg)))
    ; - IN -
    (make-snake-game
     (snake-game-snake sg)
     (cond
       [(and (key=? "up" ke) (not (string=? dir "down"))) "up"]
       [(and (key=? "down" ke) (not (string=? dir "up"))) "down"]
       [(and (key=? "left" ke) (not (string=? dir "right"))) "left"]
       [(and (key=? "right" ke) (not (string=? dir "left"))) "right"]
       [else (snake-game-mvmt sg)])
     (snake-game-food sg))))


(define (crashed? sg)
  ; SnakeGame -> Boolean
  ; returns #t when the snake crashes out
  (or 
   (not
    (and
     (< (/ UNITCELLSIZE 2)
        (point-x (first (snake-game-snake sg)))
        (- CANVASWIDTH (/ UNITCELLSIZE 2)))
     (< (/ UNITCELLSIZE 2)
        (point-y (first (snake-game-snake sg))) 
        (- CANVASHEIGHT (/ UNITCELLSIZE 2)))))
   (member? (first (snake-game-snake sg)) (rest (snake-game-snake sg)))))


(define (update-snake-model snake move food)
  ; [ListOf Point] String Point -> [ListOf Point]
  ; updates the data representation after every clock tick
  ; accounting for user-chosen direction of movement and food consumption
  (cons
   (cond
     [(string=? move "up") (-pt (first snake) (make-point 0 UNITCELLSIZE))]
     [(string=? move "down") (+pt (first snake) (make-point 0 UNITCELLSIZE))]
     [(string=? move "left") (-pt (first snake) (make-point UNITCELLSIZE 0))]
     [(string=? move "right") (+pt (first snake) (make-point UNITCELLSIZE 0))])
   (cond
     [(equal? (first snake) food) snake]
     [else (tail snake)])))


(define (tail sn)
  ; Snake -> Snake
  ; modifies the tail of the snake as it moves, It's simple!
  ;     Just delete the last element in the list
  (cond
    [(empty? (rest sn)) '()]
    [else (cons (first sn) (tail (rest sn)))]))


(define (teleport-food fd sn)
  ; Point [ListOf Point] -> Point
  ; move the food to a random, empty location after the snake eats it
  (cond
    [(equal? (first sn) fd) (random-point fd sn)]
    [else fd]))


(define (random-point pt sn)
  ; Point [ListOf Point] -> Point
  ; ensures that when food teleports, it does so to an empty location
  (cond
    [(member? pt sn)
     (random-point (make-point
                    (* (+ (random (- NCELLSHORIZ 1)) 5/4) UNITCELLSIZE)
                    (* (+ (random (- NCELLSVERT 1)) 5/4) UNITCELLSIZE)) sn)]
    [else pt]))


(define (+pt p1 p2)
  ;; Point Point -> Point
  ;; add one point to another
  (make-point (+ (point-x p1) (point-x p2))
              (+ (point-y p1) (point-y p2))))


(define (-pt p1 p2)
  ;; Point Point -> Point
  ;; subtract one point from another
  (make-point (- (point-x p1) (point-x p2))
              (- (point-y p1) (point-y p2))))


(define (render-snake sn bkgd)
  ; Snake Img -> Img
  ; render the snake on the background provided
  (local (
          (define (place-segment sgmt cvs)
            ; Img Img -> Img
            ; place a snake segment on a image of a snake under construction
            (place-image SNAKESEGMENT (point-x sgmt) (point-y sgmt) cvs)))
    ; - IN -
    (foldr place-segment bkgd sn)))


(define (render-food fd)
  ; Point -> Img
  ; place an image of food at the proper location of the playing field
  (place-image FOOD
               (point-x fd)
               (point-y fd)
               FIELDOFPLAY))


(define (game-over sg)
  ; SnakeGame -> SnakeGame
  ; render a game-over screen
  (overlay
   (above
    (text "Game Over!" 48 "black")
    (beside
     (text "you achieved a snake of " 16 "black")
     (text (number->string (length (snake-game-snake sg))) 24 "black")
     (text " segments" 16 "black")))
   (overlay/align/offset
    "right" "bottom"
    (text
     (cond
       [(member? (first (snake-game-snake sg)) (rest (snake-game-snake sg)))
        "self-annihilation"]
       [else "snake hit border"])
     16 "black")
    20 15 (render-game sg))))


; ================================
; actions
(define FOODSTARTPT (random-point SNAKESTARTPT (list SNAKESTARTPT)))
(define PLAYSNAKE (make-snake-game (list SNAKESTARTPT) SNAKESTARTDIR FOODSTARTPT))

(main PLAYSNAKE)