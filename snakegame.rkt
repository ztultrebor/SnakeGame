;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snakegame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


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


; A Snake is a ListOfPoints
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

; constants

(define SNAKECOLOR "red")
(define BACKGROUNDCOLOR "white")
(define UNITCELLSIZE 16)
(define SPACER 1)
(define SEGMENTSIZE (- UNITCELLSIZE SPACER))
(define CURVATURE 4)
(define PULLBACK (- SEGMENTSIZE CURVATURE CURVATURE))
(define CANVASWIDTH (* 80 UNITCELLSIZE))
(define CANVASHEIGHT (* 46 UNITCELLSIZE))
(define NCELLSHORIZ (/ CANVASWIDTH UNITCELLSIZE))
(define NCELLSVERT (/ CANVASHEIGHT UNITCELLSIZE))
(define SNAKESTARTPT (make-point
                      (* (quotient (+ NCELLSHORIZ UNITCELLSIZE) 2) UNITCELLSIZE)
                      (* (quotient (+ NCELLSVERT UNITCELLSIZE) 2) UNITCELLSIZE)))
(define SNAKESTARTDIR "right")
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
(define FOOD (circle 7 "solid" "green"))
(define CANVAS  (empty-scene CANVASWIDTH CANVASHEIGHT BACKGROUNDCOLOR))
(define FIELDOFPLAY
  (beside
   (rectangle (/ UNITCELLSIZE 4) (+ CANVASHEIGHT (/ UNITCELLSIZE 2))
              "solid" "blue")
   (above
    (rectangle CANVASWIDTH (/ UNITCELLSIZE 4) "solid" "blue")
    CANVAS
    (rectangle CANVASWIDTH (/ UNITCELLSIZE 4) "solid" "blue"))
   (rectangle (/ UNITCELLSIZE 4) (+ CANVASHEIGHT (/ UNITCELLSIZE 2))
              "solid" "blue")))


; test constants
(define TESTSTARTPT (list
                     (make-point (* (quotient NCELLSHORIZ 2) UNITCELLSIZE)
                                 (* (quotient NCELLSVERT 2) UNITCELLSIZE))
                     (make-point (* (- (quotient NCELLSHORIZ 2) 1) UNITCELLSIZE)
                                 (* (quotient NCELLSVERT 2) UNITCELLSIZE))
                     (make-point (* (- (quotient NCELLSHORIZ 2) 2) UNITCELLSIZE)
                                 (* (quotient NCELLSVERT 2) UNITCELLSIZE))
                     (make-point (* (- (quotient NCELLSHORIZ 2) 3) UNITCELLSIZE)
                                 (* (quotient NCELLSVERT 2) UNITCELLSIZE))
                     (make-point (* (- (quotient NCELLSHORIZ 2) 4) UNITCELLSIZE)
                                 (* (quotient NCELLSVERT 2) UNITCELLSIZE))
                     (make-point (* (- (quotient NCELLSHORIZ 2) 5) UNITCELLSIZE)
                                 (* (quotient NCELLSVERT 2) UNITCELLSIZE))
                     (make-point (* (- (quotient NCELLSHORIZ 2) 6) UNITCELLSIZE)
                                 (* (quotient NCELLSVERT 2) UNITCELLSIZE))
                     (make-point (* (- (quotient NCELLSHORIZ 2) 7) UNITCELLSIZE)
                                 (* (quotient NCELLSVERT 2) UNITCELLSIZE))))



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
   (cons
    (cond
      [(string=? (snake-game-mvmt sg) "up")
       (make-point (point-x (first (snake-game-snake sg)))
                   (- (point-y (first (snake-game-snake sg))) UNITCELLSIZE))]
      [(string=? (snake-game-mvmt sg) "down")
       (make-point (point-x (first (snake-game-snake sg)))
                   (+ (point-y (first (snake-game-snake sg))) UNITCELLSIZE))]
      [(string=? (snake-game-mvmt sg) "left")
       (make-point (-  (point-x (first (snake-game-snake sg))) UNITCELLSIZE)
                   (point-y (first (snake-game-snake sg))))]
      [(string=? (snake-game-mvmt sg) "right")
       (make-point (+  (point-x (first (snake-game-snake sg))) UNITCELLSIZE)
                   (point-y (first (snake-game-snake sg))))])
    (cond
      [(equal? (first (snake-game-snake sg)) (snake-game-food sg))
       (snake-game-snake sg)]
      [else (tail (snake-game-snake sg))]))
    (snake-game-mvmt sg)
    (snake-game-food sg)))


  (define (tail sn)
    ; Snake -> Snake
    ; modifies the tail of the snake as it moves, It's simle!
    ;     Just delete the last element in the list
    (cond
      [(empty? (rest sn)) '()]
      [else (cons (first sn) (tail (rest sn)))]))


  (define (render-game sg)
    ; SnakeGame -> SnakeGame
    ; render the state of the game on screen
    (place-image FOOD
                 (point-x (snake-game-food sg))
                 (point-y (snake-game-food sg))
                 (render-snake (snake-game-snake sg))))


  (define (turn-snake sg ke)
    ; SnakeGame -> SnakeGame
    ; turns the snake using the keyboard
    (make-snake-game
     (snake-game-snake sg)
     (cond
       [(or
         (and (key=? "up" ke) (string=? (snake-game-mvmt sg) "down"))
         (and (key=? "down" ke) (string=? (snake-game-mvmt sg) "up"))
         (and (key=? "left" ke) (string=? (snake-game-mvmt sg) "right"))
         (and (key=? "right" ke) (string=? (snake-game-mvmt sg) "left")))
        (snake-game-mvmt sg)]
       [(key=? "up" ke) "up"]
       [(key=? "down" ke) "down"]
       [(key=? "left" ke) "left"]
       [(key=? "right" ke) "right"]
       [else (snake-game-mvmt sg)])
     (snake-game-food sg)))


  (define (crashed? sg)
    ; !!!
    ; SnakeGame -> Boolean
    ; returns #t when the snake crashes out
    (or
     (< (point-x (first (snake-game-snake sg))) (/ UNITCELLSIZE 2))
     (> (point-x (first (snake-game-snake sg)))
        (- CANVASWIDTH (/ UNITCELLSIZE 2)))
     (< (point-y (first (snake-game-snake sg))) (/ UNITCELLSIZE 2))
     (> (point-y (first (snake-game-snake sg)))
        (- CANVASHEIGHT (/ UNITCELLSIZE 2)))
     (member? (first (snake-game-snake sg)) (rest (snake-game-snake sg)))))


  (define (render-snake sn)
    ; SnakeGame -> SnakeGame
    ; render the state of the game on screen
    (cond
      [(empty? sn) FIELDOFPLAY]
      [else (place-image SNAKESEGMENT
                         (point-x (first sn))
                         (point-y (first sn))
                         (render-snake (rest sn)))]))


  (define (game-over sg)
    ; SnakeGame -> SnakeGame
    ; render a game-over screen
    (overlay
     (text "Game Over!" 48 "black")   
     (overlay/align/offset
      "right" "bottom"
      (text
       (cond
         [(member? (first (snake-game-snake sg)) (rest (snake-game-snake sg)))
          "self-annihilation"]
         [else "snake hit border"])
       16 "black")
      20 15 (render-game sg))))

  ; actions


  (define PLAYSNAKE (make-snake-game (list SNAKESTARTPT) SNAKESTARTDIR SNAKESTARTPT))
  (define TESTSNAKE (make-snake-game TESTSTARTPT SNAKESTARTDIR SNAKESTARTPT))


  (main PLAYSNAKE)

 ; (main TESTSNAKE)
  