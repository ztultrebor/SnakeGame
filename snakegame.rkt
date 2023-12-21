;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snakegame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)



; constants

(define SNAKECOLOR "red")
(define BACKGROUNDCOLOR "white")
(define SEGMENTSIZE 15)
(define CURVATURE 4)
(define PULLBACK (- SEGMENTSIZE CURVATURE CURVATURE))
(define SPACER 1)
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


(beside (above SNAKESEGMENT SNAKESEGMENT SNAKESEGMENT)
        (above SNAKESEGMENT SNAKESEGMENT SNAKESEGMENT)
        (above SNAKESEGMENT SNAKESEGMENT SNAKESEGMENT))

