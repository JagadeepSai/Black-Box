#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "declare.rkt")

(define f  background)
(define g  (place-image hit 670 350 background))
(define (click f x y me)
  (if (and (< x 100) (< y 100))
                 (if (mouse=? me "button-down")
                     g f) f))

(big-bang background
          [to-draw (lambda (x) x)]
          [on-mouse click] )

