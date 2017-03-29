#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "declare.rkt")


(define st_atom (cons (list ) 0))
(define ray_tile (cons 1 1))
;(define st_ray  (ray (cons 

(define f  background)
(define g  (lambda(centre f img)
             (place-image img (posn-x centre)
                                    (posn-y centre)  f)))
(define (click f x y me)
  (if (mouse=? me "button-down")
      (if (not (equal? (in_side? x y) #t))
            (g (posn 50 50) f hit) ;change this 
     (if (check_in? main_box x y)
       (let* ( [con_tile (tile_finder x y) ]  
               [next (all_check_atom st_atom con_tile) ]
               [con (cdr next) ] )
        (if (= con 0) f
        (if (= con 1)
                (begin (set! st_atom (car next)) 
                       (let ( [cen_pos (cen_tile con_tile) ])
                       (g cen_pos f atom_tile) ))
        (if (= con -1)
                (begin (set! st_atom (car next)) 
                       (let ( [cen_pos (cen_tile con_tile) ])
                       (g cen_pos f tile) )) f)))) f)) f))   
                       
        
;(define (coord x y ) 
 ;(define (tile->cord tile_no)
  ;  (
(big-bang background
          [to-draw (lambda (x) x)]
          [on-mouse click] )

