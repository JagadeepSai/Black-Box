#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "declare.rkt")


(define st_atom (cons (list ) 0))
(define ray_tile (cons 1 1))
(define buf-que que)

(define c_img p1)
;(define st_ray  (ray (cons

(define fix_atom #|(list ( cons 4  1) (cons 4  0) (cons 4  2) (cons 0  3))) |# (list(cons (random num) (random num)) (cons (random num) (random num))
                           (cons (random num) (random num)) (cons (random num) (random num)) ))

(define f  background)
(define g  (lambda(centre f img)
             (place-image img (posn-x centre)
                                    (posn-y centre)  f)))
(define (click f x y me)
  (if (mouse=? me "button-down")
      (if (equal? (in_side? x y) #t)
   (let* ([n (s_pile x y)]
            [s_tile (side_tile n x y) ]
            [enter_ray (tile_to_st_ray s_tile)]
            [end_ray (interact enter_ray fix_atom )]
            
      [ ans
       (if (equal? end_ray #\h ) (list 1 hit (side_center s_tile) )
           (if (or (equal? end_ray #\r)
                   (equal? s_tile (side_tile_finder end_ray)))
               (list 1 reflect (side_center s_tile) )
               (list 2 (begin (set! c_img (car buf-que)) (set! buf-que (cdr buf-que)) c_img)

                              (side_center s_tile) (side_center (side_tile_finder end_ray)))))]) 
       (if (= (car ans) 1) (g (caddr ans) f  (cadr ans))
           (g (caddr ans) (g (cadddr ans) f (cadr ans)) (cadr ans))))
           
         ; (g (side_center (cons 3 2)) f p1) ;change this 
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
                       (g cen_pos f tile) )) f)))) f) ) f ))
                       
        
;(define (coord x y ) 
 ;(define (tile->cord tile_no)
  ;  (
(big-bang background
          [to-draw (lambda (x) x)]
          [on-mouse click] )

