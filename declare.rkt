#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(provide (all-defined-out))
;-------------------------------------------------------------------------------
(define-struct dot (a b c d num) #:transparent)
(define-struct posn (x y) #:transparent)
(define-struct box (x1 y1 x2 y2) #:transparent)
(define-struct ray (dir sign tile) #:transparent)

;-------------------------------------------------------------------------------
(define back (scale/xy (/ 1340 1280) (/ 700 800) (bitmap "b2.jpg")))
  ;(rectangle 1340 700 "solid" "white"))
(define atoms 5)
(define num 6) 
(define sf 0.52)

(define tile (scale sf (bitmap "a (4).png")))
(define hit (scale sf (bitmap "a (19).png")))
(define reflect (scale sf (bitmap "a (5).png")))
(define s_tile (scale sf (bitmap "t (7).jpg")))
(define atom_tile (scale sf (bitmap "a (3).png")))


(define tile_side (image-width tile))
(define box_side (* tile_side  num))

(define offset (rectangle tile_side 20 "outline" "transparent"  ))

(define box-x (- 670 (* (/ num 2) tile_side)))
(define box-y (- 350 (* (/ num 2) tile_side)))
;these two are the (x y) of the top left of box 
(define origin
  (posn (+ (/ tile_side 2) box-x)
     (+ box-y (/ tile_side 2))))
;origin is the center of the first tile
                    
(define main_box
  (box box-x box-y (+ box-x box_side) (+ box-y box_side))) 
;the Whole box
(define (check_in? box x y)
 (and (and(<= x (box-x2 box)) (>= x (box-x1 box)))
      (and(<= y (box-y2 box)) (>= y (box-y1 box)))))
;Given a box it checks whether the point is in it 
(define (tile_finder x y )
  (cons (floor (/ (- x (box-x1 main_box)) tile_side))
        (floor (/ (- y (box-y1 main_box)) tile_side))))
;finds the tile given the x y
(define (cen_tile tile)
  (posn (+ (posn-x origin) (* (car tile) tile_side))
        (+ (posn-y origin) (* (cdr tile) tile_side))) )
; Given a tile finds the center of it
(define (all_check_atom st_atom con_tile)
  (let* ( [l (car st_atom)]
          [count (cdr st_atom)])

    (define (check_atom? con_tile st_atom_list)
           (if(null? st_atom_list) #t 
                (if (equal? con_tile (car st_atom_list)) #f
                          (check_atom? con_tile (cdr st_atom_list))))) 

    (if (check_atom? con_tile l)
        (if (not (= count atoms)) 
           (cons (cons (cons  con_tile l) (+ count 1)) 1)
           (cons st_atom 0))

        (cons (cons (remove con_tile l) (- count 1)) -1))))
;This returns the update for the state of atoms and a number to say it 
(define (perd st_ray)
  (if (not (ray-dir st_ray)) (lambda(x)(car x))
                                        (lambda(x)(cdr x)) ))
;distance of a atom perpendicular to the ray
(define (parl st_ray)
  (if (ray-dir st_ray) (lambda(x)(car x))
                                        (lambda(x)(cdr x)) ))
;function to distance of a atom parallel to the ray

(define a1 0)
(define a2 0)
(define a3 0)
; for debuging
(define (int_atom  st_ray atom_list near_atoms)
  (if (null? atom_list ) near_atoms
    (let* ( [chk1 (perd st_ray)]
            ;In the direction perpendicular
            [chk2 (parl st_ray) ]
            ;In the direction parallel
           [sp (if (= (ray-sign st_ray) 1) 1 -1)]
           [atom (car atom_list) ]
           [away (< (abs ( - (chk1 atom) (chk1 (ray-tile st_ray)))) 2)]
           [dist ( - (chk2 atom) (chk2 (ray-tile st_ray))) ]
           [sp2  (begin (cond [ (equal? atom (cons 3 4)) (set! a1  sp)  ])
                            (* dist sp) ) ] ; for checking the internal value remove it later
           [near_atom (car (car near_atoms))]
           [go  (int_atom st_ray (cdr atom_list) near_atoms)] )
      (if away
       (if (>= sp2 0)
           (if (< (abs dist) (cdr near_atoms))
               (int_atom st_ray (cdr atom_list) (cons (list (car atom_list)) (abs dist)))
                (if (= (abs dist) (cdr near_atoms))

                    (int_atom st_ray (cdr atom_list)
                              (cons (cons (car atom_list) (car near_atoms)) (abs dist)))
                    go)) go) go))))

(define (rules st_ray atom)
  (let* ([chk1  (perd st_ray)]
        [chk2 (parl st_ray)]
        [per_dist (- (chk1 atom) (chk1 (ray-tile st_ray)))]
        [par_dist (- (chk2 atom) (chk2 (ray-tile st_ray)))])
    
(define (beside st_ray atom)
  ( if (and (= (abs per_dist) 1)
            (= (abs par_dist) 0))
       #\r #f ))

(define (deflect st_ray atom)
  (if (= (abs per_dist) 1)
    ( let* ([ st1_ray (+ (chk1 atom) per_dist)]
            [ st2_ray (+ (chk2 atom)
                         (if (= ray-sign st_ray 0) 1 -1))]
            [st_ray_up  (if (= (chk1 (cons 1 0)) 1)
                            (cons st1_ray st2_ray)
                            (cons st2_ray st1_ray)) ]
            [o_dir (if (= (ray-dir st_ray) 1) 0 1)]
            )
           (if (> per_dist 0)
               (ray o_dir 1 st_ray_up)
               (ray o_dir 0 st_ray_up))
               )
    #f ))
              
(define (hit st_ray atom)
  (if (= per_dist 0) #\h #f )) #t))
 
;(define (interact st_ray st_atom_list)
  



 (define (place how img n)
   (define (loop srt ans)
     (if (= srt n) ans
         (loop (+ srt 1) (how ans img))))
   (loop 1 img))

(define (row img n) (place beside img n))
(define (column img n) (place above img n))
;(define (row img n ans)
;     (if (= n 10) ans
;     (row img (+ n 1) (beside ans img))))
;
;(define (column img n ans)
;  (if (= n 8) ans
;      (column img (+ n 1) (above ans img))))

(define s_row (row s_tile num))
(define tile_row (row tile num))
(define off-row (row offset num))
(define tile_box (column tile_row num))


(define (surround img_row obj)
   (let* ( (top img_row)
           (side (rotate 90 img_row))
           (main obj))
     (beside (beside side (above top (above main top))) side)))
(define off_box (surround off-row tile_box))
(define final_box (surround s_row off_box))
              
(define background (place-image final_box 670 350 back))

