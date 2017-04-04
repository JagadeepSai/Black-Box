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
(define atoms 4)
(define num 8) 
(define sf 0.52)
(define sht 20)

(define tile (scale sf (bitmap "a (4).png")))
(define hit (scale sf (bitmap "a (19).png")))
(define reflect (scale sf (bitmap "a (5).png")))
(define s_tile (scale sf (bitmap "t (7).jpg")))
(define atom_tile (scale sf (bitmap "a (3).png")))
(define p1  (bitmap "a (21).png"))
(define p2  (bitmap "a (16).png"))
(define p3  (bitmap "a (8).png"))
(define p4  (bitmap "a (15).png"))
(define p5  (bitmap "a (6).png"))
(define p6  (bitmap "a (12).png"))
(define p7  (bitmap "a (17).png"))
(define p8  (bitmap "a (14).png"))
(define p9  (bitmap "a (18).png"))
(define p10 (bitmap "a (13).png"))
(define p11  (bitmap "a (11).png"))
(define p12  (bitmap "a (10).png"))
(define p13  (bitmap "a (9).png"))
(define p14  (bitmap "a (7).png"))
(define pic-list (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14))

(define que (map(lambda(img) (scale sf img)) pic-list)) 
(define tile_side (image-width tile))
(define box_side (* tile_side  num))

(define offset (rectangle tile_side sht "outline" "transparent"  ))

(define box-x (- 670 (* (/ num 2) tile_side)))
(define box-y (- 350 (* (/ num 2) tile_side)))
;these two are the (x y) of the top left of box 
(define origin
  (posn (+ (/ tile_side 2) box-x)
     (+ box-y (/ tile_side 2))))
;origin is the center of the first tile
 (define (make_box x1 y1 side)
   (box x1 y1 (+ x1 side) (+ y1 side)))
(define main_box
  (make_box box-x box-y box_side))
;(define (rect_box x y w h)
;  (box x y (+ x w) (+ y h)))
;(define 1_box
;  (rect_box box-x (- box-y (+ sht tile_side))
;         box_side tile_side))
;(define 2_box
;  (rect_box (+ box-x sht box_side) box-y
;            tile_side box_side))
;(define 3_box
;  (rect_box (

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
(define max-posn
  (let ([ side (+ sht tile_side)])
    (posn (- box-x side) (- box-y side))))

(define max_box
  (make_box (posn-x max-posn) (posn-y max-posn)
            (+ (* (+ num 2) tile_side) (* 2 sht))))

; the max box enclosing
(define obox
  (make_box (- box-x sht)
            (- box-y sht)
            (+ (* num tile_side) (* 2 sht))))



(define (one-true list pos)
  (if (null? list) (if (equal? (cdr pos) 1) (car pos) #f)
      (if (equal? (car list) #t)
          (if (equal? (cdr pos) 0)
              (one-true (cdr list) (cons (car pos) 1))
              #f)
          (if (= (cdr pos) 1) (one-true (cdr list) (cons (car pos) 1))
          (one-true (cdr list) (cons (+ (car pos) 1) 0))))))

(define (s_pile x y)
  (let* ( [ c1 (< y box-y)]
          [ c2 (> x (+ box-x box_side))]
          [ c3 (> y (+ box-y box_side))]
          [ c4 (< x box-x)] )
      (one-true (list c1 c2 c3 c4) (cons 0 0))))

(define (in_side? x y)

  (if (and (not (equal? (s_pile x y) #f))
        (check_in? max_box x y)
           (not (check_in? obox x y))) #t #f))
;---------------------------------------------------------------------------------------------
 (define (side_tile n x y)
   ( if (or (= n 0) (= n 2)) (cons n (floor (/ (- x (box-x1 main_box)) tile_side)) )
        (cons n (floor (/ (- y (box-y1 main_box)) tile_side)) )))
(define (side_tile_finder end_ray)
  (if (= (ray-dir end_ray) 1)
      (if (= (ray-sign end_ray) 0) (cons 0 (car (ray-tile end_ray)))
                                   (cons 2 (car (ray-tile end_ray))))
      
      (if (= (ray-sign end_ray) 0) (cons 3 (cdr (ray-tile end_ray)))
                                   (cons 1 (cdr (ray-tile end_ray))))))


(define (tile_to_st_ray side_tile) ;checked 
  (let ([a  (car side_tile) ])
    (cond [(= a 0) (ray 1 1 (cons (cdr side_tile) 0))]
          [(= a 1) (ray 0 0 (cons (- num 1) (cdr side_tile)))]
          [(= a 2) (ray 1 0 (cons (cdr side_tile) (- num 1)))]
          [(= a 3) (ray 0 1 (cons 0 (cdr side_tile)))]
          [else #f] )))
  ; only use this for side tiles

(define (side_center side_tile)
  (let ([ a (car side_tile)])
    (cond [(= a 0) (posn (+ (posn-x origin) (* tile_side (cdr side_tile))) (- box-y (+ sht (/ tile_side 2))) )]
          [(= a 1) (posn (+ box-x box_side  sht (/ tile_side 2)) (+ (posn-y origin) (* tile_side (cdr side_tile))))]
          [(= a 2) (posn (+ (posn-x origin) (* tile_side (cdr side_tile))) (+ box-y box_side sht (/ tile_side 2)))]
          [(= a 3) (posn (- box-x (+ sht (/ tile_side 2))) (+ (posn-y origin) (* tile_side (cdr side_tile))))]))) 

                                                            
      
;checks whether point is along the sides not whether it is in the box 
 


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
  (if (= (ray-dir st_ray) 1) (lambda(x)(car x))
                                        (lambda(x)(cdr x)) ))
;distance of a atom perpendicular to the ray
(define (parl st_ray)
  (if (= (ray-dir st_ray) 0) (lambda(x)(car x))
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
           [sp2  (begin (cond [ (equal? atom (cons 2 2)) (set! a1  (chk1 (ray-tile st_ray)))  ])
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

;(define (hit2  near_atom_list)
 ; (if (= (length near_atom_list) 2)
  ;      (if (= (abs ( - (chk1 atom) (chk1 ))) 1)

      
(define far_atom (cons (list (cons +inf.0 +inf.0)) +inf.0))

(define (rules st_ray atom)
  (if (null? atom) st_ray
  (let* ([chk1  (perd st_ray)]
        [chk2 (parl st_ray)]
        [per_dist (- (chk1 atom) (chk1 (ray-tile st_ray)))]
        [par_dist (- (chk2 atom) (chk2 (ray-tile st_ray)))])

(define (beside st_ray atom) ;One way of reflection
  ( if (and (= (abs per_dist) 1)
            (= (abs par_dist) 0))
       #\r st_ray ))
;beside tested
(define (deflect st_ray atom)
  (if (= (abs per_dist) 1)
    ( let* ([ st1_ray (begin (set! a2 (- (chk1 atom) per_dist))
                             (- (chk1 atom) per_dist) )]
            [ st2_ray (+ (chk2 atom)
                         (if (= (ray-sign st_ray) 0) 1 -1))]
            [st_ray_up  (if (= (chk1 (cons 1 0)) 1)
                            (cons st1_ray st2_ray)
                            (cons st2_ray st1_ray)) ]
            [o_dir (if (= (ray-dir st_ray) 1) 0 1)]
            )
           (if (> per_dist 0)
               (ray o_dir 0 st_ray_up)
               (ray o_dir 1 st_ray_up))
               )
    st_ray ))
    ;deflect working fine
    ;Testcases
    
   #| (define a (int_atom (ray 0 1 (cons 0 3)) (list (cons 0 1) (cons 1 4) (cons 2 2) (cons 3 3)) (cons (list (cons 20 20)) 20) ))
> (rules (ray 0 1 (cons 0 3)) (car (car a))) 
(ray 1 0 '(0 . 3))
> (define a (int_atom (ray 0 1 (cons 0 3)) (list (cons 0 1) (cons 1 2) (cons 2 2) (cons 3 3)) (cons (list (cons 20 20)) 20) ))
> (rules (ray 0 1 (cons 0 3)) (car (car a))) 
(ray 1 1 '(0 . 3))
> (define a (int_atom (ray 0 1 (cons 0 3)) (list (cons 0 1)  (cons 2 2) (cons 3 3)) (cons (list (cons 20 20)) 20) ))
> (rules (ray 0 1 (cons 0 3)) (car (car a))) 
(ray 1 1 '(1 . 3))
> |#
              
(define (hit st_ray atom)
  (if (= per_dist 0) #\h st_ray))
;Hit tested 
    ;Testing
    (define a (beside st_ray atom ))
   (if (not (equal? a st_ray)) a
       (let ([b (hit st_ray atom) ] )
         (if (not (equal? b st_ray)) b
             (let ([c (deflect st_ray atom) ] )
            (if (not (equal? c st_ray)) c st_ray))))))) )
             
(define (end_ray st_ray)
  (let ( [ pos (ray-tile st_ray)] )
     (if (= (ray-dir st_ray) 1) 
        (if (= (ray-sign st_ray) 0) (ray 1 0 (cons (car pos) 0))
                                    (ray 1 1 (cons (car pos) num)))

        (if (= (ray-sign st_ray) 0) (ray 0 0  (cons 0 (cdr pos) ))
                                    (ray 0 1  (cons num (cdr pos) )))
        )))
; Moves straight ; Tested



(define c_int_st 0)   
(define (interact st_ray st_atom_list)
   (let* ([ int_atoms (car (int_atom st_ray st_atom_list far_atom)) ]
         [new_st_ray  (begin (map (lambda(atom) (set! c_int_st (rules st_ray atom))) int_atoms) c_int_st )])
     (define (final new_st_ray)
  (lambda (st_ray)
    (if (equal? new_st_ray #\h) #\h
      (if (equal? new_st_ray #\r) #\r
          (if (equal? new_st_ray st_ray)
              (end_ray new_st_ray)
              (interact new_st_ray st_atom_list) )))))
       ((final new_st_ray) st_ray)))
     ;(if (equal? new_st_ray st_ray) (end_ray new_st_ray)
      ;   (interact new_st_ray st_atom_list))))


;(define 
;



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

