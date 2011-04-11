#lang class5

(require class5/universe)
(require 2htdp/image)
(require (only-in racket begin write))

(define BOARD-SCALE 100)
(define WIDTH 7)
(define HEIGHT 5)
(define colors (list "purple" "blue" "green" "red" "yellow"
          "pink" "orange" "teal" "lightblue" "violet"))
(define sample-board
  '((1 ((0 2) (0 3) (1 2) (1 3)))
    (2 ((0 0) (0 1)))
    (2 ((1 4) (2 4) (2 3)))
    (1 ((2 0) (3 0) (4 0) (5 0) (5 1)))
    (1 ((1 1) (2 1) (2 2) (3 2)))
    (2 ((3 3) (4 3) (4 2) (5 2) (6 2)))
    (2 ((6 0) (6 1)))
    (1 ((4 4) (5 4) (5 3) (6 4) (6 3)))))
(define sample-serial
  '(((0 "Eric") (1 "Francis"))
    (((1 2 4) (0 "Eric") 1)
     ((0 4) (1 "Francis") 1)
     ((0 4 5) (1 "Francis") 1)
     ((4 5 6) (0 "Eric") 3)
     ((0 1 2 3 5) (0 "Eric") 3)
     ((2 3 4 6 7) (1 "Francis") 2)
     ((3 5) (1 "Francis") 1)
     ((5) (0 "Eric") 1))))

(define-class client%
  (fields name id serialboard boardrep aroll droll)
  
  (define/public (on-receive msg)
    ; s-expression message from the server
    (cond [(and (symbol? msg)
                (symbol=? msg 'turn)) empty ] ; stuff
          [(and (symbol? msg)
                (symbol=? msg 'illegal))
           this]
          [(and (symbol? msg)
                (symbol=? msg 'error))
           this]
          [(symbol=? (first msg) 'attack) empty ] ; stuff
          [(symbol=? (first msg) 'new-state) 
           (client% (field name) (field id)
                   (second msg) (field boardrep)
                   empty empty)]
          [(symbol=? (first msg) 'start)
           (client% "" (second msg)
                   (third msg) (fourth msg)
                   empty empty)]))
  
  ; Number Number MouseEvent -> Package
  (define/public (on-mouse x y m)
    this)
  
  (define/public (grid->pixel g)
    (* BOARD-SCALE (+ 1/2 g)))
  
  (define/public (outline-top gridr lobp bpos)
    (if (ormap (λ (g) (= (sub1 (second bpos))
                         (second g)))
                lobp)
        gridr
        (add-line gridr 
                  (* BOARD-SCALE (first bpos))
                  (* BOARD-SCALE (second bpos))
                  (+ BOARD-SCALE (* BOARD-SCALE (first bpos)))
                  (* BOARD-SCALE (second bpos))
                  "black")))
  
  (define/public (outline-left gridr lobp bpos)
    (if (ormap (λ (g) (= (sub1 (first bpos))
                         (first g)))
                lobp)
        gridr
        (add-line gridr 
                  (* BOARD-SCALE (first bpos))
                  (* BOARD-SCALE (second bpos))
                  (* BOARD-SCALE (first bpos))
                  (+ BOARD-SCALE (* BOARD-SCALE (second bpos)))
                  "black")))
  
  (define/public (outline-right gridr lobp bpos)
    (if (ormap (λ (g) (= (add1 (first bpos))
                         (first g)))
                lobp)
        gridr
        (add-line gridr 
                  (+ BOARD-SCALE (* BOARD-SCALE (first bpos)))
                  (* BOARD-SCALE (second bpos))
                  (+ BOARD-SCALE (* BOARD-SCALE (first bpos)))
                  (+ BOARD-SCALE (* BOARD-SCALE (second bpos)))
                  "black")))
  
  (define/public (outline-bottom gridr lobp bpos)
    (if (ormap (λ (g) (= (add1 (second bpos))
                         (second g)))
                lobp)
        gridr
        (add-line gridr 
                  (* BOARD-SCALE (first bpos))
                  (+ BOARD-SCALE (* BOARD-SCALE (second bpos)))
                  (+ BOARD-SCALE (* BOARD-SCALE (first bpos)))
                  (+ BOARD-SCALE (* BOARD-SCALE (second bpos)))
                  "black")))
  
  ; Scene [Listof BPosn] BPosn
  ; Outlines the given BPosn given its neighbors
  (define/public (outline gridr lobp bpos)
    (outline-top 
     (outline-bottom 
      (outline-left 
       (outline-right gridr lobp bpos)
       lobp bpos) lobp bpos) lobp bpos))
  
  (define/public (list-pos lst p)
    (local [(define (lp-helper lst p counter)
              (cond [(empty? lst) -1]
                    [(equal? p (first lst)) counter]
                    [else (lp-helper (rest lst) p (add1 counter))]))]
      (lp-helper lst p 0)))
  
  ; [Listof BPosn] Scene Color -> Scene
  ; Draws the region (listof bposn) onto the scene
  (define/public (draw-region lobp s color)
    (foldr 
     (λ (r s)
       (outline
         (place-image
          (rectangle BOARD-SCALE
                     BOARD-SCALE
                     "solid" color)
          (grid->pixel (first r))
          (grid->pixel (second r)) s)
        lobp r))
     s lobp))
  
  (define/public (to-draw)
    (foldr (λ (i s) (overlay/xy
                     (text (number->string 
                            (third 
                             (list-ref 
                              (second (field serialboard))
                              (list-pos (field boardrep) i))))
                           24 "black")
                     (* -1 (grid->pixel (first (first (second i)))))
                     (* -1 (grid->pixel (second (first (second i)))))
                     (place-image
                      (draw-region (cadr i) s (list-ref colors (first i)))
                      (/ (* BOARD-SCALE WIDTH) 2)
                      (/ (* BOARD-SCALE HEIGHT) 2) s)))
           (empty-scene (* BOARD-SCALE WIDTH) (* BOARD-SCALE HEIGHT))
           (field boardrep))))

(define base-client (client% "" empty sample-serial sample-board empty empty))

(big-bang base-client)