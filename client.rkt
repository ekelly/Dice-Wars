#lang class5

(require class5/universe)
(require 2htdp/image)

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

(define-class client%
  (fields name id serialboard boardrep aroll droll)
  
  (define/public (on-receive msg)
    ; s-expression message to the server
    (cond [(and (symbol? msg)
                (symbol=? msg 'turn)) empty ] ; stuff
          [(and (symbol? msg)
                (symbol=? msg 'illegal))
           (client% (field name) (field id)
                   (field serialboard) (field boardrep)
                   empty empty)]
          [(and (symbol? msg)
                (symbol=? msg 'error))
           (client% (field name) (field id)
                   (field serialboard) (field boardrep)
                   empty empty)]
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
           
  ; -> Client
  ; client does something illegal
  (define/public (illegal)
    this)
  
  ; -> Client
  ; Respond to an error 
  (define/public (error)
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
    (foldr (λ (i s) (place-image 
                     (draw-region (cadr i) s (list-ref colors (first i)))
                                 (/ (* BOARD-SCALE WIDTH) 2)
                                 (/ (* BOARD-SCALE HEIGHT) 2) s))
           (empty-scene (* BOARD-SCALE WIDTH) (* BOARD-SCALE HEIGHT))
           (field boardrep))))

(big-bang (client% "" empty empty sample-board empty empty))