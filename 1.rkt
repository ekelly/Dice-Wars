#lang class5

(require class5/universe)
(require (only-in racket/list shuffle))

#|

A Player is (player% Number Name IWorld)
The Number is the player number that the server uses to identify the player.
The Name is an arbitrary String.
The IWorld is the world that this player controls.

|#

(define-class player%
  (fields id name iworld)
  
  ; A SerialPlayer is (List Number String)
  ; IWorlds are not useful off of the server.  
  
  ; Serializes the player
  ; -> SerialPlayer
  (define/public (serialize)
    (list (field id) (field name)))
  
  ; Changes the name
  ; String ->
  (define/public (change-name! n)
    (set-field! name n)))

(define player1 (player% 0 "" iworld1))
(define player2 (player% 1 "" iworld2))
(check-expect (player1 . serialize) (list 0 ""))
(check-expect (begin (player1 . change-name! "Olin")
                            (player1 . name))
                     "Olin")
(check-expect (begin (player1 . change-name! "")
                            (player1 . name))
              "")

#|
 
A Region is (region% [Listof Region] Player Number)
Interp: the first list is the neighbor regions, as indexes into the list of
regions in the World.  The Number is the quantity of dice.

|#

(define MAXDICE 10)

(define-class region%
  (fields neighbors player dice)
  
  ; -> [Listof Number]
  (define/public (roll)
    (local [(define (roll-helper d acc)
              (if (= d 0)
                  acc
                  (roll-helper (- d 1)
                               (cons (add1 (random 6))
                                     acc))))]
      (roll-helper (field dice) empty)))
  
  ; -> Number
  ; Returns the number of possible dice that you can add
  (define/public (dice-left)
    (- MAXDICE (field dice)))
  
  ; Number -> 
  (define/public (add-dice! n)
    (set-field! dice (+ (field dice) n)))
  
  ; [listof Region] ->
  ; Sets the neighbors to the given list of regions
  (define/public (set-neighbors! lor)
    (set-field! neighbors lor))
  
  ; Number -> 
  (define/public (set-dice! d)
    (set-field! dice d))
  
  ; Player ->
  (define/public (set-player! p)
    (set-field! player p)))

;; Tests

(define region1 (region% empty player1 1))
(define region2 (region% empty player1 1))
(define region3 (region% empty player2 2))
(region1 . set-neighbors! (list region2))
(region2 . set-neighbors! (list region1 region3))
(region3 . set-neighbors! (list region2))

(check-expect (region3 . dice-left) 8)
(region2 . add-dice! 3)
(check-expect (region2 . dice) 4)
(check-expect (region1 . dice-left) 9)
(check-expect (< 0 (first (region1 . roll)) 7) true)
(check-expect (begin (region1 . set-dice! 2)
                     (region1 . dice)) 2)
(check-expect (begin (region1 . set-dice! 1)
                     (region1 . dice)) 1)
(check-expect (begin (region1 . set-player! player2)
                     (eq? (region1 . player)
                          player2)) true)
(check-expect (begin (region1 . set-player! player1)
                     (eq? (region1 . player)
                          player1)) true)
              


(define WIDTH 7)
(define HEIGHT 5)
(define MAXPLAYERS 2)
;; A BPosn is a (list [0,WIDTH) [0,HEIGHT))

;; A BoardRep is a [Listof (list Number [Listof BPosn])]

;; Interp: a BoardRep is a list of territories, where each
;; territory is owned by a player (indicated by number)
;; and consists of a set of continguous board positions.

;; Invariants:
;; - Each list of positions is disjoint from the other lists.
;; - Each list of positions is contiguous.
;; - Each list of positions consists of distinct elements.
;; - All of territories are contiguous.

(define sample-board
  '((1 ((0 2) (0 3) (1 2) (1 3)))
    (2 ((0 0) (0 1)))
    (2 ((1 4) (2 4) (2 3)))
    (1 ((2 0) (3 0) (4 0) (5 0) (5 1)))
    (1 ((1 1) (2 1) (2 2) (3 2)))
    (2 ((3 3) (4 3) (4 2) (5 2) (6 2)))
    (2 ((6 0) (6 1)))
    (1 ((4 4) (5 4) (5 3) (6 4) (6 3)))))

;; -----------------------------------------------------------------------------
;; SERVER

#|

A World is (world% [Listof Player] [Listof Region] [Listof Player])
Interp: The first list is the current players, in the order that they take their
turn.
The second list is the regions on the board.  The order must be maintained.
The third list is the players that are only observing.

|#

(define-class world%
  (fields players regions observers)
  
  ; iw -> Bundle
  (define/public (on-new iw)
    (cond [(< (length (field players)) (sub1 MAXPLAYERS))
           (make-bundle
            (world% (cons (player% (length (field players))
                                   "" iw) (field players))
                    (field regions)
                    (field observers))
            empty
            empty)]
          [(> (length (field players)) (sub1 MAXPLAYERS))
           (make-bundle
            (world% (field players)
                    (field regions)
                    (cons (player% (length (append (field observers)
                                                   (field players)))
                                   "" iw) 
                          (field observers)))
            (map (λ (p) (make-mail (p . iworld)
                                   (list 'start
                                         -1
                                         (list (serialplayers (field players))
                                               (serialregions (field regions)))
                                         sample-board))) 
                 (field observers))
            empty)]
          ; '(start Number SerialBoard BoardRep) tells the recieving player 
          ; that the game is starting, that they are the player indicated 
          ; by the given number, that the board is as described by 
          ; SerialBoard, and that the board layout may be drawn as BoardRep.
          [(= (length (field players)) (sub1 MAXPLAYERS))
           (make-bundle
            (world% (cons (player% (length (field players))
                                   "" iw) 
                          (field players))
                    (field regions)
                    (field observers))
            (map (λ (p) (make-mail (p . iworld)
                                   (list 'start 
                                         (p . id)
                                         (list (serialplayers (field players))
                                               (serialregions (field regions)))
                                         sample-board))) 
                 (cons (player% (length (field players))
                                "" iw)
                       (field players)))
            empty)]))
  
  (define/public (list-pos p lst)
    (local [(define (lp-helper p lst counter)
              (cond [(empty? lst) -1]
                    [(eq? p (first lst)) counter]
                    [else (lp-helper p (rest lst) (add1 counter))]))]
      (lp-helper p lst 0)))
  
  (check-expect ((world% empty empty empty) . list-pos 1 '(1 2 3)) 0)
  (check-expect ((world% empty empty empty) . list-pos 0 '(1 2 3)) -1)
  (check-expect ((world% empty empty empty) . list-pos 3 '(1 2 3)) 2)
  
  ; iw -> Bundle
  (define/public (on-disconnect iw)
    (if (< (length (field players)) MAXPLAYERS) ; if the game hasn't yet started
        (make-bundle ;simply drop the player from the list.
         (world% (filter (λ (p) (not (iworld=? (p . iworld) iw)))
                         (field players))
                 (field regions)
                 (field observers))
         empty
         empty)
        (make-bundle ;otherwise, kick everyone and restart the game
         (world% empty
                 (field regions)
                 empty)
         empty
         (append (map (λ (p) (p . iworld)) (field players))
                 (map (λ (p) (p . iworld)) (field observers))))))
  
  #|

A SerialBoard is
(List [Listof SerialPlayer]
      [Listof SerialRegion])
There is no need to send the list of observers.
 
|#
  
  ; -> [Listof Player]
  ; Returns a list with the players rotated one
  (define/public (next-player)
    (append (rest (field players))
            (list (first (field players)))))
  
  ; [listof Player] -> [Listof SerialPlayer]
  ; Returns a list of serialized players
  (define/public (serialplayers lop)
    (map (λ (sp) (sp . serialize)) lop))
  
  ; Region -> Number
  ; takes a region and returns its position in the list
  (define/public (->number r)
    (local [(define (->number-helper n lor)
              (if (eq? r (first lor))
                  n
                  (->number-helper (add1 n) (rest lor))))]
      (->number-helper 0 (field regions))))
  
  ; [Listof Region] -> [Listof SerialRegion]
  ; Serializes the list of regions
  (define/public (serialregions lor)
    (map (λ (sr) (list (map (λ (n) (->number n)) 
                            (sr . neighbors))
                       ((sr . player) . serialize)
                       (sr . dice)))
         lor))
  
  ; Player -> Number
  ; Counts the max number of adjacent territories of the given player
  (define/public (adjacent-territories p)
    (local [(define player-regions 
              (filter (λ (r) (eq? (r . player) p)) (field regions)))
            (define (adj-helper region lovr)
              (if (ormap (λ (r) (eq? region r))
                         lovr)
                  0
                  (apply max (map (λ (r) (add1 
                                          (adj-helper r (cons region lovr))))
                                  (region . neighbors)))))]
      (if (empty? player-regions) 0
          (add1 (apply max (map (λ (r) (adj-helper r (r . neighbors)))
                                player-regions))))))
  
  ; Number -> [Listof Region]
  ; (> 0 (random 2))
  ; Adds a specific number of dice to random regions
  (define/public (distribute-dice p)
    (local [(define pregions (filter (λ (r) (eq? (r . player) p))
                                     (field regions)))
            ; Number [Listof Region]
            ; Takes a number of dice and distributes random numbers of dice
            ; to random regions
            (define (distribute-helper n acc)
              (cond [(and (empty? acc)
                          (> n 0))
                     (distribute-helper n pregions)]
                    [(<= n 0) (field regions)]
                    [else (local [(define shuffled (shuffle acc))]
                            (if (> ((first shuffled) . dice-left) 0)
                                (begin ((first shuffled) . add-dice! 1)
                                       (distribute-helper (sub1 n)
                                                          shuffled))
                                (distribute-helper n (rest shuffled))))]))]
      (distribute-helper (adjacent-territories p)
                         pregions)))
  
  ; -> Bundle
  ; Creates a bundle of the next player
  (define/public (player-done)
    (local [(define dice-added-regions 
              (distribute-dice (first (field players))))]
      (make-bundle 
       (world% (next-player)
               dice-added-regions
               (field observers))
       (cons (make-mail ((first (next-player)) . iworld)
                        'turn)
             (map (λ (p) 
                    (make-mail 
                     (p . iworld)
                     (list 'new-state
                           (list 
                            (serialplayers (next-player))
                            (serialregions dice-added-regions)))))
                  (next-player)))
       empty)))
  
  
  ; IWorld S-Expr -> Bundle
  (define/public (on-msg iw m)
    (cond 
      ; player is done attacking
      [(and (symbol? m)
            (symbol=? m 'done)) (player-done)]
      ; Player is attacking
      [(and (cons? m)
            (cons? (rest m))
            (cons? (rest (rest m)))
            (symbol? (first m))
            (symbol=? (first m) 'attack)
            (number? (second m))
            (< (second m) (length (field regions)))
            (number? (third m))
            (< (third m) (length (field regions))))
       (local [(define legalmove? 
                 (and (ormap (λ (dr) (= (third m) dr))
                             (first 
                              (list-ref (serialregions (field regions)) 
                                        (second m)))) ; serialregion's neighbors
                      (> ((list-ref (field regions) (second m)) . dice) 1)))]
         (if legalmove?
             ; attack
             (attack! (second m) (third m))
             ; return illegal move
             (illegal-attack! (second m))))]
      ; player wants to change his name
      [(and (cons? m)
            (symbol? (first m))
            (symbol=? (first m) 'name)) (change-name iw (second m))]
      [else (make-bundle this
                         (list (make-mail iw 'error))
                         empty)]))
  
  ; iWorld String -> Bundle
  ; Change the name of the given iworld to the given String
  (define/public (change-name iw name)
    (begin (set-field! players 
                       (map (λ (p) 
                              (if (iworld=? iw (p . iworld))
                                  (begin (p . change-name! name)
                                         p)
                                  p))
                            (field players)))
    (make-bundle
     (world% (field players)
             (field regions)
             (field observers))
     (map (λ (p) (make-mail (p . iworld)
                            (list 'new-state
                                  (list (serialplayers (field players))
                                        (serialregions (field regions))))))
            (append (field players)
                    (field observers)))
     empty)))
  
  ; Region Region -> [Listof Region]
  ; Returns a listof regions where the attacker overran the defender
  (define/public (successful-attack! a d)
    (map (λ (r) (cond [(eq? d r) (begin (d . set-player! (a . player))
                                        (d . set-dice! (sub1 (a . dice)))
                                        d)]
                      [(eq? a r) (begin (a . set-dice! 1)
                                        a)]
                      [else r]))
         (field regions)))
  
  ; Region Region -> [Listof Region]
  ; Returns a listof regions where the defender beat back the attacker
  (define/public (failed-attack! a d)
    (map (λ (r) (if (eq? a r)
                    (begin (r . set-dice! 1)
                           r)
                    r))
         (field regions)))
  
  ; Number Number -> Bundle
  ; '(attack Number aRoll Number dRoll SerialBoard)
  (define/public (attack! a d)
    (local [(define (attacker) (list-ref (field regions) a))
            (define (defender) (list-ref (field regions) d))
            (define aRoll ((attacker) . roll))
            (define dRoll ((defender) . roll))
            (define success (> (foldr + 0 aRoll) 
                               (foldr + 0 dRoll)))]
      (make-bundle
       (world%
        (field players)
        (if success
            (successful-attack! a d)
            (failed-attack! a d))
        (field observers))
       (map (λ (p) (make-mail 
                    (p . iworld)
                    (list 'attack a aRoll d dRoll 
                          (if success
                              (serialregions (successful-attack! a d))
                              (serialregions (failed-attack! a d))))))
            (field players))
       empty)))
  
  ; -> Bundle
  ; if the attack is illegal, let the attacking player know
  (define/public (illegal-attack! a)
    (local [(define (attacker) (list-ref (field regions) a))]
      (make-bundle this
                   (list 
                    (make-mail 
                     ((first (filter 
                              (λ (p) 
                                (iworld=? (p . iworld) 
                                          (((attacker) . player) . iworld)))
                              (field players))) . iworld)
                     'illegal))
                   empty))))

; region1 - player 0, 1 dice, borders 2
; region2 - player 0, 4 dice, borders 1 + 3
; region3 - player 1, 2 dice, borders 2
(define world1 (world% empty (list region1 region2 region3) empty))
(define blank-world (world% empty empty empty))
(define world2 (world% (list player1
                             player2)
                       (world1 . regions)
                       empty))
(check-expect (length (world1 . players)) 0)
(check-expect (world1 . on-new iworld1)
              (make-bundle
               (world% (list player1)
                       (world1 . regions)
                       empty)
               empty
               empty))
(if (= MAXPLAYERS 2)
    (begin (check-expect 
            ((world% (list player1)
                     (world1 . regions)
                     empty) . on-new iworld2)
            (make-bundle
             (world% (list player2 
                           player1)
                     (world1 . regions)
                     empty)
             (list (make-mail iworld2
                              (list 'start 1
                                    (list (world1 . serialplayers (list player1))
                                          (world1 . serialregions (world1 . regions)))
                                    sample-board))
                   (make-mail iworld1
                              (list 'start 0 
                                    (list (world1 . serialplayers (list player1))
                                          (world1 . serialregions (world1 . regions)))
                                    sample-board)))
             empty))
           (check-expect ((world% (list player2 
                                        player1)
                                  (world1 . regions)
                                  empty) . on-new iworld3)
                         (make-bundle
                          (world% (list player2 
                                        player1)
                                  (world1 . regions)
                                  (list (player% 2 "" iworld3)))
                          empty
                          empty))
           (check-expect ((world% (list player2 
                                        player1)
                                  (world1 . regions)
                                  empty) . on-disconnect iworld1)
                         (make-bundle
                          (world% empty
                                  (world1 . regions)
                                  empty)
                          empty
                          (list iworld2 iworld1)))
           true)
    "These 2 tests are engineered with MAXPLAYERS = 2, for convenient testing")

(check-expect ((world% (list player1)
                       (world1 . regions)
                       empty) . on-disconnect iworld1)
              (make-bundle
               (world% empty
                       (world1 . regions)
                       empty)
               empty
               empty))
(check-expect ((world% (list player1
                             player2)
                       (world1 . regions)
                       empty) . next-player)
              (list player2
                    player1))
(check-expect ((world% (list player2 
                             player1)
                       (world1 . regions)
                       empty) . serialplayers (list player1
                                                    player2))
              (list '(0 "") '(1 "")))
(check-expect (world1 . ->number region2) 1)
(check-expect (world1 . serialregions (world1 . regions))
              (list '((1) (0 "") 1)
                    '((0 2) (0 "") 4)
                    '((1) (1 "") 2)))
(check-expect (world2 . adjacent-territories
                      (first (world2 . players)))
              2)

(define region1a (region% empty player1 1))
(define region2a (region% empty player1 1))
(define region3a (region% empty player2 1))
(region1a . set-neighbors! (list region2a))
(region2a . set-neighbors! (list region1a region3a))
(region3a . set-neighbors! (list region2a))

(check-expect ((world% (list (player% 0 "" iworld1)
                             (player% 0 "" iworld2))
                       (list region1a region2a region3a)
                       empty) . distribute-dice player1)
              (list region1a region2a region3a))
(check-expect (and 
               (or (> (region1a . dice) 1)
                   (> (region2a . dice) 1))
               (= (region3a . dice) 1)) true)

;; The following check will never pass, as player-done is inherently random
#;
(check-expect (world2 . on-msg iworld1 'done)
              (world2 . player-done))
;; This check also will not pass, as attacking is inherently random
#;
(check-expect (world2 . on-msg iworld1 
                      '(attack 1 2))
              (world2 . attack! 1 2))
(check-expect (world2 . on-msg iworld1
                      '(attack 0 1))
              (world2 . illegal-attack! 0))
(check-expect (world2 . on-msg iworld2
                      '(attack 2 0))
              (world2 . illegal-attack! 2))
(check-expect (world2 . on-msg iworld1
                      '(name "David"))
              (world2 . change-name iworld1 "David"))
(check-expect (world2 . change-name iworld1 "")
              (begin (player1 . change-name! "")
                     (make-bundle
                      (world% (world2 . players)
                              (world2 . regions)
                              (world2 . observers))
                      (list (make-mail 
                             iworld1
                             (list 'new-state
                                   (list (world2 . serialplayers 
                                                 (world2 . players))
                                         (world2 . serialregions 
                                                 (world2 . regions)))))
                            (make-mail 
                             iworld2
                             (list 'new-state
                                   (list (world2 . serialplayers 
                                                 (world2 . players))
                                         (world2 . serialregions 
                                                 (world2 . regions))))))
                      empty)))
(check-expect (world2 . on-msg iworld1
                      '(attack 1))
              (make-bundle world2
                         (list (make-mail iworld1 'error))
                         empty))
(check-expect (world2 . illegal-attack! 2)
              (make-bundle world2
                   (list (make-mail 
                          ((region3 . player) . iworld)
                          'illegal))
                   empty))
(check-expect (world2 . failed-attack! region2 region3)
              (begin (region2 . set-dice! 1)
                     (world2 . regions)))
(check-expect (begin (region2 . set-dice! 10)
                     (region3 . set-dice! 1)
                     (world2 . successful-attack! region2 region3))
              (list region1 
                    (begin (region2 . set-dice! 1)
                           region2)
                    (begin (region3 . set-player! (region2 . player))
                           (region3 . set-dice! 9)
                           region3)))