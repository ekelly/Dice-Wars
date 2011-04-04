#lang class5

(require class5/universe)

#|
 
A Region is (region% [Listof Number] Player Number)
Interp: the first list is the neighbor regions, as indexes into the list of
regions in the World.  The Number is the quantity of dice.

|#

(define MAXDICE 10)

(define region%
  (class (fields neighbors player dice)
    
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
    (define/public (add-dice n)
      (set-field! dice (+ (field dice) n)))
    
    ; [listof Region] ->
    ; Sets the neighbors to the given list of regions
    (define/public (set-neighbors lor)
      (set-field! neighbors lor))))

(define region1 (region% empty 1 1))
(define region2 (region% empty 1 1))
(define region3 (region% empty 2 2))
(region1 . set-neighbors (list region2))
(region2 . set-neighbors (list region1 region3))
(region3 . set-neighbors (list region2))

(check-expect (region3 . dice-left) 8)
(check-expect (region1 . dice-left) 9)
(check-expect (< 0 (first (region1 . roll)) 7) true)
#|

A Player is (player% Number Name IWorld)
The Number is the player number that the server uses to identify the player.
The Name is an arbitrary String.
The IWorld is the world that this player controls.

|#

(define player%
  (class (fields id name iworld)
    
    #|
    A SerialPlayer is (List Number String)
IWorlds are not useful off of the server.
|#    
    
    ; Serializes the player
    ; -> SerialPlayer
    (define/public (serialize)
      (list (field id) (field name)))))

(define player1 (player% 0 "David" iworld1))
(check-expect (player1 . serialize) (list 0 "David"))

(define WIDTH 7)
(define HEIGHT 5)
(define MAXPLAYERS 4)
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

(define world%
  (class (fields players regions observers)
    
    ; iw -> world%
    (define/public (on-new iw)
      (if (< (length (field players)) (sub1 MAXPLAYERS))
          (world% (cons iw (field players))
                  (field regions)
                  (field observers))
          (world% (field players)
                  (field regions)
                  (cons iw (field observers)))))
    
    #|

PLAYER MESSAGES:

'done indicates that the player is done with their turn.

'(attack Number Number) indicates that the player wishes to have the Region
 indicated by the first number attack the Region indicated by the second Number.

'(name String) indicates that the player should be known by the indicated name.

SERVER MESSAGES:

'turn indicates that the player that receives the message should take their turn.

'illegal indicates the the player sent an attack message that involved incorrect regions.

'error indicates the the player sent a message out of turn, or the message was ill-formed.

'(attack Number Roll Number Roll SerialBoard) represents an attack that happened 
on the board (not necessarily by the player that receives the message). 
The first number is the attacking region, the second number is the defending region. 
The Rolls are the dice rolls of the two regions. The SerialBoard is the new board state. 
A Roll is a [Listof Number], with each number between 1 and 6.

'(new-state SerialBoard) indicates a state update for any other reason, 
such as new dice allocation.

'(start Number SerialBoard BoardRep) tells the recieving player that the 
game is starting, that they are the player indicated by the given number, 
that the board is as described by SerialBoard, and that the board layout may be drawn as BoardRep.

  |#
    
    #|

A SerialBoard is
(List [Listof SerialPlayer]
      [Listof SerialRegion])
There is no need to send the list of observers.
 
|#
    
    ; -> [Listof Player]
    ; Returns a list with the players rotated one
    (define/public (next-player)
      (append (first (field players))
              (rest (field players))))
    
    ; -> [Listof SerialPlayer]
    ; Returns a list of serialized players
    (define/public (serialplayers)
      (map (λ (sp) (sp . serialize)) 
           (next-player)))
    
    ; Region -> Number
    ; takes a region and returns its position in the list
    (define/public (->number r)
      (local [(define (->number-helper n lor)
                (if (eq? r (first lor))
                    n
                    (->number-helper (add1 n) (rest lor))))]
        (->number-helper 0 (field regions))))
    
    ; -> [Listof SerialRegion]
    ; Serializes the list of regions
    (define/public (serialregions lor)
      (map (λ (sr) (list (map (λ (n) (->number n)) 
                              (sr . neighbors))
                         (sr . player)
                         (sr . id)))
           lor))
    
    ; Player -> Number
    ; Counts the max number of adjacent territories of the given player
    (define/public (adjacent-territories p)
      (local [(define max-regions 0)
              (define player-regions 
                (filter (λ (r) (eq? (r . player) p)) (field regions)))
              (define (adj-helper n lor)
                ())]
        (max () max-regions))
                
    
    ; Number -> [Listof Region]
    ; (> 0 (random 2))
    ; Adds a specific number of dice to random regions
    (define/public (distribute-dice p)
      (local [(define (distribute-helper n acc)
                (if (= n 0)
                    acc
                    (local [(define sub-dice 0)
                            (define random-dice-num 0)]
                      (map (λ (r) (if (> (r . dice-left) 1)
                                      (begin (set! random-dice-num 
                                                   (random (r . dice-left)))
                                             (set! sub-dice 
                                                   (+ sub-dice random-dice-num))
                                             (r . add-dice random-dice-num))
                                      r)
                             acc)))))]
        (distribute-helper (adjacent-territories p) (field regions))))
    
    ; -> Bundle
    ; Creates a bundle of the next player
    (define/public (player-done)
      (local [(define dice-added-regions 
                (distribute-dice (first (field players))))]
        (make-bundle 
         (world% (next-player)
                 (dice-added-regions)
                 (field observers))
         (cons (make-mail ((first (next-player)) . iworld)
                          'turn)
               (map (λ (p) 
                      (make-mail 
                       (p . iworld)
                       (list 'new-state
                             (list 
                              (serialplayers)
                              (serialregions (dice-added-regions))))))
                    (rest (next-player))))
         empty))
      
      ; IWorld S-Expr -> Bundle
      ; NOTE: THIS IS WHERE WE NEED TO ADD ANTI-CHEATING CODE
      (define/public (on-msg iw m)
        (cond 
          ; player is done attacking
          [(symbol=? m 'done) (player-done)]
          ; Player is attacking
          [(symbol=? (first m) 'attack)
           (local [(define legalmove? 
                     (ormap (λ (r)
                              (ormap (λ (ar) (= (second m) ar))
                                     (first r)))
                            (serialregions (field regions))))]
             (if legalmove?
                 ; attack
                 (attack (second m) (third m))
                 ; return illegal move
                 (illegal-attack (second m))))]
          ; player wants to change his name
          [(symbol=? (first m) 'name) (change-name iw (second m))]
          [else (make-bundle this
                  (list iw 'error)
                  empty)]))
      
      ; iWorld String -> Bundle
      ; Change the name of the given iworld to the given String
      (define/public (change-name iw name)
        (make-bundle
         (world% (map (λ (p) 
                        (if (iworld=? iw (p . iworld))
                            (player% (p . id) 
                                     name
                                     (p . iworld))
                            p))
                      (field players))
                 (field regions)
                 (field observers))
         (list (serialplayers)
               (serialregions))
         empty))
      
      ; Number Number -> [Listof Region]
      ; Returns a listof regions where the attacker overran the defender
      (define/public (successful-attack a d)
        (map (λ (r) (if (eq? d r)
                        (region% (d . neighbors)
                                 (a . player)
                                 (- (a . dice) 1))
                        r))
             (field regions)))
      
      ; Region Region -> [Listof Region]
      ; Returns a listof regions where the defender beat back the attacker
      (define/public (failed-attack a d)
        (map (λ (r) (if (eq? a r)
                        (region% (a . neighbors)
                                 (a . player)
                                 1)
                        r))
             (field regions)))
      
      ; Number Number -> Bundle
      ; '(attack Number aRoll Number dRoll SerialBoard)
      (define/public (attack a d)
        (local [(define attacker (list-ref (field regions) a))
                (define defender (list-ref (field regions) d))
                (define aRoll ((attacker) . roll))
                (define dRoll ((defender) . roll))
                (define success (> aRoll dRoll))]
          (make-bundle
           (world%
            (field players)
            (if (success)
                (successful-attack a d)
                (failed-attack a d))
            (field observers))
           (list (map (λ (p) (list 'attack a aRoll d dRoll 
                                   (if success
                                       (serialregions (successful-attack a d))
                                       (serialregions (failed-attack a d)))))
                      (field players)))
           empty)))
      
      ; -> Bundle
      ; if the attack is illegal, let the attacking player know
      (define/public (illegal-attack a)
        (local [(define attacker (list-ref (field regions) a))]
          (make-bundle this
                       (list ((first (filter (λ (p) (iworld=? (p . iworld) 
                                                              ((attacker) . iworld)))
                                             (field players))) . iworld)
                             'illegal)
                       empty)))))