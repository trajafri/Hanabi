#lang racket

(require racket/list "hanabigfx.rkt" 2htdp/image
         (only-in heresy monad-do list-do)
         rackunit)

;; Color is one of:
;; - "dodgerblue"
;; - "green"
;; - "red"
;; - "white"
;; - "yellow"
;; - "lightblue"
;; - "tomato"

;; A Card is a (Card Color Number)
(struct Card [color value])

;; card=?: Card Card -> Boolean
(define (card=? c1 c2)
  (and (string=? (Card-color c1) (Card-color c2))
       (= (Card-value c1) (Card-value c2))))

;; Card count

(define card-count '(3 2 2 2 1))
(define card-color '("dodgerblue" "green" "red" "white" "yellow"))

;; deck-generator: -> [ListOf Card]
;; Generates a unshuffled deck of card.
(define (deck-generator)
  (list-do
   (count&num <- (map list card-count (range 1 (add1 (length card-count)))))
   (color <- card-color)
   (count = (car count&num))
   (num = (cadr count&num))
   (yield (build-list count (λ (x) (Card color num))))))

(check-equal? (* (length card-color) (foldl + 0 card-count)) (length (deck-generator)))


;; StateM is a (State (S -> (V . S)))
(struct State [tfunc])

;; return: V -> StateM
(define (return v)
  (State (λ (s) `(,v . ,s))))

;; bind: StateM (V -> StateM) -> StateM 
(define (bind ma f)
  (match ma
    [(State tfunc) (State
                    (λ (new-s)
                      (match-let*
                          [(`(,nv . ,ns) (tfunc new-s))
                           ((State func) (f nv))]
                        (func ns))))]))

;; get: StateM -> StateM
(define get
  (State (λ (s) `(,s . ,s))))

;; put: S -> StateM
;; Note: Since heresyy's do notation does not implement >>, we need to bind put to some value.
(define (put st)
  (State (λ (x) `(() . ,st))))

;; deck-shuffler: [ListOf Card] -> [ListOf Card]
;; Shuffles the given deck of cards
(define (deck-shuffler deck)
  ;; remove: Int [ListOf Card] -> Card [ListOf Card]
  (define (remove index ls)
    (if (zero? index) (values (car ls) (cdr ls))
        (let-values [((card ret-list) (remove (sub1 index) (cdr ls)))]
          (values card (cons (car ls) ret-list)))))
  ;; helper: StateM ([ListOfCard] [ListOf Card]) Int-> StateM 
  (define (helper statem int)
    (if (zero? int) statem (monad-do (bind return (λ (x) x))
                                     (curr-deck <- statem)
                                     (remaining-deck <- get)
                                     (new-card&new-deck = (let-values [((nc nd) (remove (random int) remaining-deck))]
                                                            `(,nc ,nd)))
                                     (_ <- (put (cadr new-card&new-deck)))
                                     (helper (return (cons (car new-card&new-deck) curr-deck)) (sub1 int)))))
  (car ((State-tfunc (helper (return '()) (length deck))) deck)))


(check-true (andmap (λ (x y) (card=? x y))
                    (sort (deck-shuffler (deck-generator))
                          (λ (x y) (cond
                                     [(string=? (Card-color x) (Card-color y))
                                      (< (Card-value x) (Card-value y))]
                                     [(= (Card-value x) (Card-value y))
                                      (string<? (Card-color x) (Card-color y))]
                                     [else (< (Card-value x) (Card-value y))])))
                    (deck-generator)))
