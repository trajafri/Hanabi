(module deck racket
  (provide deck-generator deck-shuffler dealer)
  (require racket/list "gfx.rkt" "structs.rkt" 2htdp/image
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

  ;; card=?: Card Card -> Boolean
  (define (card=? c1 c2)
    (and (string=? (card-color c1) (card-color c2))
         (= (card-value c1) (card-value c2))))

  ;; Card count
  (define card-count '(3 2 2 2 1))
  (define card-colors '("dodgerblue" "green" "red" "white" "yellow"))

  ;; deck-generator: -> [ListOf Card]
  ;; Generates a unshuffled deck of card.
  (define (deck-generator)
    (list-do
     (count&num <- (map list card-count (range 1 (add1 (length card-count)))))
     (color <- card-colors)
     (count = (car count&num))
     (num = (cadr count&num))
     (yield (build-list count (λ (x) (Card color num))))))

  (check-equal? (* (length card-colors) (foldl + 0 card-count)) (length (deck-generator)))

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
    (car ((state-tfunc (helper (return '()) (length deck))) deck)))


  (check-true (andmap (λ (x y) (card=? x y))
                      (sort (deck-shuffler (deck-generator))
                            (λ (x y) (cond
                                       [(string=? (card-color x) (card-color y))
                                        (< (card-value x) (card-value y))]
                                       [(= (card-value x) (card-value y))
                                        (string<? (card-color x) (card-color y))]
                                       [else (< (card-value x) (card-value y))])))
                      (deck-generator)))

  ;; dealer: Int:n Int:m ([ListOf Card] -> Card [ListOf Card]):pick [ListOf Card] -> [ListOf [ListOf Card]]
  ;; For each n, dealer uses the pick function to pick m cards
  (define (dealer n m pick deck)
    ;; pick-m: Int StateM -> StateM
    (define (pick-m m st)
      (if (zero? m) st
          (monad-do (bind return (λ (x) x))
                    (curr <- st)
                    (deck <- get)
                    (card&deck = (let-values [((card deck) (pick deck))]
                                   `(,card ,deck)))
                    (_ <- (put (cadr card&deck)))
                    (pick-m (sub1 m) (return (cons (car card&deck) curr))))))
    ;; helper: Int StateM -> StateM
    (define (helper n st)
      (if (zero? n) st
          (monad-do (bind return (λ (x) x))
                    (curr <- st)
                    (deal <- (pick-m m (return '())))
                    (helper (sub1 n) (return (cons deal curr))))))
    (car ((state-tfunc (helper n (return '()))) deck)))

  (check-true (let [(deck (deck-generator))]
                (andmap (λ (x y) (andmap (λ (x y) (card=? x y)) x y))
                        (dealer 4 4 (λ (deck) (values (car deck) (cdr deck))) deck)
                        `((,(Card "dodgerblue" 2) ,(Card "yellow" 1) ,(Card "yellow" 1) ,(Card "yellow" 1))
                          (,(Card "white" 1) ,(Card "white" 1) ,(Card "white" 1) ,(Card "red" 1))
                          (,(Card "red" 1) ,(Card "red" 1) ,(Card "green" 1) ,(Card "green" 1))
                          (,(Card "green" 1) ,(Card "dodgerblue" 1) ,(Card "dodgerblue" 1) ,(Card "dodgerblue" 1))))))
  (check-true (let [(deck (deck-generator))]
                (andmap (λ (x y) (andmap (λ (x y) (card=? x y)) x y))
                        (dealer 4 4 (λ (deck) (values (car (reverse deck)) (reverse (cdr (reverse deck))))) deck)
                        `((,(Card "yellow" 3) ,(Card "dodgerblue" 4) ,(Card "dodgerblue" 4) ,(Card "green" 4))
                          (,(Card "green" 4) ,(Card "red" 4) ,(Card "red" 4) ,(Card "white" 4))
                          (,(Card "white" 4) ,(Card "yellow" 4) ,(Card "yellow" 4) ,(Card "dodgerblue" 5))
                          (,(Card "green" 5) ,(Card "red" 5) ,(Card "white" 5) ,(Card "yellow" 5)))))))