(module structs racket
  (provide Card State return bind get put
           (rename-out
            (State-tfunc state-tfunc))
           (rename-out
                 (Card-color card-color)
                 (Card-value card-value)
                 (Card? card?))
           (rename-out
                 (Player-name player-name)
                 (Player-cards player-cards)
                 (Player-info player-info)
                 (Player? player?)))

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
  ;; Note: Since heresy's do notation does not implement >>, we need to bind put to some value.
  (define (put st)
    (State (λ (x) `(() . ,st))))

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

  ;; A Player is a (Player String [ListOf Card] [ListOf String])
  (struct Player [name cards info]))