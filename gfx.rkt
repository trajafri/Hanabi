(module gfx racket/gui
  (require "structs.rkt" racket/draw
           (only-in mrlib/image-core render-image)
           (except-in 2htdp/image make-pen make-color))
  (provide card fan-combine deck coin)
  
  ;; Card dimensions
  (define C-H 180)
  (define C-W (* C-H 0.63))
  (define C-R 10) ;;Radius for roundness

  ;; Card borders
  (define C-B-COLOR "white")
  (define C-B-W 5)

  ;; Card color
  (define C-COLOR "mediumblue")
  (define C-BACK-COLOR "darkgray")
  ;; - "tomato"

  ;; Card number properties
  (define N-SIZE 20)
  (define N-B-W 1.5)
  (define N-B-COLOR "white")
  (define N-GAP 0.3) ;; Gap between borders and number's edge

  ;; show-image: bitmap% -> Image
  ;; Creates an image of the given bitmap-dc% object.
  ;; Use this to see the result of drawing functions.
  (define (show-image bm)
    (make-object image-snip% bm))

  ;; number: Int Color -> bitmap%
  ;; Creates an image of a number with the given color
  (define (number n c)
    (define num (number->string n))
    (define target (make-bitmap N-SIZE (+ N-SIZE (/ N-SIZE 2))))
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc set-font (make-font #:size N-SIZE #:style 'italic #:weight 'bold))
    (send dc set-text-foreground N-B-COLOR)
    (send dc draw-text num 0 0)
    (send dc set-font (make-font #:size (- N-SIZE N-B-W) #:style 'italic #:weight 'bold))
    (send dc set-text-foreground c)
    (send dc draw-text num (/ N-B-W 2) (/ N-B-W 2))
    target)

  ;; card: Int Color -> bitmap%
  ;; Creates an image of a card
  (define (card n c)
    (define num (number n c))
    (define num-width (send num get-width))
    (define num-height (send num get-height))
    (define target (make-bitmap (exact-round C-W) C-H))
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc set-pen C-B-COLOR C-B-W 'solid)
    (send dc set-brush C-COLOR 'solid)
    (send dc draw-rounded-rectangle 0 0 C-W C-H C-R)
    (define l-x (+ C-B-W N-GAP))
    (define r-x (- C-W (+ num-width C-B-W N-GAP)))
    (define t-y (+ C-B-W N-GAP))
    (define b-y (- C-H (+ num-height C-B-W N-GAP)))
    (send dc draw-bitmap num l-x t-y)
    (send dc draw-bitmap num l-x b-y)
    (send dc draw-bitmap num r-x t-y)
    (send dc draw-bitmap num r-x b-y)
    target)

  ; card-back: bitmap%
  (define card-back
    ((λ ()
       (define target (make-bitmap (exact-round C-W) C-H))
       (define dc (new bitmap-dc% [bitmap target]))
       (send dc set-pen C-B-COLOR C-B-W 'solid)
       (send dc set-brush C-BACK-COLOR 'solid)
       (send dc draw-rounded-rectangle 0 0 C-W C-H C-R)
       target)))
  
  (define fanning-angle -9)
  
  ;; fan-combine: Image Image -> Image
  ;; Combines two images by the set fanning angle
  (define (fan-combine img1 img2)
    (overlay/xy img1 20 -5 (rotate fanning-angle img2)))

  ;; deck: [ListOf Card] (Image Image -> Image) -> Image
  ;; Returns an image of a deck
  (define (deck loc comb-f)
    (if (empty? loc) empty-image (comb-f (show-image (card (card-value (car loc))
                                                           (card-color (car loc))))
                                         (deck (cdr loc) comb-f))))

  #;(overlay (deck `(,(Card "red" 1)
                     ,(Card "red" 2)
                     ,(Card "red" 3)
                     ,(Card "red" 4)
                     ,(Card "red" 5)) (λ (x y) (beside x y))) (empty-scene 600 190 "black"))

  #;(overlay (rotate 20 (deck `(,(Card "red" 1)
                                ,(Card "red" 2)
                                ,(Card "red" 3)
                                ,(Card "red" 4)
                                ,(Card "red" 5)) fan-combine)) (empty-scene 510 170 "black"))

  ;; Coin dimensions
  (define CO-R 18)
  (define CO-BORDER 4)

  (define HINT-C "lightblue")
  (define HINT-C-BG "blue")
  (define BOMB-C "red")
  (define BOMB-C-BG "tomato")

  ;; coin: Color Color -> bitmap%
  (define (coin fg bg)
    (define target (make-bitmap (* 2 CO-R) (* 2 CO-R)))
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc set-brush fg 'solid)
    (send dc set-pen bg CO-BORDER 'solid)
    ;; for some reason, the circle does not fit when drawing starts from 0 0
    (send dc draw-ellipse 2 2 (- (* 2 CO-R) CO-BORDER) (- (* 2 CO-R) CO-BORDER))
    target)

  ;; Frame dimensions
  (define FRAME-W 700)  ;; Eventually, this shouldn't be fixed values.
  (define FRAME-H 400)

  ;; Table color
  (define T-COLOR "brown")

  ;; add-decks: dc% [ListOf Player]
  ;; Adds all the cards held by the players in the given list
  (define (add-decks dc lop)
    ;; Assuming frame dimension is FRAME-W and FRAME-H
    (define players (length lop))
    (define box-w (/ FRAME-W players))
    (define (draw-cards lop pos)
      (match lop
        [`(,a) (render-image (rotate 20 (deck (player-cards a) fan-combine)) dc pos 0)]
        [(cons a b) (render-image (rotate 20 (deck (player-cards a) fan-combine)) dc pos 0)
                    (draw-cards b (+ pos box-w))]))
    (draw-cards lop 0))

  ;; add-player-deck: dc% Player
  (define (add-player-deck dc p)
    ;; Assuming frame dimension is FRAME-W and FRAME-H
    (define top (/ FRAME-H 2))
    (render-image (apply beside (build-list (length (player-cards p)) (λ (_) card-back))) dc (/ FRAME-W 2) (/ FRAME-H 2)))
  
  ;; draw-table: frame% Player [ListOf Player] [ListOf Coin] [ListOf Coin]
  ;; Adds a canvas with the whole table to the given frame%
  (define (draw-table f p other-players hints bombs)
    (new canvas%
         [parent f]
         [paint-callback
          (λ (canvas dc)
            (send canvas set-canvas-background (make-object color% T-COLOR))
            (add-decks dc other-players)
            (add-player-deck dc p))])))