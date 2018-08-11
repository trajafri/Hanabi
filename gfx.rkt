(module gfx racket/gui
  (require "structs.rkt" racket/draw
           (only-in mrlib/image-core render-image)
           (except-in 2htdp/image make-pen make-color))
  (provide card fan-combine deck coin)
  
  ;; Card dimensions
  (define C-H 165)
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

  ;; Table color
  (define T-COLOR "brown")

  ;; add-decks: dc% Number Number [ListOf Player]
  ;; Adds all the cards held by the players in the given list
  (define (add-decks dc FRAME-W FRAME-H lop)
    ;; Assuming frame dimension is FRAME-W and FRAME-H
    (define players (length lop))
    (define (draw-cards lop box-w pos)
      (match lop
        [`(,a)
         (define dck (above (deck (player-cards a) beside) (square 25 "solid" "transparent") (text (player-name a) 20 "white")))
         (define width (image-width dck))
         (render-image dck dc (- pos (/ width 2)) 10)]
        [(cons a b)
         (define dck (above (deck (player-cards a) beside) (square 25 "solid" "transparent") (text (player-name a) 20 "white")))
         (define width (image-width dck))
         (render-image dck dc (- pos (/ width 2)) 10)
         (draw-cards b box-w (+ pos box-w))]))
    (if (> (length lop) 2)
        (let* [(deck1 (first lop))
              (deckn (foldl (λ (f acc) f) '() lop))
              (mid-deck (cdr (reverse (cdr lop))))
              (players (length mid-deck))]
          (render-image (above (rotate 90 (deck (player-cards deck1) beside)) (square 25 "solid" "transparent") (text (player-name deck1) 20 "white"))
                        dc 10 (+ 15 C-H))
          (draw-cards mid-deck (/ FRAME-W players) (/ (/ FRAME-W players) 2))
          (render-image (above (rotate 90 (deck (player-cards deckn) beside)) (square 25 "solid" "transparent") (text (player-name deckn) 20 "white"))
                        dc (- FRAME-W 10 C-H) (+ 15 C-H)))
        (draw-cards lop (/ FRAME-W players) (/ (/ FRAME-W players) 2))))

  ;; add-player-deck: dc% Number Number Player
  (define (add-player-deck dc FRAME-W FRAME-H p)
    ;; Assuming frame dimension is FRAME-W and FRAME-H
    (define deck (apply beside (build-list (length (player-cards p)) (λ (_) card-back))))
    (define width (image-width deck))
    (define height (image-height deck))
    (define top (- FRAME-H height 10))
    (render-image deck dc (- (/ FRAME-W 2) (/ width 2)) top))
  
  ;; draw-table: frame% Player [ListOf Player] [ListOf Coin] [ListOf Coin]
  ;; Adds a canvas with the whole table to the given frame%
  (define (draw-table f p other-players hints bombs)
    (new canvas%
         [parent f]
         [paint-callback
          (λ (canvas dc)
            (send canvas set-canvas-background (make-object color% T-COLOR))
            (add-decks dc (send f get-width) (send f get-height) other-players)
            (add-player-deck dc (send f get-width) (send f get-height) p))])))