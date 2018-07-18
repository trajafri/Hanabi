(module gfx racket
  (require "structs.rkt" racket/draw racket/gui (except-in 2htdp/image make-pen make-color beside #;empty-image))
  (provide #;card #;#;#;fan-combine deck coin)
  
  ;; Card dimensions
  (define C-H 180)
  (define C-W (* C-H 0.63))
  (define C-R 10) ;;Radius for roundness

  ;; Card borders
  (define C-B-COLOR "white")
  (define C-B-W 5)

  ;; Card color
  (define C-COLOR "mediumblue")

  ;; Color is one of:
  ;; - "yellow"
  ;; - "red"
  ;; - "green"
  ;; - "dodgerblue"
  ;; - "white"
  ;; - "lightblue"
  ;; - "tomato"

  ;; Card number properties
  (define N-SIZE 20)
  (define N-B-W 1.5)
  (define N-B-COLOR "white")
  (define N-GAP 0.3) ;; Gap between borders and number's edge

  ;; show-image: bitmap -> Image
  ;; Creates an image of the given bitmap-dc% object
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
  
  (define fanning-angle -10)
  
  ;; empty-image: bitmap%
  (define empty-image (make-bitmap 1 1))

  ;; beside: Int bitmap% bitmap% -> bitmap%
  ;; Combines the given bitmap objects with the given gap in between
  (define (beside gap bmp1 bmp2)
    (define bmp2-w (send bmp2 get-width))
    (define bmp2-h (send bmp2 get-height))
    (define bmp1-w (send bmp1 get-width))
    (define bmp1-h (send bmp1 get-height))
    (define target (make-bitmap (+ bmp2-w gap bmp1-w) (max bmp2-h bmp1-h)))
    (define dc (new bitmap-dc% [bitmap target])) 
    (send dc draw-bitmap bmp1 0 0)
    (send dc draw-bitmap bmp2 (+ gap bmp1-w) 0)
    target)

  ;; center: bitmap% bitmap% -> bitmap%
  ;; Places the first bitmap at the center of the second bitmap
  (define (center bmp1 bmp2)
    (define bmp2-w (send bmp2 get-width))
    (define bmp2-h (send bmp2 get-height))
    (define bmp1-w (send bmp1 get-width))
    (define bmp1-h (send bmp1 get-height))
    (define target (make-bitmap (max bmp1-w bmp2-w) (max bmp1-h bmp2-h)))
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc draw-bitmap bmp2 0 0)
    (send dc draw-bitmap bmp1 (- (/ bmp2-w 2) (/ bmp1-w 2)) (- (/ bmp2-h 2) (/ bmp1-h 2)))
    target)

  ;; fan-combine: bitmap% bitmap% -> bitmap%
  ;; Combines two images by the set fanning angle
  (define (fan-combine img1 img2)
    (define img2-w (send img2 get-width))
    (define img2-h (send img2 get-height))
    (send img2 rotate fanning-angle)
    (send img2 draw-bitmap img1 20 -5))

  ;; deck: [ListOf Card] (bitmap-% Image -> Image) -> Image
  ;; Returns an image of a deck
  (define (deck loc comb-f)
    (if (empty? loc) empty-image (comb-f (card (card-value (car loc))
                                               (card-color (car loc)))
                                         (deck (cdr loc) comb-f))))

  #;(overlay (rotate 20 (deck `(,(Card "red" 1)
                                ,(Card "red" 2)
                                ,(Card "red" 3)
                                ,(Card "red" 4)
                                ,(Card "red" 5)) fan-combine)) (empty-scene 510 170 "black"))

  ;; Coin dimensions
  (define CO-R 10)
  (define CO-BORDER 4)

  (define HINT-C "lightblue")
  (define HINT-C-BG "blue")
  (define BOMB-C "red")
  (define BOMB-C-BG "tomato")

  ;; coin: Color Color -> Image
  (define (coin fg bg)
    (define target (make-bitmap (* 2 CO-R) (* 2 CO-R)))
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc set-brush fg 'solid)
    (send dc set-pen bg CO-BORDER 'solid)
    ;; for some reason, the circle does not fit when drawing starts from 0 0
    (send dc draw-ellipse 2 2 (- (* 2 CO-R) CO-BORDER) (- (* 2 CO-R) CO-BORDER))
    target))