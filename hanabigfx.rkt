(module hanabigfx racket
  (require 2htdp/image 2htdp/universe)
  (provide card coin)
  
  ;; Card dimensions
  (define C-H 160)
  (define C-W (* C-H 0.63))

  ;; Card borders
  (define B-COLOR "white")
  (define B-W 10)

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
  (define N-B 1)
  (define N-GAP 0.3) ;; Gap between borders and number's edge 

  ;; number: Int Color -> Image
  ;; Creates an image of a number with the given color
  (define (number n c)
    (define num (number->string n))
    (overlay (text/font num (- N-SIZE N-B) c #f "default" "italic" "normal" #f)
             (text/font num (- N-SIZE N-B) "white" #f "default" "italic" "bold" #f)))

  ;; card: Int Color -> Image
  ;; Creates an image of a card
  (define (card n c)
    (define num (number n c))
    (define card-base (overlay (rectangle (- C-W B-W) (- C-H B-W) "solid" C-COLOR)
                               (rectangle C-W C-H "solid" B-COLOR)))
    (define l-x (+ (/ (image-width num) 2) B-W N-GAP))
    (define r-x (- C-W (+ (/ (image-width num) 2) B-W N-GAP)))
    (define t-y (+ (/ (image-height num) 3) B-W N-GAP))
    (define b-y (- C-H (+ (/ (image-height num) 3) B-W N-GAP)))
    (place-image num r-x t-y
                 (place-image num l-x t-y
                              (place-image num l-x b-y
                                           (place-image num r-x b-y card-base)))))

  ;; Coin dimensions
  (define CO-R 5)
  (define CO-BORDER 2)

  (define HINT-C "lightblue")
  (define HINT-C-BG "blue")
  (define BOMB-C "red")
  (define BOMB-C-BG "tomato")

  ;; coin: Color Color -> Image
  (define (coin fg bg)
    (overlay (circle CO-R "solid" fg)
             (circle (+ CO-R CO-BORDER) "solid" bg))))