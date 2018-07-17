(module structs racket
  (provide Card (rename-out
                 (Card-color card-color)
                 (Card? card?)
                 (Card-value card-value)))

  ;; Color is one of:
  ;; - "dodgerblue"
  ;; - "green"
  ;; - "red"
  ;; - "white"
  ;; - "yellow"
  ;; - "lightblue"
  ;; - "tomato"
  
  ;; A Card is a (Card Color Number)
  (struct Card [color value]))