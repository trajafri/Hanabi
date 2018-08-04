#lang racket
(require (except-in mrlib/image-core make-pen make-color) racket/gui "deck.rkt")

;; A Player is a (Player String [ListOf Card] [ListOf String])
(struct Player [name cards info]) ;info are the hints this player is aware of.

(define CARD-RATIO 20) ;; This number calculate how big a card should be depending on the size of the window.
(define COINT-RATIO "TODO") ;; Should be calculated wrt CARD-RATIO

