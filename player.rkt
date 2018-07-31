#lang racket
(require "deck.rkt")

;; A Player is a (Player String [ListOf Card] [ListOf String])
(struct Player [name cards info]) ;info are the hints this player is aware of.

(define DECK-SIZE 30)

(define T-COLOR "brown")

;; draw-table: frame% Player [ListOf Player]
;; Adds a canvas with 