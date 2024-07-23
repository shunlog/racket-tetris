#lang racket/base

(require threading)

(require "playfield.rkt")
(require "block.rkt")


(~> (empty-playfield 1 0)
    (playfield-add-block (block 0 0 'L))
    (playfield-add-block* (list
                           (block 0 1 'ghost)
                           (block 0 1 'garbage)))
    (playfield-blocks))
