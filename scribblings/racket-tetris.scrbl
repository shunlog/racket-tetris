#lang scribble/manual
@(require scribble/example
   (for-label racket
          "../block.rkt"
          "../draw.rkt"))

@(define ss-eval (make-base-eval)) 

@(ss-eval '(require "draw.rkt"))
@(ss-eval '(require "block.rkt"))


@title{Racket Tetris}
 
@defmodule[racket-tetris/block]

@examples[
     #:eval ss-eval
     TILE-GARBAGE
   ]

@examples[
     #:eval ss-eval
     (tile-pict TILE-GARBAGE)
      (tile-pict (tile-normal 'L))
            (tile-pict (tile-ghost 'L))
   ]