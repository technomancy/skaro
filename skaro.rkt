#!/usr/bin/env racket
#lang racket

;;;; SKARO: a game based on https://en.wikipedia.org/wiki/Robots_(BSD_game)

(struct board (width height piles enemies player))

;;; rules

(define (move position input)
  (match input
    ['up (cons (car position) (sub1 (cdr position)))]
    ['down (cons (car position) (add1 (cdr position)))]
    ['left (cons (sub1 (car position)) (cdr position))]
    ['right (cons (add1 (car position)) (cdr position))]
    [_ #f]))

(define (allowed? a-board position)
  (and position
       (<= 0 (car position) (board-width  a-board))
       (<= 0 (cdr position) (board-height a-board))))

(define (collision? obstacles position)
  (> (count (curry equal? position) obstacles) 1))

(define (get-collisions enemies obstacles)
  (filter (curry collision? obstacles) enemies))

(define (killed? a-board)
  (member (board-player a-board)
          (append (board-enemies a-board)
                  (board-piles   a-board))))

;;; drawing

(define (place marker position rows)
  (hash-set rows position marker))

(define (draw-board a-board)
  (let* ([rows (hash (board-player a-board) 'O)]
         [rows (foldl (curry place 'M) rows (board-enemies a-board))]
         [rows (foldl (curry place 'X) rows (board-piles   a-board))])
    (for ([y (board-height a-board)])
      (for ([x (board-width a-board)])
        (display (hash-ref rows (cons x y) '_)))
      (newline))))

;;; game loop

(define (move-player a-board input)
  (if (eq? input 'teleport)
      (struct-copy board a-board
                   [player (cons (random (board-width  a-board))
                                 (random (board-height a-board)))])
      (let ([new-position (move (board-player a-board) input)])
        (if (allowed? a-board new-position)
            (struct-copy board a-board [player new-position])
            a-board))))

(define (move-enemy player enemy)
  (define dx (- (car player) (car enemy)))
  (define dy (- (cdr player) (cdr enemy)))
  (if (> (abs dx) (abs dy))
      (cons ((if (positive? dx) add1 sub1) (car enemy)) (cdr enemy))
      (cons (car enemy) ((if (positive? dy) add1 sub1) (cdr enemy)))))

(define (move-enemies a-board)
  (struct-copy board a-board
               [enemies (map (curry move-enemy (board-player a-board))
                             (board-enemies a-board))]))

(define (collisions a-board)
  (define collisions (get-collisions (board-enemies a-board)
                                     (append (board-enemies a-board)
                                             (board-piles   a-board))))
  (struct-copy board a-board
               [piles   (append  collisions (board-piles   a-board))]
               [enemies (remove* collisions (board-enemies a-board))]))

(define round (compose collisions move-enemies move-player))

(define (play a-board)
  (draw-board a-board)
  (cond [(killed? a-board)
         (display "You died.\n")]
        [(null? (board-enemies a-board))
         (display "You won. Nice job.\n")]
        [else (define input (read))
              (if (eq? input 'quit)
                  (display "Bye.\n")
                  (play (round a-board input)))]))

;;; setup

(define (make-board width height enemies)
  (define (rand-pos) (cons (random width) (random height)))
  (board width height '()
         (for/list ([i enemies]) (rand-pos))
         (rand-pos)))

(module+ main
  (define width    10)
  (define height   10)
  (define enemies#  4)
  (command-line
   #:once-each
   [("-W" "--width") W "board width"
    (set! width (or (string->number W)
                    (error "width is not not a number")))]
   [("-H" "--height") H "board height"
    (set! height (or (string->number H)
                     (error "height is not not a number")))]
   [("-E" "--enemies") N "number of enemies"
    (set! enemies# (or (string->number N)
                       (error "enemies is not not a number")))])
  (play (make-board width height enemies#)))
