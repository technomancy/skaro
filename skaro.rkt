#!/usr/bin/env racket
#lang racket

;;;; SKARO: a game based on https://en.wikipedia.org/wiki/Robots_(BSD_game)

;;; rules

(define (move position input)
  (match input
    ['up (cons (car position) (sub1 (cdr position)))]
    ['down (cons (car position) (add1 (cdr position)))]
    ['left (cons (sub1 (car position)) (cdr position))]
    ['right (cons (add1 (car position)) (cdr position))]
    [_ #f]))

(define (allowed? board position)
  (and position
       (<= 0 (car position) (hash-ref board 'width))
       (<= 0 (cdr position) (hash-ref board 'height))))

(define (collision? obstacles position)
  (> (count (curry equal? position) obstacles) 1))

(define (get-collisions enemies obstacles)
  (filter (curry collision? obstacles) enemies))

(define (killed? board)
  (member (hash-ref board 'player)
          (append (hash-ref board 'enemies)
                  (hash-ref board 'piles))))

;;; drawing

(define (place marker position rows)
  (hash-set rows position marker))

(define (draw-board board)
  (let* ([rows (hash (hash-ref board 'player) 'O)]
         [rows (foldl (curry place 'M) rows (hash-ref board 'enemies))]
         [rows (foldl (curry place 'X) rows (hash-ref board 'piles))])
    (for ([y (hash-ref board 'height)])
      (for ([x (hash-ref board 'width)])
        (display (hash-ref rows (cons x y) '_)))
      (newline))))

;;; game loop

(define (move-player board input)
  (if (eq? input 'teleport)
      (hash-set board 'player
                (cons (random (hash-ref board 'width))
                      (random (hash-ref board 'height))))
      (let ([new-position (move (hash-ref board 'player) input)])
        (if (allowed? board new-position)
            (hash-set board 'player new-position)
            board))))

(define (move-enemy player enemy)
  (define dx (- (car player) (car enemy)))
  (define dy (- (cdr player) (cdr enemy)))
  (if (> (abs dx) (abs dy))
      (cons ((if (positive? dx) add1 sub1) (car enemy)) (cdr enemy))
      (cons (car enemy) ((if (positive? dy) add1 sub1) (cdr enemy)))))

(define (move-enemies board)
  (hash-update board 'enemies
               (curry map (curry move-enemy (hash-ref board 'player)))))

(define (collisions board)
  (define collisions (get-collisions (hash-ref board 'enemies)
                                     (append (hash-ref board 'enemies)
                                             (hash-ref board 'piles))))
  (define new-board (hash-update board 'piles (curry append collisions)))
  (hash-update new-board 'enemies (curry remove* collisions)))

(define round (compose collisions move-enemies move-player))

(define (play board)
  (draw-board board)
  (cond [(killed? board)
         (display "You died.\n")]
        [(null? (hash-ref board 'enemies))
         (display "You won. Nice job.\n")]
        [else (define input (read))
              (if (eq? input 'quit)
                  (display "Bye.\n")
                  (play (round board input)))]))

;;; setup

(define (make-board width height enemies)
  (define (rand-pos) (cons (random width) (random height)))
  (hash 'width width
        'height height
        'piles '()
        'enemies (for/list ([i enemies]) (rand-pos))
        'player (rand-pos)))

(module+ main
  (play (make-board 10 10 4)))
