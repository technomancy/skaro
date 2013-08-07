;;;; SKARO: a game based on https://en.wikipedia.org/wiki/Robots_(BSD_game)

(use srfi-1 srfi-26 srfi-69 extras)

;;; rules

(define (move position input)
  (case input
    (up (cons (car position) (sub1 (cdr position))))
    (down (cons (car position) (add1 (cdr position))))
    (left (cons (sub1 (car position)) (cdr position)))
    (right (cons (add1 (car position)) (cdr position)))
    (else #f)))

(define (allowed? board position)
  (and position
       (>= 0 (car position) (board-width board))
       (>= 0 (cdr position) (board-height board))))

(define (collision? position obstacles)
  (> (count (cut equal? position <>) obstacles) 1))

(define (get-collisions enemies obstacles)
  (filter (cut collision? <> obstacles) enemies))

(define (killed? board)
  (member (board-player board)
          (append (board-enemies board)
                  (board-piles board))))

;;; drawing

(define (place marker position rows)
  (set-car! (drop (list-ref rows (cdr position)) (car position)) marker))

(define (draw-board board)
  (let ((rows (map (lambda (_) (make-list (board-width board) '_))
                   (iota (board-height board)))))
    (place 'O (board-player board) rows)
    (for-each (cut place 'M <> rows) (board-enemies board))
    (for-each (cut place 'X <> rows) (board-piles board))
    (for-each print rows)))

;;; game loop

(define (move-player! board input)
  (if (eq? input 'teleport)
      (board-player-set! board (cons (random (board-width board))
                                     (random (board-height board))))
      (let ((new-position (move (board-player board) input)))
        (when (allowed? board new-position)
          (board-player-set! board new-position)))))

(define (move-enemy player enemy)
  (let ((dx (- (car player) (car enemy)))
        (dy (- (cdr player) (cdr enemy))))
    (if (> (abs dx) (abs dy))
        (cons ((if (positive? dx) add1 sub1) (car enemy)) (cdr enemy))
        (cons (car enemy) ((if (positive? dy) add1 sub1) (cdr enemy))))))

(define (move-enemies! board)
  (board-enemies-set! board (map (cut move-enemy (board-player board) <>)
                                 (board-enemies board))))

(define (collisions! board collisions)
  (when (not (null? collisions))
    (board-enemies-set! board (remove (cut equal? (car collisions) <>)
                                      (board-enemies board)))
    (board-piles-set! board (cons (car collisions) (board-piles board)))
    (collisions! board (cdr collisions))))

(define (play board input)
  (cond ((killed? board)
         (display "You died.\n"))
        ((eq? input 'quit)
         (display "Bye.\n"))
        ((null? (board-enemies board))
         (display "You won. Nice job.\n"))
        (else (move-player! board input)
              (move-enemies! board)
              (let* ((enemies (board-enemies board))
                     (obstacles (append enemies (board-piles board))))
                (collisions! board (get-collisions enemies obstacles)))
              (draw-board board)
              (play board (read)))))

;;; setup

(define-record board width height player enemies piles)

(define (make-enemies width height enemies enemy-count)
  (if (zero? enemy-count)
      enemies
      (cons (cons (random width) (random height))
            (make-enemies width height enemies (sub1 enemy-count)))))

(define (main args)
  (let* ((width 10)
         (height 10)
         (board (make-board width height (cons (random width)
                                               (random height))
                            (make-enemies width height '()  4) '())))
    (draw-board board)
    (play board (read))))
