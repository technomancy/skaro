;;;; SKARO: a game based on https://en.wikipedia.org/wiki/Robots_(BSD_game)

(use srfi-1 srfi-26 srfi-69 extras)

;;; rules

(define (move position input)
  (case input
    ('up (cons (car position) (sub1 (cdr position))))
    ('down (cons (car position) (add1 (cdr position))))
    ('left (cons (sub1 (car position)) (cdr position)))
    ('right (cons (add1 (car position)) (cdr position)))
    (else #f)))

(define (allowed? board position)
  (and position
       (not (negative? (car position)))
       (not (negative? (cdr position)))
       (not (>= (car position) (hash-table-ref board 'width)))
       (not (>= (cdr position) (hash-table-ref board 'height)))))

(define (collision? position obstacles)
  (> (length (filter (cut equal? position <>) obstacles)) 1))

(define (get-collisions enemies obstacles)
  (filter (cut collision? <> obstacles) enemies))

(define (killed? board)
  (member (hash-table-ref board 'player)
          (append (hash-table-ref board 'enemies)
                  (hash-table-ref board 'piles))))

;;; drawing

(define (update-row row marker col)
  (append (take row col) (list marker) (drop row (add1 col))))

(define (place marker position rows)
  (let ((before-rows (take rows (cdr position)))
        (row (list-ref rows (cdr position)))
        (after-rows (drop rows (add1 (cdr position)))))
    (append before-rows
            (list (update-row row marker (car position)))
            after-rows)))

(define (draw-board board)
  (let* ((rows (make-list (hash-table-ref board 'height)
                          (make-list (hash-table-ref board 'width) '_)))
         (rows (place 'O (hash-table-ref board 'player) rows))
         (rows (fold (cut place 'M <> <>) rows (hash-table-ref board 'enemies)))
         (rows (fold (cut place 'X <> <>) rows (hash-table-ref board 'piles))))
    (for-each print rows)))

;;; game loop

(define (move-player! board input)
  (if (eq? input 'teleport)
      (hash-table-set! board 'player
                       (cons (random (hash-table-ref board 'width))
                             (random (hash-table-ref board 'height))))
      (let ((new-position (move (hash-table-ref board 'player) input)))
        (when (allowed? board new-position)
          (hash-table-set! board 'player new-position)))))

(define (move-enemy player enemy)
  (let ((dx (- (car player) (car enemy)))
        (dy (- (cdr player) (cdr enemy))))
    (if (> (abs dx) (abs dy))
        (cons ((if (positive? dx) add1 sub1) (car enemy)) (cdr enemy))
        (cons (car enemy) ((if (positive? dy) add1 sub1) (cdr enemy))))))

(define (move-enemies! board)
  (hash-table-update! board 'enemies
                      (cut map (cut move-enemy
                                    (hash-table-ref board 'player) <>) <>)))

(define (collisions! board collisions)
  (when (not (null? collisions))
    (hash-table-update! board 'enemies
                        (cut remove
                             (cut equal? (car collisions) <>) <>))
    (hash-table-update! board 'piles (cut cons (car collisions) <>))
    (collisions! board (cdr collisions))))

(define (play board input)
  (cond ((killed? board)
         (display "You died.\n"))
        ((eq? input 'quit)
         (display "Bye.\n"))
        ((null? (hash-table-ref board 'enemies))
         (display "You won. Nice job.\n"))
        (#t (move-player! board input)
            (move-enemies! board)
            (let* ((enemies (hash-table-ref board 'enemies))
                   (obstacles (append enemies (hash-table-ref board 'piles))))
              (collisions! board (get-collisions enemies obstacles)))
            (draw-board board)
            (play board (read)))))

;;; setup

(define (add-enemy board enemies)
  (cons (cons (random (hash-table-ref board 'width))
              (random (hash-table-ref board 'height))) enemies))

(define (add-enemies! board enemies)
  (when (positive? enemies)
    (hash-table-update! board 'enemies
                        (cut add-enemy board <>)
                        (constantly '()))
    (add-enemies! board (sub1 enemies))))

(define (make-board width height enemies)
  (let ((board (make-hash-table)))
    (hash-table-set! board 'width width)
    (hash-table-set! board 'height height)
    (hash-table-set! board 'piles '())
    (hash-table-set! board 'player (cons (random width)
                                         (random height)))
    (add-enemies! board enemies)
    board))

(define (main args)
  (let ((board (make-board 10 10 4)))
    (draw-board board)
    (play board (read))))
