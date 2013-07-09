;;;; skaro.el --- Silly robots game

(defvar skaro-size '(10 . 10))
(defvar skaro-enemies ())
(defvar skaro-player ())
(defvar skaro-piles ())

(defun skaro-random (&rest _)
  (cons (random (car skaro-size))
        (random (cdr skaro-size))))

(defun skaro-killed-p ()
  (some (lambda (x) (equal x skaro-player)) (concat skaro-enemies skaro-piles)))

(defun skaro-allowed-p (x y)
  (and (> x 0) (> y 0)
       (>= x (car skaro-size)) (>= y (cdr skaro-size))))

(defun skaro-place (marker position)
  (goto-char (point-min))
  (beginning-of-line (cdr position))
  (forward-char (car position))
  (insert marker))

(defun skaro-draw-board ()
  (let (buffer-read-only)
    (delete-region (point-min) (point-max))
    (skaro-place "O" skaro-player)
    (mapc (apply-partially 'skaro-place "M") skaro-enemies)
    (mapc (apply-partially 'skaro-place "X") skaro-piles)))

(defun skaro-collision-p (obstacles position)
  (> (length (remove-if-not (apply-partially 'equal position) obstacles)) 1))

(defun skaro-get-collisions ()
  (remove-if-not (apply-partially 'skaro-collision-p
                                  (concat skaro-enemies skaro-piles)
                                  skaro-enemies)))

(defun skaro-collide (collision)
  (setq skaro-enemies (remove collision skaro-enemies))
  (add-to-list 'skaro-piles collision))

(defun move-enemy (enemy)
  (let ((dx (- (car skaro-player) (car enemy)))
        (dy (- (cdr skaro-player) (cdr enemy))))
    (if (> (abs dx) (abs dy))
        (cons ((if (> dx 0) 1+ 1-) (car enemy)) (cdr enemy))
        (cons (car enemy) ((if (> dy 0) 1+ 1-) (cdr enemy))))))

(defun skaro-move (x y)
  (when (skaro-allowed-p (+ x (car skaro-player)) (+ y (cdr skaro-player)))
    (setq skaro-player (cons (+ x (car skaro-player))
                             (+ y (cdr skaro-player)))))
  (setq skaro-enemies (mapcar 'skaro-move-enemy skaro-enemies))
  (mapc 'skaro-collide (skaro-get-collisions))
  (skaro-draw-board)
  (when (skaro-killed-p)
    (skaro-end "You died."))
  (when (null skaro-enemies)
    (skaro-end "You won. Nice job.")))

(defvar skaro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") (lambda () (interactive) (skaro-move 0 -1)))
    (define-key map (kbd "<down>") (lambda () (interactive) (skaro-move 0 1)))
    (define-key map (kbd "<left>") (lambda () (interactive) (skaro-move -1 0)))
    (define-key map (kbd "<right>") (lambda () (interactive) (skaro-move 1 0)))
    map))

(define-derived-mode skaro-mode fundamental-mode "skaro" "A game"
  (set (make-local-variable 'skaro-enemies) (mapcar 'skaro-random (make-list 4 nil)))
  (set (make-local-variable 'skaro-player) (skaro-random))
  (set (make-local-variable 'skaro-piles) ())
  (set (make-local-variable 'buffer-read-only) t)
  (overwrite-mode)
  (skaro-draw-board))
