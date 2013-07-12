;;;; skaro.el --- Silly robots game

;;; rules

(defun skaro-killed-p ()
  (member skaro-player (append skaro-enemies skaro-piles)))

(defun skaro-allowed-p (x y)
  (and (>= x 0) (>= y 0)
       (<= x (car skaro-size)) (<= y (cdr skaro-size))))

(defun skaro-collision-p (obstacles position)
  (> (count position (append skaro-enemies skaro-piles)) 1))

(defun skaro-get-collisions ()
  (remove-if-not 'skaro-collision-p skaro-enemies))

;;; drawing

(defun skaro-place (marker position)
  (goto-char (point-min))
  (beginning-of-line (cdr position))
  (forward-char (car position))
  (delete-char 1)
  (insert marker))

(defun skaro-draw-board ()
  (let (buffer-read-only)
    (delete-region (point-min) (point-max))
    (dotimes (n (cdr skaro-size))
      (insert (make-string (car skaro-size) ?_) "\n"))
    (skaro-place "O" skaro-player)
    (dolist (enemy skaro-enemies)
      (skaro-place "M" enemy))
    (dolist (pile skaro-piles)
      (skaro-place "X" pile))))

;;; game commands

(defun skaro-end (text)
  (message text)
  (fundamental-mode)
  (read-only-mode t))

(defun skaro-collide (collision)
  (delete collision skaro-enemies)
  (add-to-list 'skaro-piles collision))

(defun skaro-move-enemy (enemy)
  (let ((dx (- (car skaro-player) (car enemy)))
        (dy (- (cdr skaro-player) (cdr enemy))))
    (if (> (abs dx) (abs dy))
        (cons (funcall (if (> dx 0) '1+ '1-) (car enemy)) (cdr enemy))
        (cons (car enemy) (funcall (if (> dy 0) '1+ '1-) (cdr enemy))))))

(defun skaro-play (x y)
  (when (skaro-allowed-p (+ x (car skaro-player)) (+ y (cdr skaro-player)))
    (setq skaro-player (cons (+ x (car skaro-player))
                             (+ y (cdr skaro-player)))))
  (setq skaro-enemies (mapcar 'skaro-move-enemy skaro-enemies))
  (dolist (collision (skaro-get-collisions))
    (skaro-collide collision))
  (skaro-draw-board)
  (when (skaro-killed-p)
    (skaro-end "You died."))
  (when (null skaro-enemies)
    (skaro-end "You won. Nice job.")))

(defun skaro-teleport ()
  (intercative)
  (let ((new-position (skaro-random)))
    (skaro-play (- (car new-position) (car skaro-player))
                (- (cdr new-position) (cdr skaro-player)))))

;;; setup

(defvar skaro-size '(10 . 10))
(defvar skaro-enemies ())
(defvar skaro-player ())
(defvar skaro-piles ())

(defun skaro-random (&rest _)
  (cons (random (car skaro-size))
        (random (cdr skaro-size))))

(defvar skaro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") (lambda () (interactive) (skaro-play 0 -1)))
    (define-key map (kbd "<down>") (lambda () (interactive) (skaro-play 0 1)))
    (define-key map (kbd "<left>") (lambda () (interactive) (skaro-play -1 0)))
    (define-key map (kbd "<right>") (lambda () (interactive) (skaro-play 1 0)))
    (define-key map (kbd "SPC") 'skaro-teleport)
    (define-key map (kbd "q") 'kill-this-buffer)
    map))

(define-derived-mode skaro-mode fundamental-mode "skaro" "A silly game."
  (set (make-local-variable 'skaro-enemies) (mapcar 'skaro-random
                                                    (make-list 4 nil)))
  (set (make-local-variable 'skaro-player) (skaro-random))
  (set (make-local-variable 'skaro-piles) ())
  (skaro-draw-board))

(defun skaro ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*skaro*"))
  (skaro-mode))
