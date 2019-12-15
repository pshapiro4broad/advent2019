
(setq min-x 1000)
(setq max-x -1)
(setq min-y 1000)
(setq max-y -1)

(maphash (lambda (key value)
           (setq min-x (min min-x (car key)))
           (setq max-x (max max-x (car key)))
           (setq min-y (min min-y (cdr key)))
           (setq max-y (max max-y (cdr key)))
           )
         (gethash 'tiles bot))
nil
min-x
;; => 0
max-x
;; => 37
min-y
;; => 0
max-y
19
;; => 19

(require 'gamegrid)
(require 'tetris)

(setq program (make-program problem1))
(setq amp (make-amp program))
(puthash 0 2 program)

(defun day13-start ()
  (interactive)
  (switch-to-buffer "*TILES*")
  (gamegrid-init (tetris-display-options))
  (gamegrid-init-buffer 40 21 9)
  (let* ((program (make-program problem1))
         (amp (make-amp program)))
    (run-amplifier amp)))

(day13-start)

(defun bot-get (bot)
  1)

(setq score-x 0)
(setq score-y 20)

(defun show-tiles (tiles)
  (maphash (lambda (key value)
             (gamegrid-set-cell
              (car key) (cdr key) value))
           (gethash 'tiles bot)))

(defun show-score (score)
  (let ((score-string (format "Score:  %05d" score)))
    (dotimes (x (length score-string))
      (gamegrid-set-cell (+ x score-x) score-y (aref score-string x)))))

(show-score 100)
