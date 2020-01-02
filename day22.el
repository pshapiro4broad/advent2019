;; -*- lexical-binding: t -*-

(setq problem1
      '("deal with increment 5"
"cut 9569"
"deal with increment 22"
"cut -9977"
"deal with increment 64"
"cut -4758"
"deal with increment 12"
"cut 8507"
"deal with increment 7"
"cut -4724"
"deal with increment 3"
"cut 7577"
"deal with increment 20"
"cut -1543"
"deal into new stack"
"deal with increment 62"
"deal into new stack"
"deal with increment 62"
"cut 4879"
"deal into new stack"
"deal with increment 34"
"cut 3555"
"deal with increment 8"
"cut -6954"
"deal with increment 32"
"cut -4299"
"deal into new stack"
"deal with increment 70"
"cut -5387"
"deal with increment 32"
"deal into new stack"
"cut -5074"
"deal into new stack"
"deal with increment 14"
"cut -1405"
"deal with increment 40"
"cut 4665"
"deal with increment 42"
"deal into new stack"
"deal with increment 20"
"cut 5945"
"deal with increment 73"
"cut 9777"
"deal with increment 32"
"cut 4783"
"deal into new stack"
"deal with increment 63"
"cut -3388"
"deal with increment 18"
"cut 6364"
"deal with increment 34"
"cut -7962"
"deal into new stack"
"cut -5937"
"deal with increment 70"
"cut -3600"
"deal with increment 46"
"deal into new stack"
"cut -3460"
"deal with increment 61"
"cut 8430"
"deal with increment 33"
"cut -9068"
"deal into new stack"
"deal with increment 75"
"cut 3019"
"deal with increment 5"
"cut -2963"
"deal with increment 59"
"cut -2973"
"deal with increment 64"
"cut 3203"
"deal with increment 13"
"cut -9915"
"deal with increment 60"
"cut 5823"
"deal with increment 26"
"cut 2255"
"deal with increment 35"
"cut -8491"
"deal with increment 75"
"cut -8065"
"deal with increment 38"
"cut 8417"
"deal with increment 75"
"cut 7005"
"deal into new stack"
"deal with increment 67"
"deal into new stack"
"cut -896"
"deal into new stack"
"cut -7243"
"deal into new stack"
"deal with increment 29"
"cut -4407"
"deal with increment 63"
"cut -8660"
"deal into new stack"
"cut 7411"
"deal into new stack"))

;; shuffle

(setq deal-into-name "deal into new stack")

(defun deal-into-new-stack (stack)
  (reverse stack))

(setq cut-cards-name "cut")

(defun cut-cards (stack command)
  (let* ((raw-n (string-to-number
                 (substring command (length cut-cards-name))))
         (n (if (< raw-n 0)
                (+ (length stack) raw-n)
              raw-n)))
    (vconcat
     (seq-drop stack n)
     (seq-take stack n))))

(defun deal-with-increment (stack command)
  (let ((increment (string-to-number
                    (substring command (length deal-with-increment-name))))
        (new-stack (make-vector (length stack) nil))
        (new-pos 0)
        (card 0))
    (while (< card (length stack))
      (if (elt new-stack new-pos)
          (setq new-pos (mod (+ new-pos increment) (length stack)))
        (progn
          (aset new-stack new-pos (elt stack card))
          (setq card (1+ card)))))
    new-stack))

(defun shuffle-one (stack command)
  (cond ((equal command deal-into-name)
         (deal-into-new-stack stack))
        ((equal (substring command 0 (length cut-cards-name))
                cut-cards-name)
         (cut-cards stack command))
        ((equal (substring command 0 (length deal-with-increment-name))
                deal-with-increment-name)
         (deal-with-increment stack command))))

(shuffle-one cards "deal with increment 3")

(defun make-deck (length)
  (apply 'vector (number-sequence 0 (1- length))))

(setq cards (make-deck 10))

(defun shuffle (stack commands)
  (dolist (command commands stack)
    (setq stack (shuffle-one stack command))))

(shuffle cards '("deal with increment 7"
"deal into new stack"
"deal into new stack"))
;; => [0 3 6 9 2 5 8 1 4 7]

(shuffle cards '("cut 6"
"deal with increment 7"
"deal into new stack"))
;; => [3 0 7 4 1 8 5 2 9 6]

(shuffle cards '("deal with increment 7"
"deal with increment 9"
"cut -2"))
;; => [6 3 0 7 4 1 8 5 2 9]

(shuffle cards '("deal into new stack"
"cut -2"
"deal with increment 7"
"cut 8"
"cut -4"
"deal with increment 7"
"cut 3"
"deal with increment 9"
"deal with increment 3"
"cut -1"))
;; => [9 2 5 8 1 4 7 0 3 6]

(setq space-cards (make-deck 10007))
;; => [0 1 2 3 4 5 6 7 8 9 10 11 ...]
(setq space-shuffle (shuffle space-cards problem1))
;; => [1412 4406 7400 387 3381 6375 9369 2356 5350 8344 1331 4325 ...]
(seq-position space-shuffle 2019)
;; => 8326
