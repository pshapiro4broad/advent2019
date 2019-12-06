;;; -*- lexical-binding: t -*-

(defun make-pass (number)
  (apply 'vector (mapcar 'string-to-number (split-string (number-to-string number) "" t))))

(defun valid-pass (number)
  (defun at-least-one-duplicate (pass)
    (let ((end (- (length pass) 2)))
      (loop
       for i from 0 to end
       if (and (= (elt pass i) (elt pass (1+ i)))
               ;; not after
               (or (= i end)
                   (not (= (elt pass i) (elt pass (+ i 2)))))
               ;; not before
               (or (= i 0)
                   (not (= (elt pass i) (elt pass (1- i))))))
       return t
       finally return nil)))
  (defun in-sequential-order (pass)
    (loop
     for i from 0 to (- (length pass) 2)
     if (> (elt pass i) (elt pass (1+ i))) return nil
     finally return t))
  (let ((pass (make-pass number)))
    (and (in-sequential-order pass)
         (at-least-one-duplicate pass))))

(defun count-pass (low high)
  (loop
   for pass from low to high
   count (valid-pass pass)))

(count-pass 271973 785961)
;; part2 607

;; part1 925
