;; -*- lexical-binding: t -*-

(setq problem1 '(".#.##"
                 ".#.#."
                 "##.#."
                 "####."
                 "#.###"))

(setq example '("....#"
                "#..#."
                "#..##"
                "..#.."
                "#...."))

(defun new-grid () (make-hash-table :test 'equal))

    (cl-loop
     for row being the elements of example
     using (index i) do
     (print (list i row)))


(defun make-grid (data)
  (let ((grid (new-grid)))
    (cl-loop
     for row being the elements of data
     using (index i) do
     (cl-loop
      for cell being the elements of row
      using (index j) do
      (puthash (cons i j) cell grid)))
    grid))

(defun print-grid (grid)
  (cl-loop
   for i from 0 below height do
   (progn
     (cl-loop
      for j from 0 below width do
      (princ (string (gethash (cons i j) grid))))
     (princ "\n"))))

(setq grid (new-grid))

(defun is-alive (pos grid)
  (eq (gethash pos grid) ?#))

(defun pos-above (pos)
  (cons (1- (car pos)) (cdr pos)))

(defun pos-below (pos)
  (cons (1+ (car pos)) (cdr pos)))

(defun pos-left (pos)
  (cons (car pos) (1- (cdr pos))))

(defun pos-right (pos)
  (cons (car pos) (1+ (cdr pos))))

(defun live-neighbors (pos grid)
  (+ (if (is-alive (pos-above pos) grid) 1 0)
     (if (is-alive (pos-below pos) grid) 1 0)
     (if (is-alive (pos-right pos) grid) 1 0)
     (if (is-alive (pos-left pos) grid) 1 0)))

(defun next-live (pos grid)
  (if (= (live-neighbors pos grid) 1)
      ?#
    ?.))

(defun next-dead (pos grid)
  (let ((neighbors (live-neighbors pos grid)))
    (if (or (= neighbors 1) (= neighbors 2))
        ?#
      ?.)))

(defun next-state (pos grid)
  (if (is-alive pos grid)
      (next-live pos grid)
    (next-dead pos grid)))

(defun generation (grid)
  (let ((new-grid (new-grid)))
    (cl-loop
     for i from 0 below height do
     (cl-loop
      for j from 0 below width do
      (let ((pos (cons i j)))
        (puthash pos (next-state pos grid) new-grid))))
    new-grid))

(defun biodiversity-rating (grid)
  (let ((total 0))
    (cl-loop
     for i from 0 below height do
     (cl-loop
      for j from 0 below width do
      (if (is-alive (cons i j) grid)
          (setq total (+ total (expt 2 (+ j (* i width))))))))
    total))

(defun find-repeat (grid)
  (let ((seen-ratings (make-hash-table))
        (current-rating (biodiversity-rating grid))
        (num 0))
    (while (not (gethash current-rating seen-ratings))
      (puthash current-rating t seen-ratings)
      (setq grid (generation grid))
      (setq current-rating (biodiversity-rating grid))
      (setq num (1+ num)))
    (print-grid grid)
    (list current-rating num)))
find-repeat

(find-repeat (make-grid example))
.....
.....
.....
#....
.#...
;; => (2129920 86)

(find-repeat (make-grid problem1))
###.#
....#
..#.#
....#
#...#
;; => (18371095 25)

