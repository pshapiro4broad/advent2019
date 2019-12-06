;;; -*- lexical-binding: t -*-

;; create an orbit map
;; count number of orbits / tree depth from COM

(setq raw-map "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L K)YOU I)SAN")

;; map format is cons of
;; - hashmap of object -> list of things orbiting it
;; - hashmap of object -> thing it's orbiting

(defun make-map (raw-map)
  (let ((map-list (mapcar #'(lambda (elt) (split-string elt ")"))  (split-string raw-map " ")))
        (forward-links (make-hash-table :test 'equal))
        (back-links (make-hash-table :test 'equal)))
    (dolist (element map-list)
      (let* ((center (car element))
             (orbit (cadr element))
             (val (gethash center forward-links)))
        (puthash orbit center back-links)
        (if val
            (puthash center (cons orbit val) forward-links)
          (puthash center (list orbit) forward-links))))
    (cons forward-links back-links)))

(defun count-orbits (map)
  (defun inner-count (object depth)
    (let ((orbits (gethash object (car map)))
          (count depth))
      (dolist (orbit orbits count)
        (setq count (+ count (inner-count orbit (1+ depth)))))))
  (inner-count "COM" 0))

(count-orbits (make-map raw-map))
;; => 42

;;(setq map (make-map raw-map))
;;(get-neighbors "J")

(setq raw-map1
      (with-temp-buffer
        (insert-file-contents "day6-input.txt")
        (replace-regexp-in-string "\n" " " (buffer-string))))
(count-orbits (make-map raw-map1))
;; => 314247

(defun filter-list (list f)
  (let ((filtered '()))
    (dolist (elt list filtered)
      (if (apply f (list elt))
          (setq filtered (cons elt filtered))))))

(defun get-distance (map start end)
  (let ((real-start (gethash start (cdr map)))
        (real-end (gethash end (cdr map)))
        (visited (make-hash-table :test 'equal)))
    (defun get-neighbors (object)
      (filter-list
       (cons (gethash object (cdr map))
             (gethash object (car map)))
       #'(lambda (object) (not (gethash object visited)))))
    (defun inner-distance (object distance)
      (puthash object object visited)
      (if (equal real-end object)
          distance
        (loop
         for neighbor in (get-neighbors object)
         for inner = (inner-distance neighbor (1+ distance))
         if inner return inner)))
    (inner-distance real-start 0)))

(get-distance (make-map raw-map) "YOU" "SAN")
;; => 4

(get-distance (make-map raw-map1) "YOU" "SAN")
;; => 514
