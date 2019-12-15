;;; -*- lexical-binding: t -*-

(defun make-moon (x y z)
  (cons (list x y z) (list 0 0 0)))

;; (signum (- p2 p1))
(defun velocity-delta (p1 p2)
  (cond ((= p1 p2) 0)
        ((< p1 p2) 1)
        (t -1)))

(defun compute-velocity (m1 m2)
  (mapcar* 'velocity-delta (car m1) (car m2)))

(defun step-velocity (m1 others)
  (apply 'mapcar*
         (cons '+ (mapcar (apply-partially 'compute-velocity m1) others))))

(defun adjust-velocity (m1 others)
  (cons (car m1)
        (mapcar* '+ (cdr m1) (step-velocity m1 others))))

(defun adjust-all-velocity (moons)
  (mapcar
   (lambda (moon)
     (adjust-velocity
      moon
      (seq-filter (lambda (other) (not (equal moon other))) moons)))
   moons))

(defun adjust-position (moon)
  (cons (mapcar* '+ (car moon) (cdr moon))
        (cdr moon)))

;; <x=-1, y=0, z=2>
;; <x=2, y=-10, z=-7>
;; <x=4, y=-8, z=8>
;; <x=3, y=5, z=-1>
(setq moons
      (list (make-moon -1 0 2)
            (make-moon 2 -10 -7)
            (make-moon 4 -8 8)
            (make-moon 3 5 -1)))

;; compute new velocities for all moons
;; apply velocities to all moons
(defun step-system (moons)
  (mapcar 'adjust-position (adjust-all-velocity moons)))

(defun system-energy (moons)
  (apply '+
         (mapcar
          (lambda (moon)
            (* (apply '+ (mapcar 'abs (car moon)))
               (apply '+ (mapcar 'abs (cdr moon)))))
          moons)))

(setq moon '((1 2 3) 4 5 6))

moons
(((-1 0 2) 0 0 0) ((2 -10 -7) 0 0 0) ((4 -8 8) 0 0 0) ((3 5 -1) 0 0 0))


(0 0 0)


(defun all-equal (s1 s2 i)
  (and (= (elt (car (elt s1 0)) i) (elt (car (elt s2 0)) i))
       (= (elt (cdr (elt s1 0)) i) (elt (cdr (elt s2 0)) i))
       (= (elt (car (elt s1 1)) i) (elt (car (elt s2 1)) i))
       (= (elt (cdr (elt s1 1)) i) (elt (cdr (elt s2 1)) i))
       (= (elt (car (elt s1 2)) i) (elt (car (elt s2 2)) i))
       (= (elt (cdr (elt s1 2)) i) (elt (cdr (elt s2 2)) i))
       (= (elt (car (elt s1 3)) i) (elt (car (elt s2 3)) i))
       (= (elt (cdr (elt s1 3)) i) (elt (cdr (elt s2 3)) i))))

moons
(((-1 0 2) 0 0 0) ((2 -10 -7) 0 0 0) ((4 -8 8) 0 0 0) ((3 5 -1) 0 0 0))

(any-equal (car moons) (car moons))

(defun any-system-equal (s1 s2)
  (or
   (all-equal s1 s2 0)
   (all-equal s1 s2 1)
   (all-equal s1 s2 2)))

(defun loop-energy (system n)
  (let ((initial system)
        (x-cycle)
        (y-cycle)
        (z-cycle))
    (loop
     for i from 0 to n
     until (and x-cycle y-cycle z-cycle) do
     (progn
       (if (= 0 (% i 1000))
           (print (list i (system-energy system) system)))
       (if (> i 0)
           (progn
             (if (and (not x-cycle)
                      (all-equal initial system 0))
                 (setq x-cycle i))
             (if (and (not y-cycle)
                      (all-equal initial system 1))
                 (setq y-cycle i))
             (if (and (not z-cycle)
                      (all-equal initial system 2))
                 (setq z-cycle i))))
       (setq system (step-system system)))
     finally return (list x-cycle y-cycle z-cycle))))

(loop-energy moons 100)

(0 0 (((-1 0 2) 0 0 0) ((2 -10 -7) 0 0 0) ((4 -8 8) 0 0 0) ((3 5 -1) 0 0 0)))
(18 28 44)

(setq moons2
      (list (make-moon -8 -10 0)
            (make-moon 5 5 10)
            (make-moon 2 -7 3)
            (make-moon 9 -8 -3)))

(loop-energy problem1 10000)
;; => (2028 5898 4702)


;; <x=5, y=13, z=-3>
;; <x=18, y=-7, z=13>
;; <x=16, y=3, z=4>
;; <x=0, y=8, z=8>
(setq problem1
      (list (make-moon 5 13 -3)
            (make-moon 18 -7 13)
            (make-moon 16 3 4)
            (make-moon 0 8 8)))

(loop-energy problem1 1000000)
;; => (186028 161428 144624)

;; LCM is 271,442,326,847,376
;; 271442326847376

