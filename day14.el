;; -*- lexical-binding: t -*-

(setq reactions1
      '("10 ORE => 10 A"
        "1 ORE => 1 B"
        "7 A, 1 B => 1 C"
        "7 A, 1 C => 1 D"
        "7 A, 1 D => 1 E"
        "7 A, 1 E => 1 FUEL"))

;; 1 FUEL => 7 A 1 E
;; 7 A => ORE
;; 1 E => 7 A 1 D
;; 7 A => ORE
;; 1 D => 7 A 1 C
;; 7 A => ORE
;; 1 C => 7 A 1 B
;; 7 A => ORE
;; 1 B => ORE
;; (* 4 7) A => 28 A => 30 ORE
;; 1 B => 1 ORE => 31 ORE

;; a chem is (<chem-symbol> . <amount>)
(defun make-chem (s)
  (let ((data (split-string s)))
    (cons (intern (cadr data))
          (string-to-number (car data)))))

;; create map of output to inputs
;; 
(defun make-reactions (raw-reactions)
  (let ((table (make-hash-table)))
    (mapc
     (lambda (raw-reaction)
       (let* ((reaction (split-string raw-reaction "=>" t " "))
              (output (make-chem (cadr reaction)))
              (inputs (mapcar 'make-chem
                              (split-string (car reaction) "," t " "))))
         (puthash (car output) (cons output inputs) table)))
     raw-reactions)
    table))

(setq reactions (make-reactions reactions1))

;;
(setq inventory (make-hash-table))
#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ( ...))
(puthash 'FUEL 1 inventory)

(defun convert-inventory (inventory reactions)
  (let ((next-inventory (copy-hash-table inventory)))
    (maphash
     (lambda (element amount)
       (let* ((reaction (gethash element reactions))
              (output (car reaction))
              (output-element (car output))
              (output-amount (cdr output))
              (inputs (cdr reaction)))
         (mapc
          (lambda (input)
            (let ((input-element (car input))
                  (input-amount (cdr input)))
              (puthash input-element
                       (+ (gethash input-element next-inventory 0)
                          input-amount)
                       next-inventory)))
          inputs)))
     inventory)
    next-inventory))

(setq next-inventory (convert-inventory inventory reactions))
next-inventory
#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data (FUEL 1 A 7 E 1))
