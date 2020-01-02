;; -*- lexical-binding: t -*-

(setq problem1 "109,424,203,1,21102,11,1,0,1105,1,282,21102,18,1,0,1105,1,259,1201,1,0,221,203,1,21101,31,0,0,1105,1,282,21101,38,0,0,1106,0,259,21002,23,1,2,22101,0,1,3,21102,1,1,1,21101,0,57,0,1105,1,303,1201,1,0,222,20102,1,221,3,21001,221,0,2,21101,259,0,1,21101,0,80,0,1105,1,225,21102,1,76,2,21101,91,0,0,1106,0,303,1201,1,0,223,21001,222,0,4,21102,1,259,3,21101,0,225,2,21102,1,225,1,21102,1,118,0,1106,0,225,20101,0,222,3,21101,100,0,2,21102,1,133,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21101,148,0,0,1105,1,259,2102,1,1,223,20102,1,221,4,21001,222,0,3,21101,0,17,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21101,0,195,0,106,0,109,20207,1,223,2,21002,23,1,1,21102,1,-1,3,21101,214,0,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1201,-4,0,249,22101,0,-3,1,21201,-2,0,2,22102,1,-1,3,21101,0,250,0,1106,0,225,22101,0,1,-4,109,-5,2105,1,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22101,0,-2,-2,109,-3,2105,1,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,22101,0,-2,3,21102,1,343,0,1105,1,303,1106,0,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,21201,-4,0,1,21102,1,384,0,1106,0,303,1105,1,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21201,1,0,-4,109,-5,2106,0,0")

;; a program in memory is a map of (location -> data)
(defun make-program (program-string)
  (let ((memory (make-hash-table))
        (data-list (mapcar 'string-to-number (split-string program-string ","))))
    (cl-loop
     for data in data-list
     for loc from 0 to (length data-list)
     do (puthash loc data memory))
    memory))

(defun decode-inst (inst-raw)
  (let ((inst (mod inst-raw 100))
         (op1 (mod (/ inst-raw 100) 10))
         (op2 (mod (/ inst-raw 1000) 10))
         (op3 (mod (/ inst-raw 10000) 10))
         )
    (list inst op1 op2 op3)))

;; a computer is
;; - a program -- a map of memory locations to values
;; - a program counter register
;; - a rel-base register
;; - the i/o state

(defun make-computer (program)
  (let ((computer (make-hash-table)))
    (puthash 'program program computer)
    (puthash 'pc 0 computer)
    (puthash 'rel-base 0 computer)
    computer))

;; run a computer until it produces output or halts
(defun run-computer (computer io-state)
  (let ((program (gethash 'program computer)))
    (defun prog-get (addr) (gethash addr program 0))
    (defun prog-set (addr val) (puthash addr val program))
    (defun prog-set-output (raw-addr val mode)
;;      (print (list 'prog-set-output: raw-addr val mode))
      (let ((addr
             (+ (prog-get raw-addr)
                (if (= mode 0)
                    0
                  (prog-rel-base)))))
        (puthash addr val program)))
    (defun prog-pc () (gethash 'pc computer))
    (defun prog-rel-base () (gethash 'rel-base computer))
    (defun prog-io () io-state)
    (defun get-operand (val mode)
;;      (print (list (prog-pc) val mode))
      (cond ((= mode 0) (prog-get val))
            ((= mode 1) val)
            ((= mode 2) (prog-get (+ val (prog-rel-base))))
            (t (concat "Unknown mode: " (number-to-string mode)))))
    (defun op (f arg1 arg2 out-mode)
      (prog-set-output (+ (prog-pc) 3) (apply f (list arg1 arg2)) out-mode)
      (list (+ 4 (prog-pc))))
    (defun op-test (f arg1 arg2 out-mode)
      (prog-set-output (+ (prog-pc) 3) (if (apply f (list arg1 arg2)) 1 0) out-mode)
      (list (+ 4 (prog-pc))))
    (defun op-input (out-mode)
      (let ((val (io-get (prog-io))))
;;        (print (list 'op-input: val out-mode))
        (prog-set-output (+ (prog-pc) 1) val out-mode)
        (list (+ 2 (prog-pc)))))
    (defun run-one-op ()
      (let* ((pc (prog-pc))
             (inst-parsed (decode-inst (prog-get pc)))
             (inst (car inst-parsed))
             (arg1 (get-operand (prog-get (+ pc 1)) (cadr inst-parsed)))
             (arg2 (get-operand (prog-get (+ pc 2)) (caddr inst-parsed))))
;;        (print (list 'pc: pc 'inst: inst-parsed 'arg1: arg1 'arg2: arg2))
        ;; Run an op and return (<next-pc> . <output>)
        (cond ((= inst 1) (op '+ arg1 arg2 (cadddr inst-parsed)))
              ((= inst 2) (op '* arg1 arg2 (cadddr inst-parsed)))
              ((= inst 3) (op-input (cadr inst-parsed)))
              ((= inst 4)
               (io-put (prog-io) arg1)
               (list (+ 2 pc)))
              ((= inst 5) (list (if (not (= 0 arg1)) arg2 (+ 3 pc))))
              ((= inst 6) (list (if (= 0 arg1) arg2 (+ 3 pc))))
              ((= inst 7) (op-test '< arg1 arg2 (cadddr inst-parsed)))
              ((= inst 8) (op-test '= arg1 arg2 (cadddr inst-parsed)))
              ((= inst 9)
;;               (print (list 'rel-base: (prog-rel-base)))
               (puthash 'rel-base (+ (prog-rel-base) arg1) computer)
               (list (+ 2 pc)))
              ((= inst 99)
               (cons 'done 'done))
              (t (print (list 'Unknown 'instruction: inst 'at 'pc pc))))))
    (while (numberp (prog-pc))
      (let ((result (run-one-op)))
        (puthash 'pc (car result) computer)))
    (prog-io)))

;; IO state

(setq width 50)
(setq height 50)

(defun make-io-state (input)
  (cons input nil))

(defun io-put (io-state output)
  (setcdr io-state output))

(defun io-get (io-state)
  (let* ((input (car io-state))
         (val (car input)))
    (setcar io-state (cdr input))
    val))

(setq computer (make-computer (make-program problem1)))

(setq tiles (make-hash-table :test 'equal))

(cl-loop
 for i from 0 to height do
 (cl-loop
  for j from 0 to width do
  (let ((pos (cons i j))
        (io-state (make-io-state (list i j))))
    (run-computer (make-computer (make-program problem1)) io-state)
    (puthash pos (cdr io-state) tiles))))

(run-computer (make-computer (make-program problem1)) (make-io-state (list 1 0)))

(setq sum 0)
0


(cl-loop
 for i from 0 to height do
 (progn
   (cl-loop
    for j from 0 to width do
    (progn
      (setq sum (+ sum (gethash (cons i j) tiles)))
      (princ (gethash (cons i j) tiles))))
   (princ "\n")))
10000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000
00001000000000000000000000000000000000000000000000
00000100000000000000000000000000000000000000000000
00000010000000000000000000000000000000000000000000
00000001000000000000000000000000000000000000000000
00000000100000000000000000000000000000000000000000
00000000110000000000000000000000000000000000000000
00000000011000000000000000000000000000000000000000
00000000001100000000000000000000000000000000000000
00000000000110000000000000000000000000000000000000
00000000000011000000000000000000000000000000000000
00000000000011100000000000000000000000000000000000
00000000000001110000000000000000000000000000000000
00000000000000111000000000000000000000000000000000
00000000000000011100000000000000000000000000000000
00000000000000001110000000000000000000000000000000
00000000000000001111000000000000000000000000000000
00000000000000000111100000000000000000000000000000
00000000000000000011110000000000000000000000000000
00000000000000000001111000000000000000000000000000
00000000000000000001111100000000000000000000000000
00000000000000000000111110000000000000000000000000
00000000000000000000011110000000000000000000000000
00000000000000000000001111000000000000000000000000
00000000000000000000000111100000000000000000000000
00000000000000000000000111110000000000000000000000
00000000000000000000000011111000000000000000000000
00000000000000000000000001111100000000000000000000
00000000000000000000000000111110000000000000000000
00000000000000000000000000011111000000000000000000
00000000000000000000000000011111100000000000000000
00000000000000000000000000001111110000000000000000
00000000000000000000000000000111111000000000000000
00000000000000000000000000000011111100000000000000
00000000000000000000000000000001111110000000000000
00000000000000000000000000000001111111000000000000
00000000000000000000000000000000111111100000000000
00000000000000000000000000000000011111110000000000
00000000000000000000000000000000001111111000000000
00000000000000000000000000000000000111111100000000
00000000000000000000000000000000000111111110000000
00000000000000000000000000000000000011111111000000
00000000000000000000000000000000000001111111100000
00000000000000000000000000000000000000111111110000
00000000000000000000000000000000000000111111111000
00000000000000000000000000000000000000011111111100
nil
sum
209
