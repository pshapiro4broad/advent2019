;;; -*- lexical-binding: t -*-


;; a program in memory is a map of (blocation -> data)
(defun make-program (program-string)
  (let ((memory (make-hash-table))
        (data-list (mapcar 'string-to-number (split-string program-string ","))))
    (loop
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

;; an amplifier is
;; - a program
;; - a program counter
;; - a paint bot

(defun make-amp (program)
  (let ((amp (make-hash-table)))
    (puthash 'program program amp)
    (puthash 'pc 0 amp)
    (puthash 'rel-base 0 amp)
    (puthash 'bot (make-paint-bot) amp)
    amp))

;; run an amplifier until it produces output or halts
(defun run-amplifier (amp)
  (let ((program (gethash 'program amp))
        (done))
    (defun prog-get (addr) (gethash addr program 0))
    (defun prog-set (addr val) (puthash addr val program))
    (defun prog-set-output (raw-addr val mode)
;;      (print (concat "prog-set-output: " (prin1-to-string (list raw-addr val mode))))
      (let ((addr
             (+ (prog-get raw-addr)
                (if (= mode 0)
                    0
                  (prog-rel-base)))))
        (puthash addr val program)))
    (defun prog-pc () (gethash 'pc amp))
    (defun prog-rel-base () (gethash 'rel-base amp))
    (defun prog-bot () (gethash 'bot amp))
    (defun get-operand (val mode)
;;      (print (prin1-to-string (list (prog-pc) val mode)))
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
      (let ((val (bot-get (prog-bot))))
;;        (print (concat "op-input: " (prin1-to-string (list val out-mode))))
        (prog-set-output (+ (prog-pc) 1) val out-mode)
        (list (+ 2 (prog-pc)))))
    (defun run-one-op ()
      (let* ((pc (prog-pc))
             (inst-parsed (decode-inst (prog-get pc)))
             (inst (car inst-parsed))
             (arg1 (get-operand (prog-get (+ pc 1)) (cadr inst-parsed)))
             (arg2 (get-operand (prog-get (+ pc 2)) (caddr inst-parsed))))
;;        (print (concat "pc: " (number-to-string pc) " inst: " (prin1-to-string inst-parsed) " arg1: " (prin1-to-string arg1) " arg2: " (prin1-to-string arg2)))
        ;; Run an op and return (<next-pc> . <output>)
        (cond ((= inst 1) (op '+ arg1 arg2 (cadddr inst-parsed)))
              ((= inst 2) (op '* arg1 arg2 (cadddr inst-parsed)))
              ((= inst 3) (op-input (cadr inst-parsed)))
              ((= inst 4)
               (bot-put (prog-bot) arg1)
               (list (+ 2 pc)))
              ((= inst 5) (list (if (not (= 0 arg1)) arg2 (+ 3 pc))))
              ((= inst 6) (list (if (= 0 arg1) arg2 (+ 3 pc))))
              ((= inst 7) (op-test '< arg1 arg2 (cadddr inst-parsed)))
              ((= inst 8) (op-test '= arg1 arg2 (cadddr inst-parsed)))
              ((= inst 9)
;;               (print (concat "rel-base: " (number-to-string (prog-rel-base))))
               (puthash 'rel-base (+ (prog-rel-base) arg1) amp)
               (list (+ 2 pc)))
              ((= inst 99)
               (cons 'done 'done))
              (t (print (concat "Unknown instruction: " (number-to-string inst) " at pc " (number-to-string pc)))))))
    (while (not done)
      (let ((result (run-one-op)))
        (puthash 'pc (car result) amp)
        ;; run until output appears, or the program exits
        (if (cdr result)
            (setq done (cdr result)))))
    (prog-bot)))

;; paint-bot
;; 'panels - pos -> white, black
;; 'pos - robot position (x . y)
;; 'dir - 'up 'down 'left 'right
;; 'state - 'paint-next or 'move-next

(defun make-paint-bot ()
  (let ((bot (make-hash-table)))
    (puthash 'panels (make-hash-table :test 'equal) bot)
    (puthash 'pos (cons 0 0) bot)
    (puthash 'dir 'U bot)
    (puthash 'state 'paint-next bot)
    ;; draw white pixel
    (puthash (cons 0 0) 1 (gethash 'panels bot))
    bot))

(defun bot-get (bot)
  (gethash
   (gethash 'pos bot)
   (gethash 'panels bot)
   0))

;; compute direction
;; - can only turn left or right
;; - only x or y changes at a time
;; - if U, 0 = y - 1, 1 = y + 1
;; - if D, 0 = y + 1, 1 = y - 1
;; - if L, 0 = x - 1, 1 = x + 1
;; - if R, 0 = x + 1, 1 = x - 1
(defun adjust-pos-and-dir (pos input dir)
  (cond ((eq dir 'U)
         (if (= input 0)
             (cons 'L (cons (1- (car pos)) (cdr pos)))
           (cons 'R (cons (1+ (car pos)) (cdr pos)))))
        ((eq dir 'D)
         (if (= input 1)
             (cons 'L (cons (1- (car pos)) (cdr pos)))
           (cons 'R (cons (1+ (car pos)) (cdr pos)))))
        ((eq dir 'L)
         (if (= input 0)
             (cons 'D (cons (car pos) (1- (cdr pos))))
           (cons 'U (cons (car pos) (1+ (cdr pos))))))
        ((eq dir 'R)
         (if (= input 1)
             (cons 'D (cons (car pos) (1- (cdr pos))))
           (cons 'U (cons (car pos) (1+ (cdr pos))))))))

(defun bot-put (bot input)
  (cond ((eq (gethash 'state bot) 'paint-next)
         (let* ((panels (gethash 'panels bot))
                (pos (gethash 'pos bot))
                (painted (gethash pos panels)))
           (if (= input 1)
               (if (or (not painted) (= painted 0))
                   (puthash pos 1 panels))
             (if (or (not painted) (= painted 1))
                 (puthash pos 0 panels))))
         (puthash 'state 'move-next bot))
        (t
         (let ((pos-and-dir (adjust-pos-and-dir (gethash 'pos bot) input (gethash 'dir bot))))
           (puthash 'pos (cdr pos-and-dir) bot)
           (puthash 'dir (car pos-and-dir) bot)
           (puthash 'state 'paint-next bot)))))

(defconst problem1 "3,8,1005,8,302,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,29,1006,0,78,2,1007,9,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,58,1006,0,7,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,83,2,1009,4,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,109,1,106,11,10,1006,0,16,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,138,2,108,0,10,1,101,14,10,1,1109,1,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,172,2,3,10,10,1006,0,49,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,201,1006,0,28,2,3,15,10,2,109,12,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,233,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,255,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,277,2,1107,9,10,101,1,9,9,1007,9,946,10,1005,10,15,99,109,624,104,0,104,1,21101,0,932856042280,1,21101,0,319,0,1105,1,423,21101,0,387512640296,1,21101,330,0,0,1106,0,423,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,46266346499,1,21102,1,377,0,1105,1,423,21102,1,46211836967,1,21102,1,388,0,1105,1,423,3,10,104,0,104,0,3,10,104,0,104,0,21102,1,825460941588,1,21102,411,1,0,1106,0,423,21101,709475738388,0,1,21102,1,422,0,1105,1,423,99,109,2,21201,-1,0,1,21101,0,40,2,21102,454,1,3,21101,0,444,0,1106,0,487,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,449,450,465,4,0,1001,449,1,449,108,4,449,10,1006,10,481,1102,1,0,449,109,-2,2106,0,0,0,109,4,2102,1,-1,486,1207,-3,0,10,1006,10,504,21101,0,0,-3,22101,0,-3,1,21201,-2,0,2,21102,1,1,3,21102,1,523,0,1105,1,528,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,551,2207,-4,-2,10,1006,10,551,22101,0,-4,-4,1105,1,619,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,570,0,0,1106,0,528,22102,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,589,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,611,21201,-1,0,1,21101,611,0,0,106,0,486,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0")

(setq paint (run-amplifier (make-amp (make-program problem1))))
#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data (panels #s(hash-table size 325 test equal rehash-size 1.5 rehash-threshold 0.8125 data ((0 . 0) 0 (1 . 0) 1 (1 . -1) 1 (2 . -1) 0 (2 . 0) 1 (3 . 0) 1 (3 . -1) 0 (4 . -1) 0 (4 . 0) 1 (5 . 0) 0 (5 . -1) 0 (6 . -1) 1 ...)) pos (41 . -4) dir R state paint-next ...))
(setq panels (gethash 'panels paint))
#s(hash-table size 325 test equal rehash-size 1.5 rehash-threshold 0.8125 data ((0 . 0) 0 (1 . 0) 1 (1 . -1) 1 (2 . -1) 0 (2 . 0) 1 (3 . 0) 1 (3 . -1) 0 (4 . -1) 0 (4 . 0) 1 (5 . 0) 0 (5 . -1) 0 (6 . -1) 1 ...))
(hash-table-count panels)
249
(setq min-x 1000)
(setq min-y 1000)
(setq max-x -1)
(setq max-y -1)

(maphash
 (lambda (key value)
   (let ((x (car key))
         (y (cdr key)))
     (setq min-x (min x min-x))
     (setq min-y (min y min-y))
     (setq max-x (max x max-x))
     (setq max-y (max y max-y))))
 panels)
nil

min-x
0
min-y
-5
max-x
42
max-y
0

(loop
 for i from 0 to 42 do
 (progn
   (loop
    for j from -5 to 0 do
    (if (= (gethash (cons i j) panels 0) 1)
        (princ "X ")
      (princ ". ")))
   (princ "\n")))
. . . . . . 
X X X X X X 
. . . X . X 
. . . X . X 
. . . . . X 
. . . . . . 
X X X X X X 
. . . X . . 
. X X . X . 
X . . . . X 
. . . . . . 
X X X X X X 
X . . X . X 
X . . X . X 
X . . . . X 
. . . . . . 
X X X X X X 
. . . X . . 
. X X . X . 
X . . . . X 
. . . . . . 
. X X X X . 
X . . . . X 
X . . . . X 
. X . . X . 
. . . . . . 
X X X X X X 
. . . X . X 
. . . X . X 
. . . . . X 
. . . . . . 
X X X X X X 
. . X . . X 
. X X . . X 
X . . X X . 
. . . . . . 
X X X X X X 
. . . X . . 
. X X . X . 
X . . . . X 
. . . . . . 
. . . . . . 
. . . . . . 
nil
