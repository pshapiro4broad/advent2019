;;; -*- lexical-binding: t -*-

(defun make-program (program-string)
  (mapcar 'string-to-number (split-string program-string ",")))

(defun decode-inst (inst-raw)
  (let ((inst (mod inst-raw 100))
         (op1 (mod (/ inst-raw 100) 10))
         (op2 (mod (/ inst-raw 1000) 10))
         (op3 (mod (/ inst-raw 10000) 10))
         )
    (list inst op1 op2 op3)))

(defun permutations (list)
  "Return a list of all the permutations of the input."
  (if list
      (mapcan
       (lambda (e)
         (mapcar
          (lambda (perm) (cons e perm))
          (permutations (remove e list))))
       list)
    '(())))

;; an amplifier is
;; - a program
;; - a program counter
;; - an input queue

(defun make-amp (program phase)
  (let ((amp (make-hash-table)))
    (puthash 'program program amp)
    (puthash 'pc 0 amp)
    ;; input is a queue, read from head, push at end
    (puthash 'input (list phase) amp)
    amp))

;; run an amplifier until it produces output or halts
(defun run-amplifier (amp)
  (let ((program (gethash 'program amp))
        (done))
    (defun prog-get (addr) (nth addr program))
    (defun prog-set (addr val) (setf (nth addr program) val))
    (defun prog-pc () (gethash 'pc amp))
    (defun get-operand (val mode)
      (if (and (numberp val) (= mode 0))
          ;; val is addr
          (prog-get val)
        val))
    (defun op (f arg1 arg2)
      (let ((out-pos (prog-get (+ (prog-pc) 3))))
        (prog-set out-pos (apply f (list arg1 arg2)))))
    (defun op-test (f arg1 arg2)
      (let ((out-pos (prog-get (+ (prog-pc) 3))))
        (prog-set out-pos (if (apply f (list arg1 arg2)) 1 0))))
    (defun op-input ()
      (let* ((out-pos (prog-get (+ (prog-pc) 1)))
             (input (gethash 'input amp))
             (val (car input)))
        (prog-set out-pos val)
        (puthash 'input (cdr input) amp)))
    (defun run-one-op ()
      (let* ((pc (prog-pc))
             (inst-parsed (decode-inst (prog-get pc)))
             (inst (car inst-parsed))
             (arg1 (get-operand (prog-get (+ pc 1)) (cadr inst-parsed)))
             (arg2 (get-operand (prog-get (+ pc 2)) (caddr inst-parsed))))
        ;; Run an op and return (<next-pc> . <output>)
        (cond ((= inst 1)
               (op '+ arg1 arg2)
               (list (+ 4 pc)))
              ((= inst 2)
               (op '* arg1 arg2)
               (list (+ 4 pc)))
              ((= inst 3)
               (op-input)
               (list (+ 2 pc)))
              ((= inst 4)
               (cons (+ 2 pc) arg1))
              ((= inst 5)
               (list (if (not (= 0 arg1)) arg2 (+ 3 pc))))
              ((= inst 6)
               (list (if (= 0 arg1) arg2 (+ 3 pc))))
              ((= inst 7)
               (op-test '< arg1 arg2)
               (list (+ 4 pc)))
              ((= inst 8)
               (op-test '= arg1 arg2)
               (list (+ 4 pc)))
              ((= inst 99)
               (cons 'done 'done))
              (t (print (concat "Unknown instruction: " (number-to-string inst) " at pc " (number-to-string pc)))))))
    (while (not done)
      (let ((result (run-one-op)))
        (puthash 'pc (car result) amp)
        ;; run until output appears, or the program exits
        (if (cdr result)
            (setq done (cdr result)))))
    done))

;; a machine state is
;; - a list of amplifiers
(defun make-machine (program phases)
  (mapcar (lambda (phase) (make-amp (make-program program) phase)) phases))

(defconst num-amps 5)

(defun run-machine (machine)
  (let ((last-output 0)
        (current-amp 0)
        (done))
    (while (not done)
      (let* ((amp (nth current-amp machine))
             (amp-input (gethash 'input amp)))
        ;; push previous output onto curren amp's input
        (puthash 'input (append amp-input (list last-output)) amp)
        (let ((output (run-amplifier amp)))
          (if (eq output 'done)
              (setq done t)
            (progn
              (setq last-output output)
              (setq current-amp (% (1+ current-amp) num-amps)))))))
    last-output))

(defun test-program (program)
  (let ((perms (permutations (number-sequence 5 (+ 5 (1- num-amps)))))
        (max-phase nil)
        (max-signal -1))
    (mapc
     (lambda (phase-sequence)
       (let ((machine-output (run-machine (make-machine program phase-sequence))))
         (if (and (not (eq machine-output 'done)) (> machine-output max-signal))
             (progn
               (setq max-signal machine-output)
               (setq max-phase phase-sequence)))))
     perms)
    (cons max-signal max-phase)))

(test-program "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
;; => (139629729 9 8 7 6 5)

(test-program "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
;; => (18216 9 7 8 5 6)


(defconst day7-program "3,8,1001,8,10,8,105,1,0,0,21,38,63,72,81,106,187,268,349,430,99999,3,9,101,5,9,9,1002,9,3,9,101,3,9,9,4,9,99,3,9,102,3,9,9,101,4,9,9,1002,9,2,9,1001,9,2,9,1002,9,4,9,4,9,99,3,9,1001,9,3,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,102,4,9,9,1001,9,2,9,1002,9,5,9,1001,9,2,9,102,3,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99")

(test-program day7-program)
;; => (2299406 6 5 9 7 8)
