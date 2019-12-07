(defun make-program (program-string)
  (mapcar 'string-to-number (split-string program-string ",")))

(defun decode-inst (inst-raw)
  (let ((inst (mod inst-raw 100))
         (op1 (mod (/ inst-raw 100) 10))
         (op2 (mod (/ inst-raw 1000) 10))
         (op3 (mod (/ inst-raw 10000) 10))
         )
    (list inst op1 op2 op3)))

(defun get-operand (val mode program)
  (if (and (numberp val) (= mode 0))
      (nth val program)
    val))

(defun execute-program (program input)
  (defun op (program pc f arg1 arg2)
    (let ((out-pos (nth (+ pc 3) program)))
      (setf (nth out-pos program)
            (apply f (list arg1 arg2)))
      program))
  (defun op-test (program pc f arg1 arg2)
    (let ((out-pos (nth (+ pc 3) program)))
      (setf (nth out-pos program)
            (if (apply f (list arg1 arg2))
                1
              0))
      program))
  (defun op-input (program pc val)
    (let ((out-pos (nth (+ pc 1) program)))
      (setf (nth out-pos program) val)
      program))
  (defun op-output (program pc arg1)
    (setq output arg1)
    program)
  (defun ex (program pc input)
    (let* ((inst-parsed (decode-inst (nth pc program)))
           (inst (car inst-parsed))
           (arg1 (get-operand (nth (+ pc 1) program) (cadr inst-parsed) program))
           (arg2 (get-operand (nth (+ pc 2) program) (caddr inst-parsed) program))
           )
      (cond ((= inst 1)
             (ex (op program pc '+ arg1 arg2) (+ 4 pc) input))
            ((= inst 2)
             (ex (op program pc '* arg1 arg2) (+ 4 pc) input))
            ((= inst 3)
             (ex (op-input program pc (car input)) (+ 2 pc) (cdr input)))
            ((= inst 4)
             (ex (op-output program pc arg1) (+ 2 pc) input))
            ((= inst 5)
             (ex program
                 (if (not (= 0 arg1))
                     arg2
                   (+ 3 pc))
                 input))
            ((= inst 6)
             (ex program
                 (if (= 0 arg1)
                     arg2
                   (+ 3 pc))
                 input))
            ((= inst 7)
             (ex (op-test program pc '< arg1 arg2) (+ 4 pc) input))
            ((= inst 8)
             (ex (op-test program pc '= arg1 arg2) (+ 4 pc) input))
            ((= inst 99)
             program)
            (t (concat "Unknown instruction: " (number-to-string inst) " at pc " (number-to-string pc))))))
  (setq output nil)
  (ex program 0 input)
  output)

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

(defun test-program (program num-amps)
  (let ((perms (permutations (number-sequence 0 (1- num-amps))))
        (max-phase nil)
        (max-signal -1))
    (mapc
     (lambda (phase-sequence)
       (let ((last-output 0))
         (mapc
          (lambda (phase)
            (let ((output (execute-program program (list phase last-output))))
              (setq last-output output)))
          phase-sequence)
         (if (> last-output max-signal)
             (progn
               (setq max-signal last-output)
               (setq max-phase phase-sequence)))))
     perms)
    (cons max-signal max-phase)))

(test-program (make-program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") 5)

(test-program (make-program "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0") 5)
;; => (54321 0 1 2 3 4)

(test-program (make-program "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0") 5)
;; => (65210 1 0 4 3 2)

(defconst day7-program (make-program "3,8,1001,8,10,8,105,1,0,0,21,38,63,72,81,106,187,268,349,430,99999,3,9,101,5,9,9,1002,9,3,9,101,3,9,9,4,9,99,3,9,102,3,9,9,101,4,9,9,1002,9,2,9,1001,9,2,9,1002,9,4,9,4,9,99,3,9,1001,9,3,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,102,4,9,9,1001,9,2,9,1002,9,5,9,1001,9,2,9,102,3,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99"))

(test-program day7-program 5)
;; => (206580 2 0 1 4 3)

;; 1409363
;; => (314 225 1 225 6 6 1105 1 238 225 104 0 ...)

;; (execute-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" '(5))
