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
;; - an input queue

(defun make-amp (program initial-input)
  (let ((amp (make-hash-table)))
    (puthash 'program program amp)
    (puthash 'pc 0 amp)
    (puthash 'rel-base 0 amp)
    ;; input is a queue, read from head, push at end
    (puthash 'input initial-input amp)
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
      (let* ((input (gethash 'input amp))
             (val (car input)))
;;        (print (concat "op-input: " (prin1-to-string (list input val out-mode))))
        (prog-set-output (+ (prog-pc) 1) val out-mode)
        (puthash 'input (cdr input) amp)
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
              ((= inst 4) (cons (+ 2 pc) arg1))
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
    done))

;;(run-machine (make-machine "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" '((0))))
;;(run-machine (make-machine "1102,34915192,34915192,7,4,7,99,0" '((0))))
;;(run-machine (make-machine "104,1125899906842624,99" '((0))))

(defconst day9-program
  "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,3,1,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,21,1008,1101,427,0,1028,1102,23,1,1012,1101,32,0,1009,1101,37,0,1007,1102,1,892,1023,1102,27,1,1004,1102,1,38,1013,1102,1,20,1005,1101,0,29,1001,1101,0,22,1015,1102,1,35,1003,1101,0,39,1016,1102,34,1,1011,1101,899,0,1022,1102,195,1,1024,1101,36,0,1014,1101,0,24,1000,1102,1,31,1006,1101,0,28,1017,1101,422,0,1029,1102,1,33,1019,1102,1,26,1018,1102,1,0,1020,1102,25,1,1002,1102,712,1,1027,1101,0,190,1025,1101,0,715,1026,1102,1,1,1021,1101,30,0,1010,109,30,2105,1,-6,4,187,1106,0,199,1001,64,1,64,1002,64,2,64,109,-19,1206,10,211,1106,0,217,4,205,1001,64,1,64,1002,64,2,64,109,-13,1202,8,1,63,1008,63,28,63,1005,63,241,1001,64,1,64,1106,0,243,4,223,1002,64,2,64,109,8,1201,-2,0,63,1008,63,29,63,1005,63,263,1105,1,269,4,249,1001,64,1,64,1002,64,2,64,109,-9,2101,0,3,63,1008,63,24,63,1005,63,295,4,275,1001,64,1,64,1106,0,295,1002,64,2,64,109,12,2107,31,0,63,1005,63,317,4,301,1001,64,1,64,1106,0,317,1002,64,2,64,109,7,21101,40,0,0,1008,1016,43,63,1005,63,341,1001,64,1,64,1106,0,343,4,323,1002,64,2,64,109,-14,1208,-1,31,63,1005,63,363,1001,64,1,64,1106,0,365,4,349,1002,64,2,64,109,9,1208,-6,20,63,1005,63,387,4,371,1001,64,1,64,1105,1,387,1002,64,2,64,109,2,2102,1,-7,63,1008,63,31,63,1005,63,413,4,393,1001,64,1,64,1106,0,413,1002,64,2,64,109,21,2106,0,-6,4,419,1106,0,431,1001,64,1,64,1002,64,2,64,109,-25,2108,35,-6,63,1005,63,449,4,437,1106,0,453,1001,64,1,64,1002,64,2,64,109,3,21107,41,42,0,1005,1012,471,4,459,1105,1,475,1001,64,1,64,1002,64,2,64,109,7,21108,42,39,-2,1005,1017,495,1001,64,1,64,1105,1,497,4,481,1002,64,2,64,109,-8,1206,9,515,4,503,1001,64,1,64,1106,0,515,1002,64,2,64,109,4,1205,6,529,4,521,1105,1,533,1001,64,1,64,1002,64,2,64,109,-8,2107,26,-5,63,1005,63,553,1001,64,1,64,1106,0,555,4,539,1002,64,2,64,109,-6,2102,1,1,63,1008,63,26,63,1005,63,575,1105,1,581,4,561,1001,64,1,64,1002,64,2,64,109,10,2101,0,-8,63,1008,63,37,63,1005,63,601,1105,1,607,4,587,1001,64,1,64,1002,64,2,64,109,-19,1207,8,23,63,1005,63,627,1001,64,1,64,1106,0,629,4,613,1002,64,2,64,109,18,21101,43,0,3,1008,1013,43,63,1005,63,655,4,635,1001,64,1,64,1106,0,655,1002,64,2,64,109,-16,1207,6,25,63,1005,63,677,4,661,1001,64,1,64,1106,0,677,1002,64,2,64,109,25,21102,44,1,-4,1008,1015,44,63,1005,63,703,4,683,1001,64,1,64,1106,0,703,1002,64,2,64,109,17,2106,0,-9,1106,0,721,4,709,1001,64,1,64,1002,64,2,64,109,-16,1205,0,737,1001,64,1,64,1105,1,739,4,727,1002,64,2,64,109,-12,21107,45,44,5,1005,1013,759,1001,64,1,64,1106,0,761,4,745,1002,64,2,64,109,4,1201,-8,0,63,1008,63,27,63,1005,63,783,4,767,1106,0,787,1001,64,1,64,1002,64,2,64,109,-16,2108,25,4,63,1005,63,803,1105,1,809,4,793,1001,64,1,64,1002,64,2,64,109,27,21102,46,1,-5,1008,1018,43,63,1005,63,829,1106,0,835,4,815,1001,64,1,64,1002,64,2,64,109,-27,1202,8,1,63,1008,63,27,63,1005,63,857,4,841,1105,1,861,1001,64,1,64,1002,64,2,64,109,23,21108,47,47,-2,1005,1017,883,4,867,1001,64,1,64,1106,0,883,1002,64,2,64,109,-1,2105,1,5,1001,64,1,64,1106,0,901,4,889,4,64,99,21102,1,27,1,21102,915,1,0,1105,1,922,21201,1,29589,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,1,942,0,1106,0,922,21202,1,1,-1,21201,-2,-3,1,21102,957,1,0,1105,1,922,22201,1,-1,-2,1106,0,968,21202,-2,1,-2,109,-3,2106,0,0")

(run-machine (make-machine day9-program '(1)))
;; => 2738720997

(run-machine (make-machine day9-program '(2)))
;; => 50894

;; a machine state is
;; - a list of amplifiers
(defun make-machine (program phases)
  (mapcar (lambda (phase) (make-amp (make-program program) (list phase))) phases))

(defconst num-amps 1)

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
