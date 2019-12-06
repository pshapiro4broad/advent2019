
(defun program-to-list (program-string)
  (mapcar 'string-to-number (split-string program-string ",")))

(defvar program-input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,1,10,23,27,2,27,13,31,1,31,6,35,2,6,35,39,1,39,5,43,1,6,43,47,2,6,47,51,1,51,5,55,2,55,9,59,1,6,59,63,1,9,63,67,1,67,10,71,2,9,71,75,1,6,75,79,1,5,79,83,2,83,10,87,1,87,5,91,1,91,9,95,1,6,95,99,2,99,10,103,1,103,5,107,2,107,6,111,1,111,5,115,1,9,115,119,2,119,10,123,1,6,123,127,2,13,127,131,1,131,6,135,1,135,10,139,1,13,139,143,1,143,13,147,1,5,147,151,1,151,2,155,1,155,5,0,99,2,0,14,0")

(defun execute-program (program)
  (defun op (program pc f) 
    (let ((out-pos (nth (+ pc 3) program)))
      (setf (nth out-pos program)
            (apply f (list (nth (nth (+ pc 1) program) program)
                           (nth (nth (+ pc 2) program) program))))
      program))
  (defun ex (program pc)
    (let ((inst (nth pc program)))
      (cond ((= inst 1)
             (ex (op program pc '+) (+ 4 pc)))
            ((= inst 2)
             (ex (op program pc '*) (+ 4 pc)))
            ((= inst 99)
             program))))
  (ex program 0))

; (execute-program (program-to-list "1,0,0,0,99"))
; (execute-program (program-to-list "2,3,0,3,99"))
; (execute-program (program-to-list "1,1,1,4,99,5,6,0,99"))


(execute-program
 (let ((program-list (program-to-list program-input)))
   (setf (nth 1 program-list) 12)
   (setf (nth 2 program-list) 2)
   program-list))
; => (6327510 12 2 2 1 1 2 3 1 3 4 3 ...)


(defun test-run (noun verb)
  (nth 0 
       (execute-program
        (let ((program-list (program-to-list program-input)))
          (setf (nth 1 program-list) noun)
          (setf (nth 2 program-list) verb)
          program-list))))

(defconst target 19690720)

; couldn't get cl-loop to work :(

; (cl-loop for noun from 0 to 5
;          for result = (cl-loop for verb from 0 to 5
;                                until (= (test-run noun verb) target)
;                               finally return verb)
;         until (not (= verb 100))
;         finally return (list noun verb))

(defvar noun)
(defvar verb)

;; apparently no TRO in elsip, so am using global state

(defun test-run-while ()
  (while (not (= (test-run noun verb) target))
    (setq noun (+ noun 1))
    (cond ((> noun 99)
           (setq noun 0)
           (setq verb (+ verb 1))))
    (if (> verb 99)
        "no solution found"))
  (+ (* noun 100) verb))

(progn
  (setq noun 0)
  (setq verb 0)
  (test-run-while))
; => 4112
