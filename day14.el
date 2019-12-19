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

;; (defun reverse-reactions (inventory reactions)
;;   (let ((next-inventory (copy-hash-table inventory)))
;;     (maphash
;;      (lambda (element amount)
;;        (if (and (not (eq element 'ORE)) (> amount 0))
;;            (let* ((reaction (gethash element reactions))
;;                   (output (car reaction))
;;                   (output-element (car output))
;;                   (output-amount (cdr output))
;;                   (inputs (cdr reaction)))
;;              (mapc
;;               (lambda (input)
;;                 (let ((input-element (car input))
;;                       (input-amount (cdr input)))
;;                   (puthash input-element
;;                            (+ (gethash input-element next-inventory 0)
;;                               input-amount)
;;                            next-inventory)))
;;               inputs)
;;              (puthash output-element
;;                       (- (gethash output-element next-inventory) output-amount)
;;                       next-inventory))))
;;      inventory)
;;     next-inventory))

(defun reverse-reactions (inventory reactions)
  (maphash
   (lambda (element amount)
     (if (and (not (eq element 'ORE)) (> amount 0))
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
                         (+ (gethash input-element inventory 0)
                            input-amount)
                         inventory)))
            inputs)
           (puthash output-element
                    (- (gethash output-element inventory) output-amount)
                    inventory))))
   inventory))

(defun convert-fuel (raw-reactions)
  (let ((reactions (make-reactions raw-reactions))
        (inventory (make-hash-table)))
    (defun only-non-ore-left ()
      (let (val)
        (maphash (lambda (element amount) (if (and (not (eq element 'ORE)) (> amount 0)) (setq val t))) inventory)
        val))
    (puthash 'FUEL 1 inventory)

    (while (only-non-ore-left)
      (reverse-reactions inventory reactions))
    (gethash 'ORE inventory)))

(convert-fuel reactions1)
;; => 31

(setq reactions2
      '("9 ORE => 2 A"
        "8 ORE => 3 B"
        "7 ORE => 5 C"
        "3 A, 4 B => 1 AB"
        "5 B, 7 C => 1 BC"
        "4 C, 1 A => 1 CA"
        "2 AB, 3 BC, 4 CA => 1 FUEL"))

(setq reactions3
      '(
        "157 ORE => 5 NZVS"
        "165 ORE => 6 DCFZ"
        "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
        "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
        "179 ORE => 7 PSHF"
        "177 ORE => 5 HKGWZ"
        "7 DCFZ, 7 PSHF => 2 XJWVT"
        "165 ORE => 2 GPVTF"
        "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
))

;; => 13312

(setq problem1
      '("1 FVBHS, 29 HWPND => 4 CPXDX" "5 TNWDG, 69 VZMS, 1 GXSD, 48 NCLZ, 3 RSRZ, 15 HWPND, 25 SGPK, 2 SVCQ => 1 FUEL" "1 PQRLB, 1 TWPMQ => 4 QBXC" "9 QBXC => 7 RNHQ" "12 VZMS => 6 MGQRZ" "6 QBVG, 10 XJWX => 6 BWLZ" "4 MVGN => 6 BHZH" "2 LKTWD => 7 FVBHS" "2 BWFK => 7 TFPQ" "15 VZBJ, 9 TSVN, 2 BWLZ => 2 TNWDG" "10 KVFL, 2 BWLZ, 1 VGSBF => 4 KBFJV" "12 TXCR, 2 JMBG => 4 DCFD" "5 VMDT, 6 JKPFT, 3 RJKJD => 7 LGWM" "1 LDFGW => 2 DHRBP" "129 ORE => 8 LDFGW" "9 DNVRJ => 8 BMNGX" "7 NLPB => 6 NCLZ" "1 VMDT, 6 DCFD => 9 SGRXC" "1 LDFGW, 2 VRHFB => 8 QHGQC" "10 VGSBF, 5 WVMG, 6 BWLZ => 3 BWFK" "4 KVFL, 1 TSVN => 6 SVCQ" "2 VZBJ, 3 SWJZ => 3 QZLC" "5 JMBG, 1 PQRLB => 3 CJLH" "13 LKTWD, 6 TFPQ => 3 WVRXR" "20 QHGQC, 10 NSPVD => 5 VGSBF" "5 TFPQ, 1 DHRBP, 2 KVFL => 8 NLPB" "2 KBFJV, 1 CJLH, 20 RNHQ, 1 BWLZ, 13 MNBK, 1 BHZH, 1 PKRJF => 8 RSRZ" "154 ORE => 2 VRHFB" "2 NHRCK => 7 DNVRJ" "2 VRHFB, 4 XJWX => 4 NHRCK" "1 TFPQ, 12 JMBG => 5 MNBK" "8 TMFS => 2 VZMS" "175 ORE => 2 TMFS" "1 LBZN, 2 SWJZ, 3 VGSBF => 8 BLDN" "7 KFJD, 5 WVRXR, 5 RJKJD => 6 MVGN" "3 RJKJD, 1 TXCR => 8 KVFL" "3 QHGQC, 1 MGQRZ, 10 VGSBF => 8 LKTWD" "178 ORE => 1 XJWX" "1 QBXC, 1 BWFK => 6 TSVN" "1 NHRCK, 2 DHRBP => 4 VZBJ" "1 LDFGW, 2 NHRCK, 10 BWLZ => 8 TWPMQ" "28 TWPMQ => 4 RJKJD" "10 SVCQ, 1 KVFL => 6 CZNMG" "3 VZMS, 3 MGQRZ => 3 WVMG" "19 MGQRZ => 8 KFJD" "3 WVMG => 6 PQRLB" "31 SVCQ, 1 TXCR => 8 VMDT" "20 KFJD, 5 CPXDX, 2 BLDN, 2 PQWJX, 12 TFPQ, 2 BHZH, 2 MVGN => 9 SGPK" "7 QZLC => 8 JMBG" "1 PQRLB => 1 HWPND" "9 VMDT, 5 CZNMG, 3 CPXDX, 1 MVGN, 8 VSMTK, 2 SGRXC, 1 MNBK, 8 LGWM => 7 GXSD" "2 NSPVD => 8 QBVG" "20 CZNMG => 4 PQWJX" "1 LDFGW => 4 NSPVD" "16 KBFJV, 22 BLDN => 2 VSMTK" "10 BWLZ => 9 LBZN" "1 BWLZ => 3 SWJZ" "1 HWPND => 9 TXCR" "12 CJLH, 9 LGWM, 3 BHZH => 6 PKRJF" "5 BMNGX => 7 JKPFT"))

;; binary search ?

(defun convert-fuel-with-ore (raw-reactions total-ore)
  (let ((reactions (make-reactions raw-reactions))
        (inventory (make-hash-table))
        (target-fuel 1))
    (defun only-non-ore-left ()
      (let (val)
        (maphash (lambda (element amount) (if (and (not (eq element 'ORE)) (> amount 0)) (setq val t))) inventory)
        val))
    (puthash 'ORE 0 inventory)
    (while (< (gethash 'ORE inventory) total-ore)
      (puthash 'FUEL target-fuel inventory)
      (setq target-fuel (* 2 target-fuel))
      (while (only-non-ore-left)
        (reverse-reactions inventory reactions)))
    (list target-fuel (gethash 'ORE inventory))))
convert-fuel-with-ore

(convert-fuel problem1)
;; => 899155


;; 1,000,000,000,000
;; 1,000,000 => 2
;; 10,000,000 => 24
;; 100,000,000 => 239
;; 1,000,000,000 => 2390
;; 10,000,000,000 => 

(convert-fuel-with-ore reactions1 (expt 10 7))
(524288 15204327)
(65536 1900515)
(4096 118755)
(512 14821)
(64 1833)
(8 207)

(convert-fuel-with-ore reactions3 (expt 10 10))
(1048576 12649779960)

(16384 197642141)

(256 107058435)

(32 13354012)

(32 13354012)

(benchmark-run-compiled (convert-fuel-with-ore problem1 10000000))
(2.546198 4 0.4737169999999935)

(2.399979 3 0.4550399999999968)

(benchmark-run-compiled (convert-fuel-with-ore problem1 100000000))

(19.235748 32 3.44524100000001)


(25.295303 32 4.587180999999973)


(benchmark-run-compiled (convert-fuel-with-ore problem1 1000000000))

(expt 10 9)

(expt 10 12)
1000000000000
1000000000000

(convert-fuel-with-ore reactions3 30000)

#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data (ORE 37025 FUEL 0 XJWVT 0 KHKGT -1 QDVJ -6 NZVS -4 GPVTF 0 HKGWZ -4 DCFZ 0 PSHF 0))
3

