;;; -*- lexical-binding: t -*-

;; map is a list of asteriod coordinates
(defun make-map (grid)
  (loop
   for line being the elements of grid using (index row)
   append
   (loop
    for char being the elements of line using (index col)
    if (= ?# char)
    collect (cons col row))))

;; for each other point, compute the angle to it
;; if two points have the same angle, they're on the same line and count as 1
;; use group-by to count unique number of angles

(defun compute-angle (pos1 pos2)
  (atan (- (car pos1) (car pos2)) (- (cdr pos1) (cdr pos2))))

(defun compute-all-angles-from (pos map)
  (let ((raw-angles
         (mapcar
          (lambda (other-pos)
            (cons (compute-angle pos other-pos) other-pos))
          (seq-difference map (list pos)))))
    (cons pos (seq-group-by 'car raw-angles))))

(defun find-best-center (grid)
  (let ((map (make-map grid)))
    (car
     (sort
      (mapcar
       (lambda (pos)
         (compute-all-angles-from pos map))
       map)
      (lambda (s1 s2)
        (> (length (cdr s1)) (length (cdr s2))))))))

(defconst grid1
  '(".#..#"
    "....."
    "#####"
    "....#"
    "...##"))

(defconst grid2
  '("......#.#."
"#..#.#...."
"..#######."
".#.#.###.."
".#..#....."
"..#....#.#"
"#..#....#."
".##.#..###"
"##...#..#."
".#....####" ))
(defconst grid3
  '("#.#...#.#."
".###....#."
".#....#..."
"##.#.#.#.#"
"....#.#.#."
".##..###.#"
"..#...##.."
"..##....##"
"......#..."
".####.###."
))
(defconst grid4 '(".#..#..###"
"####.###.#"
"....###.#."
"..###.##.#"
"##.##.#.#."
"....###..#"
"..#.#..#.#"
"#..#.#.###"
".##...##.#"
".....#.#.."
))
(defconst grid5 '(".#..##.###...#######"
"##.############..##."
".#.######.########.#"
".###.#######.####.#."
"#####.##.#.##.###.##"
"..#####..#.#########"
"####################"
"#.####....###.#.#.##"
"##.#################"
"#####.##.###..####.."
"..######..##.#######"
"####.##.####...##..#"
".#####..#.######.###"
"##...#.##########..."
"#.##########.#######"
".####.#.###.###.#.##"
"....##.##.###..#####"
".#.#.###########.###"
"#.#.#.#####.####.###"
"###.##.####.##.#..##"))

(defconst problem1 '("#.#.##..#.###...##.#....##....###"
"...#..#.#.##.....#..##.#...###..#"
"####...#..#.##...#.##..####..#.#."
"..#.#..#...#..####.##....#..####."
"....##...#.##...#.#.#...#.#..##.."
".#....#.##.#.##......#..#..#..#.."
".#.......#.....#.....#...###....."
"#.#.#.##..#.#...###.#.###....#..#"
"#.#..........##..###.......#...##"
"#.#.........##...##.#.##..####..#"
"###.#..#####...#..#.#...#..#.#..."
".##.#.##.........####.#.#...##..."
"..##...#..###.....#.#...#.#..#.##"
".#...#.....#....##...##...###...#"
"###...#..#....#............#....."
".#####.#......#.......#.#.##..#.#"
"#.#......#.#.#.#.......##..##..##"
".#.##...##..#..##...##...##.....#"
"#.#...#.#.#.#.#..#...#...##...#.#"
"##.#..#....#..##.#.#....#.##...##"
"...###.#.#.......#.#..#..#...#.##"
".....##......#.....#..###.....##."
"........##..#.#........##.......#"
"#.##.##...##..###.#....#....###.#"
"..##.##....##.#..#.##..#.....#..."
".#.#....##..###.#...##.#.#.#..#.."
"..#..##.##.#.##....#...#........."
"#...#.#.#....#.......#.#...#..#.#"
"...###.##.#...#..#...##...##....#"
"...#..#.#.#..#####...#.#...####.#"
"##.#...#..##..#..###.#..........#"
"..........#..##..#..###...#..#..."
".#.##...#....##.....#.#...##...##"))

;; (find-best-center problem1)

(defun square (x) (* x x))

(defun distance (pos1 pos2)
  (sqrt
   (+ (square (- (car pos1) (car pos2)))
      (square (- (cdr pos1) (cdr pos2))))))

(defun laser-asteriods (grid)
  (let* ((result (find-best-center grid))
         (station (car result))
         (angle-and-points
          (mapcar
           (lambda (x) (cons (car x) (mapcar 'cdr (cdr x))))
           (cdr result)))
         (angles
          (sort
           angle-and-points
           (lambda (p1 p2)
             (let ((c1 (car p1))
                   (c2 (car p2)))
               (if (or (and (>= 0 c1) (>= 0 c2))
                       (and (< 0 c1) (< 0 c2)))
                   (> c1 c2)
                 (> c2 0))))))
         (seen (make-hash-table :test 'equal))
         (last-removed))
    ;; could sort above when computing angle-and-points
    (mapc
     (lambda (angle)
       (setcdr angle
               (sort (cdr angle) (lambda (p1 p2) (< (distance station p1) (distance station p2))))))
     angles)
    (while (< (hash-table-count seen) 200)
      (loop
       for angle in angles do
       (loop
        for point in (cdr angle)
        while (gethash point seen)
        finally do
        (and point
             (puthash point t seen)
             (setq last-removed point)))
       while (< (hash-table-count seen) 200)))
    last-removed))

(laser-asteriods problem1)
;; => (16 . 23)
