#lang typed/racket

;; CMSC15100 Spring 2021
;; Project 2
;; <YOUR NAME HERE>

;; include CS151-specific definitions
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

;; include testing framework
(require typed/test-engine/racket-tests)

;; ===== Data Definitions =====

;; A (Coord r c) is the index of a cell in row r, column c
;;
(define-struct Coord
  ([r : Integer]
   [c : Integer]))

;; A (Grid nr nc cells) represents a grid with nr rows of nc columns
;; each.  The cells component is a flat vector of cells in row-major
;; order, where the cell at (Coord r c) has index (+ (* r ncols) c).
;;
(define-struct (Grid A)
  ([nrows : Natural]
   [ncols : Natural]
   [cells : (Vectorof A)]))

;; ===== Coordinate Operations =====

(: neighbors : (All (A) (Grid A) -> Coord -> (Listof Coord)))
;; return the six neighbors of a grid cell
;;
(define (neighbors grid)
  (match grid
    [(Grid nr nc _)
     (lambda ([coord : Coord])
       (match coord
         [(Coord r c)
          (local
            {(define r-1 (modulo (- r 1) nr))
             (define r+1 (modulo (+ r 1) nr))
             (define c-1 (modulo (- c 1) nc))
             (define c+1 (modulo (+ c 1) nc))}
            (if (odd? r)
                (list (Coord r-1 c) (Coord r-1 c+1)
                      (Coord r c-1) (Coord r c+1)
                      (Coord r+1 c) (Coord r+1 c+1))
                (list (Coord r-1 c-1) (Coord r-1 c)
                      (Coord r c-1) (Coord r c+1)
                      (Coord r+1 c-1) (Coord r+1 c))))]))]))

;; ===== Index <-> Coordinate Conversions

(: index->coord : Integer -> Integer -> Coord)
;; given the number of columns in the grid,
;; convert an index in the cells vector to a Coord
(define (index->coord cols)
  (lambda ([i : Integer])
    (Coord (quotient i cols)
           (modulo i cols))))

(: coord->index : Integer -> Coord -> Integer)
;; given the number of columns in the grid,
;; convert a Coord to a vector index
(define (coord->index cols)
  (lambda ([coord : Coord])
    (+ (* (Coord-r coord) cols) (Coord-c coord))))

;; ===== Test Functions =====

(: coord->bool : Coord -> Boolean)
;;Fucntion used to test build-grid, outputs a #t
;;if the column portion of the coordinate is even, #f otherwise
(define (coord->bool c)
  (if (even? (Coord-c c)) #t #f))

(check-expect (coord->bool (Coord 1 2)) #t)
(check-expect (coord->bool (Coord 1 3)) #f)
(check-expect (coord->bool (Coord 1 0)) #t)

(: coord->bool2 : Coord -> Boolean)
;; Converts a Coord to a Boolean
(define (coord->bool2 c)
  (if (and (= (Coord-c c) 0) (= (Coord-r c) 0)) #t #f))

(check-expect (coord->bool2 (Coord 0 0)) #t)
(check-expect (coord->bool2 (Coord 1 0)) #f)
(check-expect (coord->bool2 (Coord 0 1)) #f)

(: bool->int : Boolean -> Integer)
;;Converts a Boolean to an Integer
(define (bool->int b)
  (if b 1 0))

(check-expect (bool->int #t) 1)
(check-expect (bool->int #f) 0)

(: coord->coord : Coord -> Coord)
;;Function to maintain coord type in build-grid
(define (coord->coord c)
  c)

(check-expect (coord->coord (Coord 2 2)) (Coord 2 2))

(: coord-bool->str : Coord Boolean -> String)
;;Takes a Coord and Boolean, and returns an Integer
(define (coord-bool->str c bool)
  (if bool "ye" "no"))

(check-expect (coord-bool->str (Coord 0 0) #t) "ye")
(check-expect (coord-bool->str (Coord 1 1) #f) "no")

(: coord->int : Coord Integer -> Integer)
;;Adds the colums values
(define (coord->int c acc)
  (+ acc (Coord-c c)))

(check-expect (coord->int (Coord 2 3) 0) 3)
(check-expect (coord->int (Coord 2 3) -1) 2)

(: coord-int->int : Coord Coord Integer -> Integer)
;;Adds the colums values
(define (coord-int->int c1 c acc)
  (+ acc (Coord-c c)))

(check-expect (coord-int->int (Coord 2 3) (Coord 4 5) 8) 13)
(check-expect (coord-int->int (Coord 2 3) (Coord 4 0) -1) -1)

(: coord-bool->img : Coord Boolean -> Image)
;;Takes a boolean and a Coord and returns an image
(define (coord-bool->img c i)
  (if i
      (overlay (circle 20 "outline" "black")
                (circle 20 "solid" "gray"))
      (circle 20 "outline" "black")))

(: coord->bool3 : Coord -> Boolean)
;;returns #t if column vaue of a Coord is less than 3
(define (coord->bool3 c)
  (if (< (Coord-c c) 3) #t #f))

;; ===== Grid Operations =====

(: build-grid : (All (A) Natural Natural (Coord -> A) -> (Grid A)))
;;Builds a grid using vectors
(define (build-grid r c coord->a)
  (Grid r c (build-vector (* r c)
                            (lambda ([i : Integer])
                              (coord->a ((index->coord c) i))))))

(check-expect (build-grid 3 3 coord->bool)
              (Grid 3 3 '#(#t #f #t #t #f #t #t #f #t)))
(check-expect (build-grid 1 1 coord->bool)
              (Grid 1 1 '#(#t)))
(check-expect (build-grid 2 2 coord->bool)
              (Grid 2 2 '#(#t #f #t #f)))
(check-expect (build-grid 4 6 coord->bool)
              (Grid 4 6 '#(#t #f #t #f #t #f
                           #t #f #t #f #t #f
                           #t #f #t #f #t #f
                           #t #f #t #f #t #f)))

(: grid-cells : (All (A) (Grid A) -> (Listof (Listof A))))
;; return the cells of the grid in list-of-list format
(define (grid-cells grid)
  (match grid
    [(Grid nr nc cells)
     (local
       {(: cell : Integer Integer -> A)
        ;; get the contents of the cell at row r, column c
        (define (cell r c) (vector-ref cells (+ (* r nc) c)))
        (: gather-rows : Integer (Listof (Listof A)) -> (Listof (Listof A)))
        ;; gather the rows by traversing the grid from bottom to top
        (define (gather-rows r rows)
          (if (< r 0)
              rows
              (local
                {(: gather-cells : Integer (Listof A) -> (Listof A))
                 ;; gather the cells of row r from right to left
                 (define (gather-cells c cells)
                   (if (< c 0)
                       cells
                       (gather-cells (- c 1) (cons (cell r c) cells))))}
                (gather-rows (- r 1) (cons (gather-cells (- nc 1) '()) rows)))))}
       (gather-rows (- nr 1) '()))]))

(check-expect
 (grid-cells (Grid 2 3 (vector 'A 'B 'C 'X 'Y 'Z)))
 (list
  (list 'A 'B 'C)
  (list 'X 'Y 'Z)))

(: grid-ref : (All (A) (Grid A) Coord -> A))
;;Finds the value of a specific cell. Assumes a valid Coord is inputted
(define (grid-ref grid c)
  (vector-ref (Grid-cells grid) ((coord->index (Grid-ncols grid)) c)))

(check-expect (grid-ref (build-grid 2 2 coord->bool) (Coord 1 1)) #f)
(check-expect (grid-ref (build-grid 1 1 coord->bool) (Coord 0 0)) #t)
(check-expect (grid-ref (build-grid 3 3 coord->bool) (Coord 2 1)) #f)

(: grid-set : (All (A) (Grid A) Coord A -> (Grid A)))
;; functional update of a cell in the grid.  This operation requires
;; making a complete copy of the grid.
(define (grid-set grid coord new-cell)
  (match grid
    [(Grid nr nc cells)
     (local
       {(define ix ((coord->index nc) coord))
        (: cell : Integer -> A)
        (define (cell jx) (if (= ix jx) new-cell (vector-ref cells jx)))}
       (Grid
        nr nc
        (build-vector (vector-length cells) cell)))]))

(check-expect
 (grid-set (Grid 2 3 (vector 'A 'B 'C 'X 'Y 'Z)) (Coord 1 1) 'W)
 (Grid 2 3 (vector 'A 'B 'C 'X 'W 'Z)))

(: grid-map : (All (A B) (A -> B) (Grid A) -> (Grid B)))
(define (grid-map a->b grid)
  (Grid (Grid-nrows grid) (Grid-ncols grid)
        (vector-map a->b (Grid-cells grid))))

(check-expect (grid-map bool->int (build-grid 3 3 coord->bool2))
              (Grid 3 3 '#(1 0 0 0 0 0 0 0 0)))
(check-expect (grid-map bool->int (build-grid 2 2 coord->bool))
              (Grid 2 2 '#(1 0 1 0)))
(check-expect (grid-map bool->int (build-grid 0 0 coord->bool2))
              (Grid 0 0 '#()))

(: grid-map-with-coord : (All (A B) (Coord A -> B) (Grid A) -> (Grid B)))
;;Map function that takes two values: one can be anything
;;other must be a Coord
(define (grid-map-with-coord f grid)
  (Grid (Grid-nrows grid) (Grid-ncols grid)
        (build-vector (* (Grid-nrows grid) (Grid-ncols grid))
                (lambda ([i : Integer]) (f ((index->coord (Grid-ncols grid)) i)
                                           (grid-ref grid ((index->coord (Grid-ncols grid)) i)))))))

(check-expect (grid-map-with-coord coord-bool->str (build-grid 2 2 coord->bool))
              (Grid 2 2 '#("ye" "no" "ye" "no")))
(check-expect (grid-map-with-coord coord-bool->str (build-grid 2 2 coord->bool2))
              (Grid 2 2 '#("ye" "no" "no" "no")))
(check-expect (grid-map-with-coord coord-bool->str (build-grid 0 0 coord->bool))
              (Grid 0 0 '#()))

(: grid-andmap : (All (A) (A -> Boolean) (Grid A) -> Boolean))
;; test a predicate against the elements of a vector
;; and return #t if any are true.
(define (grid-andmap f grid)
  (local
    {(: true? : Integer Boolean -> Boolean)
     (define (true? i acc)
       (if (< i (vector-length (Grid-cells grid)))
           (if (f (vector-ref (Grid-cells grid) i)) (true? (+ i 1) #t) #f)
           acc))}
    (true? 0 #t)))

(check-expect (grid-andmap coord->bool (build-grid 0 0 coord->coord)) #t)
(check-expect (grid-andmap coord->bool (build-grid 2 2 coord->coord)) #f)
(check-expect (grid-andmap coord->bool3 (build-grid 1 1 coord->coord)) #t)
(check-expect (grid-andmap coord->bool3 (build-grid 1 4 coord->coord)) #f)

(: grid-fold : (All (A B) (A B -> B) B (Grid A) -> B))
;; fold the function f over the vector’s elements
;; from left to right
(define (grid-fold f init grid)
  (local
    {(: helper : Integer B -> B)
     (define (helper i acc)
       (if (< i (vector-length (Grid-cells grid)))
           (helper (+ i 1) (f (vector-ref (Grid-cells grid) i) acc))
           acc))}
    (helper 0 init)))

(check-expect (grid-fold coord->int 0
                         (build-grid 2 2 coord->coord)) 2)
(check-expect (grid-fold coord->int 0
                         (build-grid 2 3 coord->coord)) 6)
(check-expect (grid-fold coord->int 0
                         (build-grid 0 0 coord->coord)) 0)
(check-expect (grid-fold coord->int 0
                         (build-grid 2 5 coord->coord)) 20)

(: grid-fold-with-coord : (All (A B) (Coord A B -> B) B (Grid A) -> B))
;; fold the function f over the vector’s elements
;; from left to right
(define (grid-fold-with-coord f init grid)
  (local
    {(: helper : Integer B -> B)
     (define (helper i acc)
       (if (< i (vector-length (Grid-cells grid)))
           (helper (+ i 1) (f ((index->coord (Grid-ncols grid)) i)
                              (vector-ref (Grid-cells grid) i) acc))
           acc))}
    (helper 0 init)))

(check-expect
 (grid-fold-with-coord coord-int->int 0
                           (build-grid 2 2 coord->coord)) 2)

(check-expect
 (grid-fold-with-coord coord-int->int 0
                           (build-grid 2 3 coord->coord)) 6)

(check-expect
 (grid-fold-with-coord coord-int->int 1
                           (build-grid 0 0 coord->coord)) 1)

(: draw-row : (All (A) (Coord A -> Image) Coord A (Grid A) Integer Image -> Image))
;;Builds a single row. i should be the radius of the image
;;created by coorda->img
(define (draw-row f c a grid i acc)
    (if (< (Coord-c c) (- (Grid-ncols grid) 1))
            (draw-row f (Coord (Coord-r c) (+ (Coord-c c) 1))
                                      (grid-ref grid
                                                (Coord (Coord-r c)
                                                       (+ (Coord-c c) 1)))
                                      grid i (beside acc (f c a)))
            (if (odd? (Coord-r c))
                (beside (rectangle i i "solid" "white") acc
                        (f c a))
                (beside acc
                        (f c a)))))

(: draw-grid : (All (A) (Coord A -> Image) Integer -> (Grid A) -> Image))
;;Puts toghether the rows. i should be the radius of the image
;;created by coorda->img
(define (draw-grid f r)
  (lambda ([grid : (Grid A)])
    (local
      {(: helper : Integer Image -> Image)
       (define (helper i acc)
         (if (< i (Grid-nrows grid))
             (helper (+ i 1)
                     (above acc
                            (draw-row f (Coord i 0)
                                      (grid-ref grid (Coord i 0)) grid (* r 2) empty-image)))
             acc))}
      (helper 0 empty-image))))
   
(test)
;; ===== Exports =====

(provide (struct-out Coord) 
         Grid)

;; ADD EXPORTS HERE
(provide neighbors
         build-grid
         (rename-out [Grid-nrows grid-nrows])
         (rename-out [Grid-ncols grid-ncols])
         grid-cells
         grid-ref
         grid-set
         grid-map
         grid-map-with-coord
         grid-andmap
         grid-fold
         grid-fold-with-coord
         draw-grid)
