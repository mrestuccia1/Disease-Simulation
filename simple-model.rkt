#lang typed/racket

;; CMSC15100 Spring 2021
;; Project 1
;; Matteo Restuccia

;; include CS151-specific definitions
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

;; include testing framework
(require typed/test-engine/racket-tests)

;; Project 1 modules
;;
(require "grid.rkt")
(require "simulate.rkt")

;; the health of a cell
(define-type Health (U 'Well 'Ill 'Immune))

;; A (Cell health ticks) represents the state of a grid cell, where health is the current
;; health of the cell and ticks is the number of simulation steps until an ill cell
;; turns immune.  If the cell is not ill, then ticks should be 0.
(define-struct Simple-Cell
  ([health : Health]
   [recovery-time : Integer]))

(: is-ill? : Simple-Cell -> Boolean)
(define (is-ill? cell) (symbol=? (Simple-Cell-health cell) 'Ill))

;; a grid of cells in the simple model
(define-type Simple-Grid (Grid Simple-Cell))

(: neighbor-health : (Listof Coord)
   Simple-Grid Coord Simple-Cell Integer -> Simple-Cell)
;;Checks for the current state, then updates cell
(define (neighbor-health lc grid c cell i)
  (cond
    [(is-ill? cell) (if (>= 1 (Simple-Cell-recovery-time cell))
                        (Simple-Cell 'Immune 0)
                        (Simple-Cell 'Ill
                                     (- (Simple-Cell-recovery-time cell) 1)))]                                                
    [(symbol=? (Simple-Cell-health cell) 'Well)
     (if (< i 2)
      (match lc
        [(cons n1 nr) (if (is-ill? (grid-ref grid n1))
                          (neighbor-health nr grid c cell (+ i 1))
                          (neighbor-health nr grid c cell i))]
        ['() cell])
      (Simple-Cell 'Ill 5))]
    [else cell]))

(check-expect (neighbor-health
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                    (Coord 0 1))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3))))
                                 (Coord 0 1) (Simple-Cell 'Ill 0) 0) (Simple-Cell 'Immune 0))
(check-expect (neighbor-health
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                    (Coord 1 1))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3))))
                                 (Coord 1 1) (Simple-Cell 'Ill 3) 0) (Simple-Cell 'Ill 2))
(check-expect (neighbor-health
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                    (Coord 0 0))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3))))
                                 (Coord 0 0) (Simple-Cell 'Well 0) 0) (Simple-Cell 'Ill 5))
(check-expect (neighbor-health
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                    (Coord 1 0))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3))))
                                 (Coord 1 0) (Simple-Cell 'Immune 0) 0) (Simple-Cell 'Immune 0))
(check-expect (neighbor-health
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Ill 0) (Simple-Cell 'Ill 3)))))
                    (Coord 0 0))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 1))
                               (list (Simple-Cell 'Ill 1) (Simple-Cell 'Ill 3))))
                                 (Coord 0 0) (Simple-Cell 'Well 0) 0) (Simple-Cell 'Ill 5))

(: update-cell : Simple-Grid -> Coord Simple-Cell -> Simple-Cell)
;;Checks for the health of the neighbors, then updates cell
(define (update-cell grid)
  (lambda ([c : Coord] [cell : Simple-Cell])
    (neighbor-health ((neighbors grid) c) grid c cell 0)))

(check-expect ((update-cell
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                                 (Coord 0 1) (Simple-Cell 'Ill 0)) (Simple-Cell 'Immune 0))
(check-expect ((update-cell
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                                 (Coord 1 1) (Simple-Cell 'Ill 3)) (Simple-Cell 'Ill 2))
(check-expect ((update-cell
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                                 (Coord 0 0) (Simple-Cell 'Well 0)) (Simple-Cell 'Ill 5))
(check-expect ((update-cell
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                                 (Coord 1 0) (Simple-Cell 'Immune 0)) (Simple-Cell 'Immune 0))
(check-expect ((update-cell
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 1))
                               (list (Simple-Cell 'Ill 1) (Simple-Cell 'Ill 3)))))
                                 (Coord 0 0) (Simple-Cell 'Well 0)) (Simple-Cell 'Ill 5))
                           
(: update-grid : Simple-Grid -> Simple-Grid)
;; update the grid for one time step
(define (update-grid grid)
  (grid-map-with-coord (update-cell grid) grid))

(check-expect (update-grid
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
              (Grid 2 2 (list (list (Simple-Cell 'Ill 5) (Simple-Cell 'Immune 0))
                              (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 2)))))
(check-expect (update-grid
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Well 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Immune 0)))))
              (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Well 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Immune 0)))))
(check-expect (update-grid
               (Grid 2 2 (list (list (Simple-Cell 'Ill 5) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Ill 5) (Simple-Cell 'Ill 3)))))
              (Grid 2 2 (list (list (Simple-Cell 'Ill 4) (Simple-Cell 'Immune 0))
                               (list (Simple-Cell 'Ill 4) (Simple-Cell 'Ill 2)))))

(: neighbor-health? : (Listof Coord)
   Simple-Grid Coord Boolean Simple-Cell Integer -> Boolean)
;;Checks for the current state, then updates cell
(define (neighbor-health? lc grid c bool cell i)
  (if (symbol=? (Simple-Cell-health cell) 'Well)
      (if (< i 2)
          (match lc
            [(cons n1 nr) (if (is-ill? (grid-ref grid n1))
                              (neighbor-health? nr grid c bool cell (+ i 1))
                              (neighbor-health? nr grid c bool cell i))]
            ['() #t])
          #f)
      #t))

(check-expect (neighbor-health?
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                    (Coord 0 1))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3))))
                                 (Coord 0 1) #t (Simple-Cell 'Ill 0) 0) #t)
(check-expect (neighbor-health?
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                    (Coord 1 1))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3))))
                                 (Coord 1 1) #t (Simple-Cell 'Ill 3) 0) #t)
(check-expect (neighbor-health?
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                    (Coord 0 0))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3))))
                                 (Coord 0 0) #t (Simple-Cell 'Well 0) 0) #f)
(check-expect (neighbor-health?
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3)))))
                    (Coord 1 0))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                               (list (Simple-Cell 'Immune 0) (Simple-Cell 'Ill 3))))
                                 (Coord 1 0) #t (Simple-Cell 'Immune 0) 0) #t)
(check-expect (neighbor-health?
               ((neighbors (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 0))
                                           (list (Simple-Cell 'Ill 0) (Simple-Cell 'Ill 3)))))
                    (Coord 0 0))
               (Grid 2 2 (list (list (Simple-Cell 'Well 0) (Simple-Cell 'Ill 1))
                               (list (Simple-Cell 'Ill 1) (Simple-Cell 'Ill 3))))
                                 (Coord 0 0) #t (Simple-Cell 'Well 0) 0) #f)

(: update-cell-bool : Simple-Grid -> Coord Simple-Cell Boolean -> Boolean)
;;Checks for the health of the neighbors, then updates cell
(define (update-cell-bool grid)
  (lambda ([c : Coord] [cell : Simple-Cell] [bool : Boolean])
    (if bool
        (neighbor-health? ((neighbors grid) c) grid c bool cell 0)
        #f)))

(: stable?-aux : Simple-Grid Boolean -> Boolean)
;; update the grid for one time step
(define (stable?-aux grid bool)
      (grid-fold-with-coord (update-cell-bool grid) bool grid))

(: stable? : Simple-Grid -> Boolean)
(define (stable? grid)
  (stable?-aux grid #t))

(define-struct Simple-Init-Cell
  ([coord : Coord]
   [health : Health]
   [recovery-time : Integer]))

(: initial-grid : (Listof Simple-Init-Cell) -> Natural Natural -> Simple-Grid)
;; generate an initial grid from a list of initial cell values and
;; the grid dimensions.
(define (initial-grid init-cells)
  (lambda ([nr : Natural] [nc : Natural])
    (local
      {(: update : Simple-Init-Cell Simple-Grid -> Simple-Grid)
       ;; update a single cell as specified by the initial cell value
       (define (update init-cell grid)
         (match init-cell
           [(Simple-Init-Cell (Coord r c) h t)
            (grid-set grid (Coord (modulo r nr) (modulo c nc)) (Simple-Cell h t))]))}
      ;; apply the update operation for each initial cell value
      (foldl update
             (build-grid nr nc (lambda ([c : Coord]) (Simple-Cell 'Well 0)))
             init-cells))))

(: make-simple-model : (Listof Simple-Init-Cell) -> (Model Simple-Cell))
;;Makes a simple model
(define (make-simple-model list)
  (Model "Pandemic"
         (initial-grid list)
         update-grid
         stable?))

(: coord-health->img : Coord Simple-Cell -> Image)
;; Creates a circle image
(define (coord-health->img c cell)
  (cond
    [(is-ill? cell) (overlay (circle 20 "outline" "black")
                             (circle 20 "solid" "red"))]
    [(symbol=? (Simple-Cell-health cell) 'Well)
               (overlay (circle 20 "outline" "black")
                             (circle 20 "solid" "green"))]
    [else (overlay (circle 20 "outline" "black")
                             (circle 20 "solid" "yellow"))]))


(: render-simple-model : Integer Positive-Real -> (Render-Info Simple-Cell))
(define (render-simple-model i nat)
  (Render-Info i
               coord-health->img
               nat))

;; ===== Exports =====

(provide (struct-out Simple-Init-Cell)
         Simple-Cell
         Simple-Grid)

(provide make-simple-model
         render-simple-model)

(test)
