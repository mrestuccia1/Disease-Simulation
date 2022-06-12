#lang typed/racket

;; CMSC15100 Spring 2021
;; Project 2
;; <YOUR NAME HERE>

;; include CS151-specific definitions
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

;; include testing framework
(require typed/test-engine/racket-tests)

;; Project 2 modules
;;
(require "grid.rkt")
(require "simulate.rkt")

;; The parameters that specify the model's behavior
(define-struct Model-Params
  ([inf-rate : Real]        ;; The scaling factor used to compute the probability of
                            ;; infection [0..1].
   [imm-adj : Real]         ;; The immunity adjustment factor [0..1]
   [vac-adj : Real]         ;; The vaccination adjustment factor [0..1]
   [vac-rate : Real]        ;; The rate of vaccination [0..1]
   [ill-rate : Real]        ;; The initial rate of infection [0..1]
   [ill-length : Natural])) ;; The length of the recovery time in steps

(: make-model-params : Real Real Real Real Real Natural -> Model-Params)
;; Creates a model with parameters
(define (make-model-params inf i-adj v-adj v-rate i-rate n)
  (if (and (<= 0 inf 1) (<= 0 i-adj 1) (<= 0 v-adj 1)
           (<= 0 v-rate 1) (<= 0 i-rate 1) (<= 1 n) (<= 0 (+ v-rate i-rate) 1))
      (Model-Params inf (- 1 i-adj) (- 1 v-adj) v-rate i-rate n)
      (error "invalid model parameters")))

(check-error (make-model-params 2 3 0 0 0 2) "invalid model parameters")
(check-error (make-model-params 0 0 0 0 0 0) "invalid model parameters")
(check-expect (make-model-params 1 1 1 1 0 2) (Model-Params 1 0 0 1 0 2))

;; the health of a cell
(define-type Health (U 'Well 'Ill 'Immune))

;; A (Cell health ticks vac?) represents the state of a grid cell, where health
;; is the current health of the cell, ticks is the number of simulation steps
;; until an ill cell turns immune, and vac? is #t for cells that have been
;; vaccinated.  If the cell is not ill, then ticks should be 0.
(define-struct Cell
  ([health : Health]
   [recovery-time : Integer]
   [vaccinated? : Boolean])) 

;==== Test Grid and Model Params ====

(define tg (Grid 3 3
                      (vector
                       (Cell 'Well 0 #t)
                       (Cell 'Ill 2 #t)
                       (Cell 'Ill 1 #f)
                       (Cell 'Immune 0 #t)
                       (Cell 'Well 0 #t)
                       (Cell 'Well 0 #t)
                       (Cell 'Well 0 #f)
                       (Cell 'Ill 4 #f)
                       (Cell 'Immune 0 #f))))

(define tg-stable (Grid 3 3
                   (vector
                    (Cell 'Ill 0 #t)
                    (Cell 'Ill 2 #t)
                    (Cell 'Ill 1 #f)
                    (Cell 'Ill 0 #t)
                    (Cell 'Ill 2 #t)
                    (Cell 'Ill 2 #t)
                    (Cell 'Ill 2 #f)
                    (Cell 'Ill 4 #f)
                    (Cell 'Ill 2 #f))))

(define tmp (make-model-params 1 0.4 0.6 0.2 0.5 2))
  
;;==== Continuing Work ====

(: count-ill-neighbors : (Grid Cell) -> Coord -> Natural)
;;Counts the ill neighbors
(define (count-ill-neighbors grid)
  (lambda ([c : Coord])
    (foldl (lambda ([c : Coord] [i : Natural])
           (if (symbol=? 'Ill (Cell-health (grid-ref grid c)))
               (+ i 1)
               i)) 0 ((neighbors grid) c))))

(check-expect ((count-ill-neighbors
                tg)
               (Coord 1 1)) 3)

(: compute-probability : Model-Params -> Cell Natural -> Real)
;;Computes probability
(define (compute-probability mp)
  (lambda ([cell : Cell] [n : Natural])
    (cond
      [(symbol=? 'Ill (Cell-health cell)) 0]
      [(and (Cell-vaccinated? cell) (symbol=? 'Immune (Cell-health cell)))
       (* n (Model-Params-imm-adj mp) (Model-Params-vac-adj mp) (Model-Params-ill-rate mp))]
      [(and (Cell-vaccinated? cell) (not (symbol=? 'Immune (Cell-health cell))))
       (* n 1 (Model-Params-vac-adj mp) (Model-Params-ill-rate mp))]
      [(and (not (Cell-vaccinated? cell)) (symbol=? 'Immune (Cell-health cell)))
       (* n (Model-Params-imm-adj mp) 1 (Model-Params-ill-rate mp))]
      [else
       (* n 1 1 (Model-Params-ill-rate mp))])))

(check-expect ((compute-probability (make-model-params 1 0.4 0.6 0.7 0.2 2)) (Cell 'Ill 2 #t) 3) 0)
(check-within ((compute-probability
                (make-model-params 1 0.4 0.6 0.7 0.2 2)) (Cell 'Immune 2 #t) 3) 0.144 0.01)
(check-within ((compute-probability
                (make-model-params 1 0.4 0.6 0.7 0.2 2)) (Cell 'Immune 2 #f) 3) .36 0.01)
(check-within ((compute-probability
                (make-model-params 1 0.4 0.6 0.7 0.2 2)) (Cell 'Well 2 #t) 3) 0.24 0.01)
(check-within ((compute-probability
                (make-model-params 1 0.4 0.6 0.7 0.2 2)) (Cell 'Well 2 #f) 3) 0.6 0.01)

(: update-cell : Model-Params -> (Grid Cell) -> Coord Cell -> Cell)
;;updates the cell
(define (update-cell mp)
  (lambda ([grid : (Grid Cell)])
    (lambda ([c : Coord] [cell : Cell])
      (local
        {(define p ((compute-probability mp) cell ((count-ill-neighbors grid) c)))
         (: help : Real -> Cell)
         ;;updates the cell taking using a probability
         (define (help p)
           (cond
             [(= 0 p) cell]
             [(<= 1 p) (Cell 'Ill
                             (Model-Params-ill-length mp)
                             (Cell-vaccinated? cell))]
             [else (if (<= (random) p)
                       (Cell 'Ill
                             (Model-Params-ill-length mp)
                             (Cell-vaccinated? cell))
                       cell)]))}
          (help p)))))

(: update-grid : Model-Params -> (Grid Cell) -> (Grid Cell))
;;Maps update-cell over grid, updates entire grid
(define (update-grid mp)
  (lambda ([grid : (Grid Cell)])
    (grid-map-with-coord ((update-cell mp) grid) grid)))

(: stable? : (Grid Cell) -> Boolean)
;;Checks if grid is stable, which is only when all cells are Ill
(define (stable? grid)
  (grid-andmap (lambda ([cell : Cell])
                 (symbol=? (Cell-health cell) 'Ill)) grid))

(check-expect (stable? tg-stable) #t)
(check-expect (stable? tg) #f)

(: init-cell : Model-Params -> Real -> Cell)
;;Creates an initial cell
(define (init-cell mp)
  (lambda ([r : Real])
    (cond
      [(< 0 r (Model-Params-vac-rate mp)) (Cell 'Well 0 #t)]
      [(and (< (Model-Params-vac-rate mp) r)
            (<= r (+ (Model-Params-vac-rate mp) (Model-Params-ill-rate mp))))
       (Cell 'Ill (Model-Params-ill-length mp) #f)]
      [else (Cell 'Well 0 #f)])))

(check-expect ((init-cell tmp) 0.5) (Cell 'Ill 2 #f))
(check-expect ((init-cell tmp) 0.2) (Cell 'Well 0 #f))
(check-expect ((init-cell tmp) 0.1) (Cell 'Well 0 #t))

(: initial-grid : Model-Params -> Natural Natural -> (Grid Cell))
;;Creates an Initial Grid
(define (initial-grid mp)
  (lambda ([r : Natural] [c : Natural])
    (build-grid r c (lambda ([c : Coord]) ((init-cell mp) (random))))))

(: make-model : Model-Params -> (Model Cell))
;;Makes Simple Model
(define (make-model mp)
  (Model "Random Model"
         (initial-grid mp)
         (update-grid mp)
         stable?))

(: coord-health->img : Coord Cell -> Image)
;; Creates a circle image
(define (coord-health->img c cell)
  (cond
      [(symbol=? 'Ill (Cell-health cell))
       (overlay (circle 20 "outline" "black")
                             (circle 20 "solid" "red"))]
      [(and (Cell-vaccinated? cell) (symbol=? 'Immune (Cell-health cell)))
       (overlay (circle 20 "outline" "black")
                             (circle 20 "solid" "yellow"))]
      [(and (Cell-vaccinated? cell) (not (symbol=? 'Immune (Cell-health cell))))
       (overlay (circle 20 "outline" "black")
                             (circle 20 "solid" "green"))]
      [(and (not (Cell-vaccinated? cell)) (symbol=? 'Immune (Cell-health cell)))
       (overlay (circle 20 "outline" "black")
                             (circle 20 "solid" "gray"))]
      [else (overlay (circle 20 "outline" "black")
                             (circle 20 "solid" "blue"))]))

(: render-model : Integer Positive-Real -> (Render-Info Cell))
;;Renders the model
(define (render-model r nat)
  (Render-Info r coord-health->img nat))
;; Exports
(provide Model-Params
         (struct-out Cell))

(provide make-model-params
         make-model
         render-model)

;; Test
(test)

