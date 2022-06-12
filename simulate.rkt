#lang typed/racket

;; CMSC15100 Spring 2021
;; Project 1
;; Matteo Restuccia

;; include CS151-specific definitions
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; include testing framework
(require typed/test-engine/racket-tests)

;; Project 1 modules
;;
(require "grid.rkt")

;;CODE HERE

;; A (Model C) collects together the information needed to run
;; a simulation, where C is the type of the individual cells.
;;
(define-struct (Model C)
  ([name : String]                         ;; the model's name
   [init : (Natural Natural -> (Grid C))]  ;; create the initial grid
   [update : ((Grid C) -> (Grid C))]       ;; update the grid one step
   [stable? : ((Grid C) -> Boolean)]))     ;; have we reached a stable state?

(: simulate : (All (C) (Model C) Natural Natural Natural -> (Grid C)))
;;Runs the simulation using model
(define (simulate mc r c s)
  (local
    {(: simulate-aux : Integer (Grid C) -> (Grid C))
     ;;auxiliary function to simulate
     (define (simulate-aux i acc)
       (if (or (< s i) ((Model-stable? mc) acc))
           acc
           (simulate-aux (+ i 1) ((Model-update mc) acc))))}
    (simulate-aux 0 ((Model-init mc) r c))))

;; The Render-Info structure is a container for various
;; bits of information needs to control the animated
;; simulation
;;
(define-struct (Render-Info C)
  ([radius : Integer]                  ;; the cell radius
   [draw-cell : (Coord C -> Image)]    ;; drawing function for cells
   [rate : Positive-Real]))            ;; the update rate in seconds
(: nat->pos : Natural -> Positive-Integer)
(define (nat->pos s)
  (cond
    [(= s 0) (error "must at least 1")]
    [else s]))
(: animate : (All (C) (Model C) Natural Natural Natural (Render-Info C) -> (Grid C)))
;;animates the simulation
(define (animate  mc r c s rend)
  (local
    {(: animate-aux : (Natural -> Positive-Integer) -> (Grid C))
     (define (animate-aux f)
       (big-bang
                 ((Model-init mc) r c) : (Grid C)
               [name "Pandemic"]
               [to-draw ((inst draw-grid C) (Render-Info-draw-cell rend) (Render-Info-radius rend))]
               [on-tick (Model-update mc) (Render-Info-rate rend) (f s)]
               [stop-when (Model-stable? mc)
                          ((inst draw-grid C)(Render-Info-draw-cell rend)
                                             (Render-Info-radius rend))]))}
    (animate-aux nat->pos)))

;; ===== Exports =====

(provide (struct-out Model)
         (struct-out Render-Info))

(provide simulate
         animate)


