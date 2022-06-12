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
; (require "simple-model.rkt") ; optional
(require "model.rkt")

(define tmp (make-model-params 1 0.4 0.6 0.2 0.5 2))

(: run-model : Model-Params Natural Natural Natural -> (Grid Cell))
;;Runs the model
(define (run-model mp r c t)
  (simulate (make-model mp) r c t))

(: animate-model :
   Model-Params Natural Natural Natural Integer Positive-Real
   -> (Grid Cell))
;;Animates the model
(define (animate-model mp r c t rad ref)
  (animate (make-model mp) r c t (render-model rad ref)))
;; run tests
;;
(test)
