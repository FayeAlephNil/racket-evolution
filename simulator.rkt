#lang racket

(require predicates)

(define X_MAX 100)

(define Y_MAX 100)

(define FOOD_VALUE_MAX 10)

(struct basic-object (x y) #:transparent #:mutable)

(struct basic-creature basic-object (dna species belly offspring parents) #:transparent #:mutable)

(struct basic-food basic-object (value) #:transparent #:mutable)

(define/contract (same-species? creatures)
  (-> (and/c list? (not/c empty?)) boolean?)
  (let ([spec (basic-creature-species (car creatures))])
    ((all? (lambda (x) (eq? (basic-creature-species x) spec)))
     creatures )))

(define/contract (basic-breed mix-dna . creatures)
  (->* (procedure?) () #:rest (and/c list? (not/c empty?) same-species?) basic-creature?)
  (let ([new-creature (struct-copy basic-creature (car creatures))])
    (set-basic-creature-dna!
     new-creature
     (mix-dna (map basic-creature-dna creatures)))
    (set-basic-creature-parents! new-creature creatures)
    (map (lambda (c)
         (set-basic-creature-offspring! c (cons new-creature (basic-creature-offspring c)))
         )
       creatures)
    new-creature))

(define/contract (gen-food [rng (current-pseudo-random-generator)])
  (->* () (pseudo-random-generator?) basic-food?)
  (basic-food
   (random 0 X_MAX rng) ; x
   (random 0 Y_MAX rng) ; y
   (+ 1 (* (- FOOD_VALUE_MAX 1) (random rng))) ; value
   ))

(define (gen-creature [rng (current-pseudo-random-generator)])
  (->* () (pseudo-random-generator?) basic-creature?)
  (basic-creature
   (random 0 X_MAX rng) ; x
   (random 0 Y_MAX rng) ; y
   0                    ; dna
   'basic               ; species
   0                    ; belly
   '()                  ; offspring
   '()                  ; parents
   ))
