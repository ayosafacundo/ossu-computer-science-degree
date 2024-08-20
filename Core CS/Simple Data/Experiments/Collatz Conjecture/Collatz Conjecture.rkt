;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Collatz Conjecture|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


(define WIDTH 1920)
(define HEIGHT 980)

;; WorldState is World
;; interp. where things happen.
(define MTS (empty-scene WIDTH HEIGHT "grey"))

;; ListOfSteps is one of:
;; - empty
;; - (cons Number ListOfSteps)
;; interp. ListOfSteps is each step in the collatz conjecture from number given to main.

(define LOS0 (cons 1 empty))
(define LOS1 (cons 39 (cons 118 (cons 59 (cons 178 (cons 89 (cons 268 (cons 134 (cons 67 (cons 202 (cons 101 (cons 304 (cons 152 (cons 76 (cons 38 (cons 19 (cons 58 (cons 29 (cons 88 (cons 44 (cons 22 (cons 11 (cons 34 (cons 17 (cons 52 (cons 26 (cons 13 (cons 40 (cons 20 (cons  10 (cons 5 (cons 16 (cons 8 (cons 4 (cons 2 (cons 1 empty))))))))))))))))))))))))))))))))))))

#;
(define (fn-for-los los)                    
  (cond [(empty? los) (...)]                ; BASE CASE
        [else (... (first los)              ; Number
                   (fn-for-los (rest los)))]; NATURAL RECURSION
        ))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Number ListOfSteps)
;; - self-reference: (rest los) is ListOfSteps

;; (width, height) -> Image
;; Produces a bar.
(check-expect (bar 10 10) (rectangle 10 10 "solid" "black"))
(check-expect (bar 5 50) (rectangle 5 50 "solid" "black"))
(check-expect (bar 3 800) (rectangle 3 800 "solid" "black"))

(define (bar w h)
  (rectangle w h "solid" "black"))

;; ListOfSteps -> ListOfSteps
;; Inserts result of make-step into the end of the conjecture.
(check-expect (next-num (cons 11 empty)) (cons 11 (cons 34 empty)))
(check-expect (next-num (cons 101 empty)) (cons 101 (cons 304 empty)))
(check-expect (next-num (cons 67 (cons 202 (cons 101 (cons 304 (cons 152 empty)))))) (cons 67 (cons 202 (cons 101 (cons 304 (cons 152 (cons 76 empty)))))))

(define (next-num los)
  (cond [(empty? los) empty]
        [(= (first los) 4) (cons (first los) empty)]
        [(empty? (rest los)) (cons (first los) (cons (make-step (first los)) (next-num (rest los))))]
        [else (cons (first los) (next-num (rest los)))]))

;; Number -> Number
;; Produces next step in the Collatz Conjecture.
(check-expect (make-step 11) 34)
(check-expect (make-step 67) 202)
(check-expect (make-step 19) 58)
(check-expect (make-step 58) 29)

(define (make-step n)
  (if (even? n)
      (/ n 2)
      (+ (* 3 n) 1)))

;; ListOfSteps -> ListOfImages
;; Produces a list of Rectangles.
(check-expect (make-images (cons 11 empty) 1) (cons (bar (/ WIDTH 1) 11) empty))
(check-expect (make-images (cons 101 empty) 1) (cons (bar (/ WIDTH 1) 101) empty))
(check-expect (make-images (cons 67 (cons 202 (cons 101 (cons 304 (cons 152 empty))))) 5) (cons (bar (/ WIDTH 5) 67) (cons (bar (/ WIDTH 5) 202) (cons (bar (/ WIDTH 5) 101) (cons (bar (/ WIDTH 5) 304) (cons (bar (/ WIDTH 5) 152) empty))))))

(define (make-images los n)
  (cond [(empty? los) empty]
        [else (cons (bar (/ WIDTH n) (first los)) (make-images (rest los) n))]))

;; ListOfImages -> ListOfPositions
;; Produces a list of positions for the images
(define (do-pos loi n)
  (cond [(empty? loi) empty]
        [else (cons (make-posn (+ (* (image-width (first loi)) (- n (length loi))) (- n (length loi))) HEIGHT) (do-pos (rest loi) n))]))


;; WorldState -> Render
;; Renders the WorldState
(define (render r)
  (place-images (make-images r (length r)) (do-pos (make-images r (length r)) (length r)) MTS))


;; Universe
(define (main ws)
  (big-bang ws
    (on-tick  next-num)      ; ListOfSteps -> ListOfSteps
    (to-draw  render)        ; ListOfSteps -> WS
    ))

(main (cons 55 empty))






