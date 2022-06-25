;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pedestrian-indicator) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A PI (pedestrian indicator) is one of:
; - "don't walk"
; - "walk"
; - a Number between 0 and 13
; interpretation: red "don't walk", green "walk", and red countdown
; signal representing the possible states the pedestrian
; indicator might assume

(define HEIGHT 300) ; distance in pixels
(define WIDTH 300)

(define CNTR-WIDTH (/ WIDTH 2))
(define CNTR-HEIGHT (/ HEIGHT 2))

(define MTSCENE (empty-scene WIDTH HEIGHT))

; PI -> Image
; big-bang obtains the image of the current state
; of the pedestrian indicator by evaluating (render x)
(check-expect (render "walk") (place-image (text "walk" 20 "green")
                                CNTR-WIDTH CNTR-HEIGHT MTSCENE))
(check-expect (render "don't walk") (place-image (text "don't walk" 20 "red")
                                CNTR-WIDTH CNTR-HEIGHT MTSCENE))
(check-expect (render 1) (place-image (text (number->string 1) 20 "red")
                                CNTR-WIDTH CNTR-HEIGHT MTSCENE))
(define (render x)
  (cond
    [(string? x)(cond
                  [(string=? x "walk")
                   (place-image (text x 20 "green")
                                CNTR-WIDTH CNTR-HEIGHT MTSCENE)]
                  [(string=? x "don't walk")
                   (place-image (text x 20 "red")
                                CNTR-WIDTH CNTR-HEIGHT MTSCENE)])]
    [(<= 0 x 13)
     (place-image (text (number->string x) 20 "red")
                   CNTR-WIDTH CNTR-HEIGHT MTSCENE)]))

; PI -> PI
; yields the next state of the pedestrian indicator
(check-expect (next "walk") 13)
(check-expect (next 13) 12)
(check-expect (next "don't walk") "walk")
(check-expect (next 0) "don't walk")
(define (next cs)
  (cond
    [(string? cs) (cond
                    [(string=? "walk" cs) 13]
                    [(string=? "don't walk" cs) "walk"])]
    [(<= 1 cs 13)(sub1 cs)]
    [(zero? cs) "don't walk"]))

; PI -> PI
(define (main x)
  (big-bang x
    [to-draw render]
    [on-tick next 2]))