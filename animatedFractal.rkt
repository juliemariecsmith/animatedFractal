#lang racket
; time spent: 13 hours

(require racket/draw)
(require colors)

(define imageWidth 2048)
(define imageHeight 1152)

(define myTarget (make-bitmap imageWidth imageHeight))
(define dc (new bitmap-dc% [bitmap myTarget]))
(send dc set-brush "black" 'solid)
(send dc draw-rectangle
      0 0
      2048
      1152)
(send dc set-pen "black" 3 'solid)
(define xw2sscale .5)
(define yw2sscale .5)
(define xw2soffset 1024)
(define yw2soffset 576)

(define (drawTri a b c d e f i)
  (define Tri (new dc-path%))  
  (send Tri move-to a b)
  (send Tri line-to c d)
  (send Tri line-to e d)
  (send Tri line-to a b)
  (send Tri close)
  (send dc set-pen (hsv->color (hsv (/ (modulo f 10) 10) 0.8 1)) 3 'solid)
  (send Tri scale (/ f 100000.0) (/ f 100000.0))
  (send Tri rotate (* 0.2 f))
  (send Tri scale  (* i xw2sscale) (* i yw2sscale))
  (send Tri translate xw2soffset yw2soffset)
  (send dc draw-path Tri))

(define (fractal a b c d e f i)
  (cond [(= f 0) #t]
        [else 
         (drawTri a b c d e f i)
         (fractal (* (- a 10) .99) (* (- b 10).99) (* (+ c 10) .99) (* (- d 10) .99) (* (+ e 10) .99) (- f 1) i)
         ]))

(define (makeoutputname testnum prefix) 
  (let ((suffix 
         (cond
           [(< testnum 10) (format "00~v.png" testnum)]
           [(< testnum 100) (format "0~v.png" testnum)]
           [ (format "~v.png" testnum)])))
    (string-append prefix suffix)))

(for/list ([i 600])
  (fractal 1900 950 2448 1100 1800 50000 (+ i 1))
  (define outName (makeoutputname i "myFractal"))
  myTarget
  (send myTarget save-file outName 'png))
