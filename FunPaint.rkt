;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname FunPaint.racket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; HOMEWORK 9- SIMRAN AJWANI & NATE GORDON

;; Project: FunPaint

(require 2htdp/image)
(require 2htdp/universe)

(define-struct tri [length color])
;; A Triangle is a (make-tri Number Color)
;; Interpretation: Represents an equilateral triangle, where
;; - _length_ is the side length and
;; - _color_ is the color
;; Examples:
(define TRI-1 (make-tri 30 "blue"))
(define TRI-2 (make-tri 20 "violet"))
(define TRI-3 (make-tri 5 "black"))
;; Template:
;; tri-templ: Triangle -> ?
(define (tri-templ t)
  (... (tri-length t) ...
       (tri-color t) ...))

(define-struct rect [width height color])
;; A Rectangle is a (make-rect Number Number Color)
;; Interpretation: Represents a rectangle, where
;; - _width_ is the width,
;; - _height_ is the height, and
;; - _color_ is the color
;; Examples:
(define RECT-1 (make-rect 20 10 "green"))
(define RECT-2 (make-rect 15 30 "red"))
(define RECT-3 (make-rect 20 30 "orange"))
;; Template:
;; rect-templ: Rectangle -> ?
(define (rect-templ r)
  (... (rect-width r) ...
       (rect-height r) ...
       (rect-color r) ...))

(define-struct circ [radius color])
;; A Circle is a (make-circ Number Color)
;; Interpretation: Represents a circle, where
;; - _radius_ is the radius and
;; - _color_ is the color
;; Examples:
(define CIRC-1 (make-circ 10 "yellow"))
(define CIRC-2 (make-circ 20 "red"))
(define CIRC-3 (make-circ 5 "violet"))
;; Template:
;; circ-templ: Circle -> ?
(define (circ-templ c)
  (... (circ-radius c) ...
       (circ-color c) ...))

(define-struct str [length color])
;; A Star is a (make-str Number Color)
;; Interpretation: Represents an equilateral star, where
;; - _length_ is the side length and
;; - _color_ is the color
;; Examples:
(define STR-1 (make-str 5 "black"))
(define STR-2 (make-str 20 "orange"))
(define STR-3 (make-str 15 "blue"))
;; Template:
;; str-templ: Star -> ?
(define (str-templ s)
  (... (str-length s) ...
       (str-color s) ...))

;; A Shape is one of:
;; - (make-tri Number Number Color)
;; - (make-rect Number Number Color)
;; - (make-circ Number Color)
;; - (make-str Number Color)
;; Interpretation: Represents the different shapes that can be drawn in Fun Paint
;; Examples:
(define TRIANGLE (make-tri 20 "blue"))
(define RECTANGLE (make-rect 55 50 "violet"))
(define CIRCLE (make-circ 30 "red"))
(define STAR (make-str 30 "green"))
;; Template:
;; shape-templ: Shape -> ?
(define (shape-templ s)
  (cond
    [(tri? s) (... (tri-templ s) ...)]
    [(rect? s) (... (rect-templ s) ...)]
    [(circ? s) (... (circ-templ s) ...)]
    [(str? s) (... (str-templ s) ...)]))

(define-struct placed-shape [shape posn])
;; A PlacedShape is a (make-placed-shape Shape Position)
;; Interpretation: Represents a shape that is placed, where
;; - _shape_ is the shape and
;; - _posn_ is the position that the shape is placed at
;; Examples:
(define PS-1 (make-placed-shape TRIANGLE (make-posn 240 350)))
(define PS-2 (make-placed-shape RECTANGLE (make-posn 156 140)))
(define PS-3 (make-placed-shape CIRCLE (make-posn 150 360)))
(define PS-4 (make-placed-shape STAR (make-posn 200 200)))
;; Template:
;; placed-shape-templ: PlacedShape -> ?
(define (placed-shaped-templ ps)
  (... (shape-templ (placed-shape-shape ps)) ...
       (posn-x (placed-shape-posn ps)) ...
       (posn-y (placed-shape-posn ps)) ...))

;; A ListOfPlacedShapes is one of:
;; - empty
;; - (cons PlacedShape ListOfPlacedShapes)
;; Interpretation: Represents a list of the shapes that have been placed so far in the world state
;; Examples:
(define LOPS-1 empty)
(define LOPS-2 (cons PS-1 LOPS-1))
(define LOPS-3 (cons PS-2 LOPS-2))
(define LOPS-4 (cons PS-3 LOPS-3))
(define LOPS-5 (cons PS-4 LOPS-4))
;; Template:
;; lops-templ: ListOfPlacedShapes -> ?
(define (lops-templ lops)
  (cond
    [(empty? lops) ...]
    [(cons? lops) (... (placed-shaped-templ (first lops)) ...
                       (lops-templ (rest lops)) ...)]))

(define BACKGROUND (empty-scene 400 400))
(define TOOLBAR-BACKGROUND (empty-scene 400 40))

(define-struct fp-ws [current-shape lops])
;; A FunPaintWorldState is a (make-fp-ws PlacedShape ListOfPlacedShapes)
;; Interpretation: Represents the world state of FunPaint, where
;; - _current-shape_ is the shape that is being placed currently, and
;; - _lops_ is the list of previous shapes that have been placed
;; Examples:
(define FPWS-1 (make-fp-ws PS-1 LOPS-1))
(define FPWS-2 (make-fp-ws PS-2 LOPS-2))
(define FPWS-3 (make-fp-ws PS-3 LOPS-3))
(define FPWS-4 (make-fp-ws PS-4 LOPS-4))
(define FPWS-5 (make-fp-ws PS-1 LOPS-5))
;; Template:
;; fp-ws-templ: FunPaintWorldState -> ?
(define (fp-ws-templ fpws)
  (... (placed-shape-templ (fp-ws-current-shape fpws)) ...
       (lops-templ (fp-ws-lops fpws)) ...)) 

;; draw-shape: Shape -> Image
;; Purpose: Takes in the information from a shape and turns it into a image
(define (draw-shape s)
  (cond
    [(tri? s) (triangle (tri-length s) "solid" (tri-color s))]
    [(rect? s) (rectangle (rect-width s) (rect-height s) "solid" (rect-color s))]
    [(circ? s) (circle (circ-radius s) "solid" (circ-color s))]
    [(str? s) (star (str-length s) "solid" (str-color s))]))

(check-expect (draw-shape TRIANGLE) (triangle 20 "solid" "blue"))
(check-expect (draw-shape RECTANGLE) (rectangle 55 50 "solid" "violet"))
(check-expect (draw-shape CIRCLE) (circle 30 "solid" "red"))
(check-expect (draw-shape STAR) (star 30 "solid" "green"))

;; draw-placed-shapes: ListOfPlacedShapes -> Image
;; Purpose: Takes in the list of placed shapes and places the shapes on the scene
(define (draw-placed-shapes lops)
  (cond
    [(empty? lops) BACKGROUND]
    [(cons? lops) 
     (place-image (draw-shape (placed-shape-shape (first lops)))
                  (posn-x (placed-shape-posn (first lops)))
                  (posn-y (placed-shape-posn (first lops)))
                  (draw-placed-shapes (rest lops)))]))

(check-expect (draw-placed-shapes LOPS-2)
              (place-image
               (triangle 20 "solid" "blue") 240 350
               BACKGROUND))
(check-expect (draw-placed-shapes LOPS-3)
              (place-image
               (rectangle 55 50 "solid" "violet") 156 140
               (place-image
                (triangle 20 "solid" "blue") 240 350
                BACKGROUND)))
(check-expect (draw-placed-shapes LOPS-4)
              (place-image
               (circle 30 "solid" "red") 150 360
               (place-image
                (rectangle 55 50 "solid" "violet") 156 140
                (place-image
                 (triangle 20 "solid" "blue") 240 350
                 BACKGROUND))))
(check-expect (draw-placed-shapes LOPS-5)
              (place-image
               (star 30 "solid" "green") 200 200
               (place-image
                (circle 30 "solid" "red") 150 360
                (place-image
                 (rectangle 55 50 "solid" "violet") 156 140
                 (place-image
                  (triangle 20 "solid" "blue") 240 350
                  BACKGROUND)))))

;; draw-mini: Shape -> Image
;; Purpose: Takes in the information from a shape and draws a mini version in the corner
(define (draw-mini s)
  (cond
    [(tri? s) (triangle 25 "solid" (tri-color s))]
    [(rect? s) (rectangle 25 15 "solid" (rect-color s))]
    [(circ? s) (circle 15 "solid" (circ-color s))]
    [(str? s) (star 20 "solid" (str-color s))]))

(check-expect (draw-mini TRIANGLE) (triangle 25 "solid" "blue"))
(check-expect (draw-mini RECTANGLE) (rectangle 25 15 "solid" "violet"))
(check-expect (draw-mini CIRCLE) (circle 15 "solid" "red"))
(check-expect (draw-mini STAR) (star 20 "solid" "green"))

;; text-mini: Shape -> Image
;; Purpose: Takes in the information from a shape and displays its size in the corner
(define (text-mini s)
  (cond
    [(tri? s) (text (number->string (tri-length s)) 15 "black")]
    [(rect? s) (text (string-append (number->string (rect-width s)) "   "
                                    (number->string (rect-height s))) 15 "black")]
    [(circ? s) (text (number->string (circ-radius s)) 15 "black")]
    [(str? s) (text (number->string (str-length s)) 15 "black")]))

(check-expect (text-mini TRIANGLE) (text (number->string 20) 15 "black"))
(check-expect (text-mini RECTANGLE) (text (string-append (number->string 55) "   "
                                                         (number->string 50)) 15 "black"))
(check-expect (text-mini CIRCLE) (text (number->string 30) 15 "black"))
(check-expect (text-mini STAR) (text (number->string 30) 15 "black"))

;; get-size: Shape -> Number
;; Purpose: Takes in a shape and returns its size
(define (get-size s)
  (cond
    [(tri? s) (tri-length s)]
    [(rect? s) (rect-width s)]
    [(circ? s) (circ-radius s)]
    [(str? s) (str-length s)]))

(check-expect (get-size TRIANGLE) 20)
(check-expect (get-size RECTANGLE) 55)
(check-expect (get-size CIRCLE) 30)
(check-expect (get-size STAR) 30)

;; get-color: Shape -> Color
;; Purpose: Takes in a shape and returns its color
(define (get-color s)
  (cond
    [(tri? s) (tri-color s)]
    [(rect? s) (rect-color s)]
    [(circ? s) (circ-color s)]
    [(str? s) (str-color s)]))

(check-expect (get-color TRIANGLE) "blue")
(check-expect (get-color RECTANGLE) "violet")
(check-expect (get-color CIRCLE) "red")
(check-expect (get-color STAR) "green")

;; change-size: Shape Number -> Shape
;; Purpose: Takes in a n input for size and changes the size of a shape by n (minimum size 10)
(define (change-size s n)
  (cond
    [(tri? s) (make-tri (max (+ n (tri-length s)) 10) (tri-color s))]
    [(rect? s) (make-rect (max (+ n (rect-width s)) 10)
                          (max (+ n (rect-height s)) 5)
                          (rect-color s))]
    [(circ? s) (make-circ (max (+ n (circ-radius s)) 10) (circ-color s))]
    [(str? s) (make-str (max (+ n (str-length s)) 10) (str-color s))]))

(check-expect (change-size TRIANGLE 5) (make-tri 25 "blue"))
(check-expect (change-size RECTANGLE 0) (make-rect 55 50 "violet"))
(check-expect (change-size CIRCLE 50) (make-circ 80 "red"))
(check-expect (change-size STAR -30) (make-str 10 "green"))

;; change-color: Shape Color -> Shape
;; Purpose: Takes in a color input and changes the color of a shape to the desired color
(define (change-color s c)
  (cond
    [(tri? s) (make-tri (tri-length s) c)]
    [(rect? s) (make-rect (rect-width s) (rect-height s) c)]
    [(circ? s) (make-circ (circ-radius s) c)]
    [(str? s) (make-str (str-length s) c)])) 

(check-expect (change-color TRIANGLE "green") (make-tri 20 "green"))
(check-expect (change-color RECTANGLE "yellow") (make-rect 55 50 "yellow"))
(check-expect (change-color CIRCLE "violet") (make-circ 30 "violet"))
(check-expect (change-color STAR "orange") (make-str 30 "orange"))

;; draw-scene: FunPaintWorldState -> FunPaintWorldState
;; Purpose: Takes in a world state and updates the world state based on the
;; current shape being placed, along with the toolbar and the mini version in the corner
(define (draw-scene fpws)
  (above/align "left" DRAW-TOOLBAR
               (place-image (above (draw-mini (placed-shape-shape (fp-ws-current-shape fpws)))
                                   (text-mini (placed-shape-shape (fp-ws-current-shape fpws)))) 
                            30 30
                            (draw-placed-shapes (fp-ws-lops fpws)))))

(check-expect (draw-scene FPWS-2)
              (above/align "left" DRAW-TOOLBAR 
                           (place-image (above (draw-mini RECTANGLE) (text-mini RECTANGLE))
                                        30 30
                                        (draw-placed-shapes LOPS-2))))
(check-expect (draw-scene FPWS-3)
              (above/align "left" DRAW-TOOLBAR
                           (place-image (above (draw-mini CIRCLE) (text-mini CIRCLE))
                                        30 30
                                        (draw-placed-shapes LOPS-3))))
(check-expect (draw-scene FPWS-4)
              (above/align "left" DRAW-TOOLBAR
                           (place-image (above (draw-mini STAR) (text-mini STAR))
                                        30 30
                                        (draw-placed-shapes LOPS-4))))
(check-expect (draw-scene FPWS-5)
              (above/align "left" DRAW-TOOLBAR
                           (place-image (above (draw-mini TRIANGLE) (text-mini TRIANGLE))
                                        30 30
                                        (draw-placed-shapes LOPS-5))))

(define BUTTON-BACK (square 20 "solid" "white"))

(define-struct button [name symbol tl-posn br-posn])
;; A Button is a (make-button Image Posn Posn)
;; Interpretation: Represents a button in the toolbar that can be clicked, where
;; - _name_ is the name of the button, 
;; - _symbol_ is the image representing the button,
;; - _tl-posn_ is the position of the top left corner of the button, and
;; - _br-posn_ is the position of the bottom right corner of the button
;; Examples:
(define CIRCLE-BUTTON (make-button "circle"
                                   (overlay (circle 9 "solid" "black") BUTTON-BACK) 
                                   (make-posn 0 0) (make-posn 20 20)))
(define TRIANGLE-BUTTON (make-button "triangle"
                                     (overlay (triangle 17 "solid" "black") BUTTON-BACK) 
                                     (make-posn 20 0) (make-posn 40 20)))
(define RECTANGLE-BUTTON (make-button "rectangle"
                                      (overlay (rectangle 18 13 "solid" "black") BUTTON-BACK) 
                                      (make-posn 40 0) (make-posn 60 20)))
(define STAR-BUTTON (make-button "star"
                                 (overlay (star 10 "solid" "black") BUTTON-BACK) 
                                 (make-posn 60 0) (make-posn 80 20)))
(define PLUS-BUTTON (make-button "plus"
                                 (overlay (text "+" 20 "black") BUTTON-BACK)
                                 (make-posn 80 0) (make-posn 100 20)))
(define MINUS-BUTTON (make-button "minus"
                                  (overlay (text "-" 20 "black") BUTTON-BACK)
                                  (make-posn 100 0) (make-posn 120 20)))
(define RED-BUTTON (make-button "red"
                                (square 20 "solid" "red")
                                (make-posn 120 0) (make-posn 140 20)))
(define ORANGE-BUTTON (make-button "orange"
                                   (square 20 "solid" "orange")
                                   (make-posn 140 0) (make-posn 160 20)))
(define YELLOW-BUTTON (make-button "yellow"
                                   (square 20 "solid" "yellow")
                                   (make-posn 160 0) (make-posn 180 20)))
(define GREEN-BUTTON (make-button "green"
                                  (square 20 "solid" "green")
                                  (make-posn 180 0) (make-posn 200 20)))
(define BLUE-BUTTON (make-button "blue"
                                 (square 20 "solid" "blue")
                                 (make-posn 200 0) (make-posn 220 20)))
(define VIOLET-BUTTON (make-button "violet"
                                   (square 20 "solid" "violet")
                                   (make-posn 220 0) (make-posn 240 20)))
;; Template:
;; button-templ: Button -> ?
(define (button-templ tb)
  (... (button-name tb) ...
       (button-symbol tb) ...
       (posn-x (button-tl-posn tb)) ...
       (posn-y (button-tl-posn tb)) ...
       (posn-x (button-br-posn tb)) ...
       (posn-y (button-br-posn tb)) ...))

;; A Toolbar is a [List-of Buttons]
(define TOOLBAR (list CIRCLE-BUTTON
                      TRIANGLE-BUTTON
                      RECTANGLE-BUTTON
                      STAR-BUTTON
                      PLUS-BUTTON
                      MINUS-BUTTON
                      RED-BUTTON
                      ORANGE-BUTTON
                      YELLOW-BUTTON
                      GREEN-BUTTON
                      BLUE-BUTTON
                      VIOLET-BUTTON))

;; Creates the toolbar image
(define DRAW-TOOLBAR
  (beside (button-symbol CIRCLE-BUTTON)
          (button-symbol TRIANGLE-BUTTON)
          (button-symbol RECTANGLE-BUTTON)
          (button-symbol STAR-BUTTON)
          (button-symbol PLUS-BUTTON)
          (button-symbol MINUS-BUTTON)
          (button-symbol RED-BUTTON)
          (button-symbol ORANGE-BUTTON)
          (button-symbol YELLOW-BUTTON)
          (button-symbol GREEN-BUTTON)
          (button-symbol BLUE-BUTTON)
          (button-symbol VIOLET-BUTTON)))

;; button-check: Posn-X Posn-Y [List-of Buttons] -> String
;; Purpose: Takes in x and y coordinates and outputs the name of the button on the toolbar
;; those coordinates corespond to
(define (button-check x y lob)
  (cond
    [(empty? lob) ""]
    [(cons? lob) (if (and (>= x (posn-x (button-tl-posn (first lob))))
                          (<= x (posn-x (button-br-posn (first lob))))
                          (>= y (posn-y (button-tl-posn (first lob))))
                          (<= y (posn-y (button-br-posn (first lob)))))
                     (button-name (first lob))
                     (button-check x y (rest lob)))]))                    

(check-expect (button-check 10 19 TOOLBAR) "circle")
(check-expect (button-check 150 5 TOOLBAR) "orange")
(check-expect (button-check 10 50 TOOLBAR) "")

;; shape-select: FunPaintWorldState Posn-X Posn-Y -> FunPaintWorldState
;; Purpose: Updates the world state based on the shape button that is clicked
(define (shape-select fpws x y)
  (cond
    [(string=? (button-check x y TOOLBAR) "circle")
     (make-fp-ws (make-placed-shape (make-circ
                                     (get-size (placed-shape-shape (fp-ws-current-shape fpws)))
                                     (get-color (placed-shape-shape (fp-ws-current-shape fpws))))
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "triangle")
     (make-fp-ws (make-placed-shape (make-tri
                                     (get-size (placed-shape-shape (fp-ws-current-shape fpws)))
                                     (get-color (placed-shape-shape (fp-ws-current-shape fpws))))
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "rectangle")
     (make-fp-ws (make-placed-shape (make-rect
                                     (get-size (placed-shape-shape (fp-ws-current-shape fpws)))
                                     (- (get-size (placed-shape-shape (fp-ws-current-shape fpws)))
                                        5)
                                     (get-color (placed-shape-shape (fp-ws-current-shape fpws))))
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "star")
     (make-fp-ws (make-placed-shape (make-str
                                     (get-size (placed-shape-shape (fp-ws-current-shape fpws)))
                                     (get-color (placed-shape-shape (fp-ws-current-shape fpws))))
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [else fpws]))

(check-expect (shape-select FPWS-1 40 10)
              (make-fp-ws (make-placed-shape (make-tri 20 "blue") (make-posn 0 0)) LOPS-1))
(check-expect (shape-select FPWS-2 60 5)
              (make-fp-ws (make-placed-shape (make-rect 55 50 "violet") (make-posn 0 0)) LOPS-2))
(check-expect (shape-select FPWS-3 1 1)
              (make-fp-ws (make-placed-shape (make-circ 30 "red") (make-posn 0 0)) LOPS-3))
(check-expect (shape-select FPWS-4 73 17)
              (make-fp-ws (make-placed-shape (make-str 30 "green") (make-posn 0 0)) LOPS-4))
(check-expect (shape-select FPWS-5 100 100) FPWS-5)

;; color-select: FunPaintWorldState Posn-X Posn-Y -> FunPaintWorldState
;; Purpose: Updates the world state based on the color button that is clicked
(define (color-select fpws x y)
  (cond
    [(string=? (button-check x y TOOLBAR) "red")
     (make-fp-ws (make-placed-shape (change-color (placed-shape-shape (fp-ws-current-shape fpws))
                                                  "red")
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "orange")
     (make-fp-ws (make-placed-shape (change-color (placed-shape-shape (fp-ws-current-shape fpws))
                                                  "orange")
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "yellow")
     (make-fp-ws (make-placed-shape (change-color (placed-shape-shape (fp-ws-current-shape fpws))
                                                  "yellow")
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "green")
     (make-fp-ws (make-placed-shape (change-color (placed-shape-shape (fp-ws-current-shape fpws))
                                                  "green")
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "blue")
     (make-fp-ws (make-placed-shape (change-color (placed-shape-shape (fp-ws-current-shape fpws))
                                                  "blue")
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "violet")
     (make-fp-ws (make-placed-shape (change-color (placed-shape-shape (fp-ws-current-shape fpws))
                                                  "violet")
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [else fpws]))

(check-expect (color-select FPWS-1 200 5)
              (make-fp-ws (make-placed-shape (make-tri 20 "green") (make-posn 0 0)) LOPS-1))
(check-expect (color-select FPWS-2 180 1)
              (make-fp-ws (make-placed-shape (make-rect 55 50 "yellow") (make-posn 0 0)) LOPS-2))
(check-expect (color-select FPWS-3 235 18)
              (make-fp-ws (make-placed-shape (make-circ 30 "violet") (make-posn 0 0)) LOPS-3))
(check-expect (color-select FPWS-4 210 19)
              (make-fp-ws (make-placed-shape (make-str 30 "blue") (make-posn 0 0)) LOPS-4))
(check-expect (color-select FPWS-5 100 100) FPWS-5)

;; make-size: FunPaintWorldState Posn-X Posn-Y -> FunPaintWorldState
;; Purpose: Updates the world state based on the size button that is clicked
(define (make-size fpws x y)
  (cond
    [(string=? (button-check x y TOOLBAR) "plus")
     (make-fp-ws (make-placed-shape (change-size (placed-shape-shape (fp-ws-current-shape fpws)) 5)
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [(string=? (button-check x y TOOLBAR) "minus")
     (make-fp-ws (make-placed-shape (change-size (placed-shape-shape (fp-ws-current-shape fpws)) -5)
                                    (make-posn 0 0))
                 (fp-ws-lops fpws))]
    [else fpws]))

(check-expect (make-size FPWS-1 90 19)
              (make-fp-ws (make-placed-shape (make-tri 25 "blue") (make-posn 0 0)) LOPS-1))
(check-expect (make-size FPWS-2 110 10)
              (make-fp-ws (make-placed-shape (make-rect 50 45 "violet") (make-posn 0 0)) LOPS-2))
(check-expect (make-size FPWS-3 99 13)
              (make-fp-ws (make-placed-shape (make-circ 35 "red") (make-posn 0 0)) LOPS-3))
(check-expect (make-size FPWS-4 115 2)
              (make-fp-ws (make-placed-shape (make-str 25 "green") (make-posn 0 0)) LOPS-4))
(check-expect (make-size FPWS-5 100 100) FPWS-5)

;; mouse-tracker: FunPaintWorldState MouseX MouseY MouseEvent -> FunPaintWorldState
;; Purpose: Takes in the x and y coordinates of the mouse and
;; draws the current shape at that position once the mouse is clicked
(define (click-check fpws x y event)
  (cond
    [(mouse=? event "button-down")
     (cond
       [(or (string=? (button-check x y TOOLBAR) "circle")
            (string=? (button-check x y TOOLBAR) "triangle")
            (string=? (button-check x y TOOLBAR) "rectangle")
            (string=? (button-check x y TOOLBAR) "star"))
        (shape-select fpws x y)]
       [(or (string=? (button-check x y TOOLBAR) "plus")
            (string=? (button-check x y TOOLBAR) "minus"))
        (make-size fpws x y)]
       [(or (string=? (button-check x y TOOLBAR) "red")
            (string=? (button-check x y TOOLBAR) "orange")
            (string=? (button-check x y TOOLBAR) "yellow")
            (string=? (button-check x y TOOLBAR) "green")
            (string=? (button-check x y TOOLBAR) "blue")
            (string=? (button-check x y TOOLBAR) "violet"))
        (color-select fpws x y)]
       [else 
        (make-fp-ws (make-placed-shape (placed-shape-shape (fp-ws-current-shape fpws))
                                       (make-posn x y)) 
                    (cons (make-placed-shape (placed-shape-shape (fp-ws-current-shape fpws))
                                             (make-posn x y))
                          (fp-ws-lops fpws)))])]
    [else fpws]))
   

(check-expect (click-check FPWS-1 30 15 "button-down")
              (make-fp-ws (make-placed-shape (make-tri 20 "blue") (make-posn 0 0)) LOPS-1))
(check-expect (click-check FPWS-2 300 100 "button-down")
              (make-fp-ws (make-placed-shape (make-rect 55 50 "violet") (make-posn 300 100))
                          (list
                           (make-placed-shape
                            (make-rect 55 50 "violet") (make-posn 300 100))
                           (make-placed-shape
                            (make-tri 20 "blue") (make-posn 240 350)))))
(check-expect (click-check FPWS-3 90 10 "button-down")
              (make-fp-ws (make-placed-shape (make-circ 35 "red") (make-posn 0 0)) LOPS-3))
(check-expect (click-check FPWS-4 143 14 "button-down")
              (make-fp-ws (make-placed-shape (make-str 30 "orange") (make-posn 0 0)) LOPS-4))
(check-expect (click-check FPWS-5 140 329 "button-down")
              (make-fp-ws (make-placed-shape (make-tri 20 "blue") (make-posn 140 329))
                          (list
                           (make-placed-shape
                            (make-tri 20 "blue") (make-posn 140 329))
                           (make-placed-shape
                            (make-str 30 "green") (make-posn 200 200))
                           (make-placed-shape
                            (make-circ 30 "red") (make-posn 150 360))
                           (make-placed-shape
                            (make-rect 55 50 "violet") (make-posn 156 140))
                           (make-placed-shape
                            (make-tri 20 "blue") (make-posn 240 350)))))

;; fun-paint: FunPaintWorldState -> Image
;; Purpose: Uses big bang to create a FunPaint program
(define (fun-paint fpws)
  (big-bang fpws
    [to-draw draw-scene]
    [on-mouse click-check]))

(fun-paint (make-fp-ws (make-placed-shape (make-circ 10 "red") (make-posn 0 0)) empty))