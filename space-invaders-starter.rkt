;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)
(define COUNT 0)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))
(define MISSILE (ellipse 5 15 "solid" "red"))

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position
;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1))           ;not landed, moving right 12
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left -10
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right 10

#;
(define (fn-for-invader invader)
  (... (invader-x invader)
       (invader-y invader)
       (invader-dx invader)))

;; ListOfInvaders is one of:
;; empty
;; (cons Invaders ListOfInvaders)
;; interp. represent a list of invaders
(define LI0 empty)
(define LI1 (cons I1 empty))
(define LI2 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else 
         (...(first loi)
             (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissiles is one of:
;; - empty
;; - (cond Missile ListOfMissiles)
;; interp. represent a list of missiles

(define LM0 empty)
(define LM1 (cons M1 empty))
(define LM2 (cons M1 (cons M2 empty)))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list (make-invader 20 20 1) (make-invader 40 40 1) (make-invader 60 60 1))
                      empty
                      (make-tank 100 1)))

;; =================
;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main game)
  (big-bang game                ; Game
    (on-tick   tock)            ; Game -> Game
    (to-draw   render)          ; Game -> Image
    (stop-when game-over?)      ; Game -> Boolean
    (on-key    handle-key)))    ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state, adavanced tank to left or rigth(-1,1)
;; advance the invanders to down and moving left ->  right or right -> left
;; advance the missiles to up.

(check-expect (tock G0) (make-game   empty
                                     empty
                                     (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))

(check-random (tock (make-game (list (make-invader 150 100 1)) (list (make-missile 150 300)) (make-tank 50 1)))
              (make-game (if (= (random INVADE-RATE) 99)
                             (cons (new-invader WIDTH) (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1))) 
                             (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1)))
                         (list (make-missile  150 (- 300 MISSILE-SPEED)))
                         (make-tank (+ 50 TANK-SPEED) 1)))

(check-random (tock (make-game (list (make-invader 150 100 1) (make-invader 150 200 1))
                               (list (make-missile 150 300) (make-missile 150 (+ 100 10)))
                               (make-tank 50 1)))
              (make-game (if (= (random INVADE-RATE) 99)
                             (cons (new-invader WIDTH) (list (make-invader (+ 150 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 1)))
                             (list (make-invader (+ 150 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 1)))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (make-tank (+ 50 TANK-SPEED) 1)))
          
;(define (tock game) game) ;stub
;<template used from game>

(define (tock game)
  (make-game
   (next-loinvader (if (= (random  INVADE-RATE)  99)
                       (cons (new-invader WIDTH) (game-invaders game))
                       (game-invaders game))
                   (game-missiles game))
   (next-lom (game-missiles game) (game-invaders game))
   (next-tank (game-tank game))))

;; Number -> Invader
;; create a invader with x postion between 0 and n
(check-random (new-invader 200) (make-invader (random (- 200 (/ (image-width INVADER) 2)))
                                              (/ (image-height INVADER) 2)
                                              1))

;(define (new-invader w) (make-invader 0 0 1)) ;stub

(define (new-invader w) (make-invader (random (- w (/ (image-width INVADER) 2)))
                                      (/ (image-height INVADER) 2)
                                      1)) 

;; ListOfInvaders ListOfMissile -> ListOfInvaders
;; produce the next state of each invaders in the list
;; advancing on x INVADER-X-SPEED  pixel to left if (invader-dx i) is -1 or
;; to right if (invader-dx i) is 1
;; and on y INVADER-Y-SPEED pixel to bottom
;; if hit the wall they will bounce off and continue in the other direction
;; hit right wall go to left
;; hit left wall go to right
;; if invader collision with missile, invader dissapear(removed from the list)
(check-expect (next-loinvader empty empty) empty)
(check-expect (next-loinvader (list (make-invader 20 30 1)) empty)
                (list (make-invader (+ 20 INVADER-X-SPEED) (+ 30 INVADER-Y-SPEED) 1)))

(check-expect (next-loinvader (list (make-invader 20 100 1) (make-invader 30  250 1)) empty)        
                (list (make-invader (+ 20 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1)                 ;middle
                      (make-invader (+ 30 INVADER-X-SPEED) (+ 250 INVADER-Y-SPEED) 1)))

(check-expect (next-loinvader (list (make-invader 50 200 1) (make-invader 30 (- HEIGHT 10)  1))
                                (list (make-missile 20 300)))
                (list (make-invader (+ 50 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 1)
                      (make-invader (+ 30 INVADER-X-SPEED) (+ (- HEIGHT 10) INVADER-Y-SPEED) 1)))      ;reach bottom
(check-expect (next-loinvader (list (make-invader 20 100 -1) (make-invader 30  150 -1))
                                (list (make-missile 20 300)))        
                (list (make-invader (- 20 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -1)                  ;to left
                      (make-invader (- 30 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) -1)))
(check-expect (next-loinvader (list (make-invader 20 100 1)
                                      (make-invader (- WIDTH (/ (image-width INVADER) 2)) 150 1))
                                (list (make-missile 20 300)))
                (list (make-invader (+ 20 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1)                    ;reach right wall
                      (make-invader (-  (- WIDTH (/ (image-width INVADER) 2)) INVADER-X-SPEED)
                                    (+ 150 INVADER-Y-SPEED) -1)))

(check-expect (next-loinvader (list (make-invader 20 100 1)
                                      (make-invader (+ (- WIDTH (/ (image-width INVADER) 2)) 2) 150 1))
                                (list (make-missile 30 400)))
                (list (make-invader (+ 20 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1)                    ;pass right wall
                      (make-invader (- (+ (- WIDTH (/ (image-width INVADER) 2)) 2) INVADER-X-SPEED)
                                    (+ 150 INVADER-Y-SPEED) -1)))
(check-expect (next-loinvader (list (make-invader 50 200 1)) (list (make-missile 40 200)))               ;collison missile invader
                empty)

(check-expect (next-loinvader (list (make-invader 50 200 1) (make-invader 20 100 1)) (list (make-missile 50 210)))
                (list (make-invader (+ 20 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1)))

(check-expect (next-loinvader (list (make-invader 50 200 1) (make-invader 20 100 1))  
                                (list (make-missile 50 210) (make-missile 18 102)))                      ;double collision
                empty)

;(define (next-loinvader loi) empty) ;stub
;<template took from LOI>

(define (next-loinvader loi lom)
  (cond [(empty? loi)  empty]
        [else
         (if (invader-fired? (first loi) lom)
             (next-loinvader (rest loi) lom)
             (cons (next-invader (first loi))
                   (next-loinvader (rest loi) lom)))]))

;; Invader ListOfMissile ->  Boolean
;; produce true if invader has a collison with some missile on lom (fired)
(check-expect (invader-fired? (make-invader 50 200 1) empty) false)
(check-expect (invader-fired? (make-invader 20 200 -1) (list (make-missile 20 100))) false)
(check-expect (invader-fired? (make-invader 20 200 1) (list (make-missile 20 190))) true)
(check-expect (invader-fired? (make-invader 20 100 1) (list (make-missile 20 190) (make-missile 20 95))) true)
(check-expect (invader-fired? (make-invader 20 100 1) (list (make-missile 20 190) (make-missile 20 25))) false)

;(define (invader-fired? i lom) false) ;stub

(define (invader-fired? i lom)
  (cond [(empty? lom) false]
        [else
         (if (hit-invader? (first lom) i)
             true
             (invader-fired? i (rest lom)))]))

;; Invader -> Invader
;; produce the next invader movig rigth INVADER-X-SPEED Pixel if (invader-dx i) is 1 or
;; hit left wall
;; move left INVADER-X-SPEED pixel  if (invader-dx i) is -1 or hit left wall go to right
;; and y move INVADER-Y-SPEED Pixel to bottom

(check-expect (next-invader (make-invader 10  10  1)) (make-invader (+ 10 INVADER-X-SPEED) (+  10 INVADER-Y-SPEED)  1))
(check-expect (next-invader (make-invader 50 200  1)) (make-invader (+ 50 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED)  1))
(check-expect (next-invader (make-invader 20 500 -1)) (make-invader (- 20 INVADER-X-SPEED) (+ 500 INVADER-Y-SPEED) -1))
(check-expect (next-invader (make-invader (- WIDTH (/ (image-width INVADER) 2)) 150 1))
              (make-invader (- (- WIDTH (/ (image-width INVADER) 2)) INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) -1))

;(define (next-invader i) i) ;stub
; <Template used from INVADER>

(define (next-invader i)
  (cond [ (hit-left-wall? i)
          (make-invader (+ (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED)  1)]
        [ (hit-right-wall? i)
          (make-invader (- (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED) -1)]
        [(= (invader-dx i) 1)
         (make-invader (+ (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED)  1)]
        [else
         (make-invader (- (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED) -1)]))

;; Invader -> Boolean
;; produce true is invader hit or pass the left wall (0 or < 0)
(check-expect (hit-left-wall? (make-invader 30 100 1)) false)
(check-expect (hit-left-wall? (make-invader (/ (image-width INVADER) 2) 100 -1)) true)       ; reach the wall
(check-expect (hit-left-wall? (make-invader (- (/ (image-width INVADER) 2) 3) 100 -1)) true) ; pass the wall 
                              
;(define (hit-left-wall? i) false) ;stub

(define (hit-left-wall? i)
  (<= (invader-x i) (/ (image-width INVADER) 2)))

;; Invader -> Boolean
;; produce true is invader hit or pass the right wall (WIDTH or > WIDTH)
(check-expect (hit-right-wall? (make-invader 100 150 1)) false)
(check-expect (hit-right-wall? (make-invader (- WIDTH (/ (image-width INVADER) 2)) 150 1)) true)       ; reach the wall
(check-expect (hit-right-wall? (make-invader (+ (- WIDTH (/ (image-width INVADER) 2)) 3) 150 1)) true) ; pass the wall

;(define (hit-right-wall? i) false) ;stub

(define (hit-right-wall? i)
  (>= (invader-x i) (- WIDTH (/ (image-width INVADER) 2)) ))

;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; produce a list with the next state of each missile; moving them MISSILE-SPEED pixel to top.
;; if missille hit invader the missile disappear
(check-expect (next-lom empty empty) empty)
(check-expect (next-lom (list (make-missile 10 200)) (list (make-invader 10 50 1)))
              (list (make-missile 10 (- 200 MISSILE-SPEED))))
(check-expect (next-lom (list (make-missile 10 200) (make-missile 20 100)) (list (make-invader 10 20 1)))
              (list (make-missile 10 (- 200 MISSILE-SPEED)) (make-missile 20 (- 100 MISSILE-SPEED))))
(check-expect (next-lom (list (make-missile 10 100)) (list (make-invader 10 90 1))) empty)
(check-expect (next-lom (list (make-missile 30 100) (make-missile 40 200)) (list (make-invader 30 50 1) (make-invader 40 210 1)))
              (list (make-missile 30 (- 100 MISSILE-SPEED))))
(check-expect (next-lom (list (make-missile 30 100) (make-missile 40 200)) (list (make-invader 20 100 1) (make-invader 40 210 1)))
              empty)

;(define (next-lom lom) lom) ;stub
;< template from LOM>

(define (next-lom lom loi)
  (cond [(empty? lom) empty]
        [else 
         (if (or (missile-hit-loi? (first lom) loi)
                 (missile-out? (first lom)))
             (next-lom (rest lom) loi)
             (cons (next-missile (first lom))
                   (next-lom (rest lom) loi)))]))

;; Missile -> Boolean
;; prouduce true if missile is out of the BACKGROUND(pass the top of background image)
(check-expect (missile-out? (make-missile 100 100)) false)
(check-expect (missile-out? (make-missile 100 0)) false)
(check-expect (missile-out? (make-missile 100 (- 0 (+ (/ (image-height MISSILE) 2) 2)))) true)

;(define (missile-out? m) false) ;stub

(define (missile-out? m)
  (< (+ (missile-y m) (/ (image-height MISSILE) 2)) 0)) 

;; Missile -> Missile
;; produce the next missile moving missile MISSILE-SPEED pixel to top.
(check-expect (next-missile (make-missile 10 200)) (make-missile 10 (- 200 MISSILE-SPEED)))
(check-expect (next-missile (make-missile 50 100)) (make-missile 50 (- 100 MISSILE-SPEED)))
(check-expect (next-missile (make-missile 60  10)) (make-missile 60 (-  10 MISSILE-SPEED)))
(check-expect (next-missile (make-missile 60   9)) (make-missile 60 (-   9 MISSILE-SPEED)))

;(define (next-missile m) m); stub
;< template from missile>

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Missile ListOfinvaders -> Boolean
;; produce true if missile hit to one invader on LOI
(check-expect (missile-hit-loi? (make-missile 30 100) empty) false)
(check-expect (missile-hit-loi? (make-missile 30 100) (list (make-invader 30  50 1))) false)
(check-expect (missile-hit-loi? (make-missile 30 100) (list (make-invader 30  90 1)))  true)
(check-expect (missile-hit-loi? (make-missile 30 100) (list (make-invader 40 100 1) (make-invader 50 80 1))) true)
(check-expect (missile-hit-loi? (make-missile 30 100) (list (make-invader 30 110 1) (make-invader 50 80 1))) true)
(check-expect (missile-hit-loi? (make-missile 50 100) (list (make-invader 40 100 1) (make-invader 50 80 1))) true)

;(define (missile-hit-loi? m loi) false) ;stub

(define (missile-hit-loi? m loi)
  (cond [(empty? loi) false]
        [else 
         (or (hit-invader? m (first loi))
             (missile-hit-loi?  m (rest loi)))]))

;; Missile Invader -> Boolean
;; produce true if missile hit invader
;; the distance between the two is <= HIT-RANGE
(check-expect (hit-invader? (make-missile 30  40) (make-invader 20 100 1)) false)
(check-expect (hit-invader? (make-missile 30 100) (make-invader 40 100 1)) true)
(check-expect (hit-invader? (make-missile 30 100) (make-invader 30  90 1)) true)
(check-expect (hit-invader? (make-missile 30 100) (make-invader 40 100 1)) true)
(check-expect (hit-invader? (make-missile 50 100) (make-invader 40 100 1)) true)

;(define (hit-invader? m i) false) ;stub

(define (hit-invader? m i)
  (<= 0 (distance m i) HIT-RANGE))

;; Missile Invader -> Number
;; caculate the distance between the center of missile-image (xm,ym) and invader-image(xi, yi)
(check-within (distance (make-missile 30 40) (make-invader 20 100 1))
              (sqrt (+ (sqr (- 20 30)) (sqr (- 100 40)))) 1)
(check-within (distance (make-missile 30 100) (make-invader 40 100 1))
              (sqrt (+ (sqr (- 40 30)) (sqr (- 100 100)))) 1)

;(define (distance m i) 0); stub

(define (distance m i)
  (sqrt (+ (sqr (- (invader-x i) (missile-x m)))
           (sqr (- (invader-y i) (missile-y m))))))


;; Tank -> Tank
;; move the tank TANK_SPEED per pixel to lef if (= dir-tank -1) or right if (= dir-tank 1)
(check-expect (next-tank (make-tank  50  1)) (make-tank (+  50 TANK-SPEED)  1))
(check-expect (next-tank (make-tank  50 -1)) (make-tank (-  50 TANK-SPEED) -1))
(check-expect (next-tank (make-tank  10  1)) (make-tank (+  10 TANK-SPEED)  1))
(check-expect (next-tank (make-tank  10 -1)) (make-tank (-  10 TANK-SPEED) -1))
(check-expect (next-tank (make-tank 290  1)) (make-tank (+ 290 TANK-SPEED)  1))
(check-expect (next-tank (make-tank 290 -1)) (make-tank (- 290 TANK-SPEED) -1))

;(define (next-tank t) t) ;stub

(define (next-tank t)
  (cond [ (= (tank-dir t)  1) (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))]
        [ (= (tank-dir t) -1) (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]))

;; Game -> Image
;; render the aproppiate image of missile, tank, invader on MTS
(check-expect (render (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (place-image empty-image 0 0
                           (place-image empty-image 0 0
                                        (place-image TANK (/ WIDTH 2) TANK-Y
                                                     BACKGROUND))))
(check-expect (render (make-game (list (make-invader 150 100 12)) (list (make-missile 150 300)) (make-tank 50 1)))
              (place-image INVADER 150 100
                           (place-image MISSILE 150 300
                                        (place-image TANK 50 TANK-Y BACKGROUND))))
(check-expect (render (make-game (list (make-invader 150 100 12) (make-invader 150 HEIGHT -10))
                                 (list (make-missile 150 300) (make-missile 150 (+ 100 10)))
                                 (make-tank 50 1)))
              (place-image INVADER 150 100
                           (place-image INVADER 150 HEIGHT
                                        (place-image MISSILE 150 300
                                                     (place-image MISSILE 150 110
                                                                  (place-image TANK 50 TANK-Y BACKGROUND))))))

;(define (render game) BACKGROUND) ;stub
;< template from Game>

(define (render s)
  (render-loinvader (game-invaders s)
                    (render-lomissile (game-missiles s)
                                      (render-tank (game-tank s)))))

;; ListOfInvaders Image -> Image
;; render each invader on ListOfinvader on to Img
(check-expect (render-loinvader empty BACKGROUND) BACKGROUND)
(check-expect (render-loinvader (list (make-invader 150 200 12)) BACKGROUND)
              (place-image INVADER 150 200 BACKGROUND))
(check-expect (render-loinvader (list (make-invader 150 100 1) (make-invader 150 HEIGHT -1)) BACKGROUND)
              (place-image INVADER 150 100 (place-image INVADER 150 HEIGHT BACKGROUND)))
1
;(define (render-loinvader loi img) img) ;stub
; < template from loi>

(define (render-loinvader loi img)
  (cond [(empty? loi) img]
        [else 
         (render-invader (first loi) 
                         (render-loinvader (rest loi) img))]))

;; Invader Image -> Image
;; render invader on appropriate place on  to img
(check-expect (render-invader (make-invader 150 200  1) BACKGROUND) (place-image INVADER 150 200 BACKGROUND))
(check-expect (render-invader (make-invader 100 200 -1) BACKGROUND) (place-image INVADER 100 200 BACKGROUND))

;(define (render-invader i img) img) ;stub

(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img)) 

;; ListOfMissile Image -> Image
;; render each missile on ListOfMissile on to Img
(check-expect (render-lomissile empty BACKGROUND) BACKGROUND)
(check-expect (render-lomissile (list (make-missile 150 300) (make-missile 150 (+ 100 10))) BACKGROUND)
              (place-image MISSILE 150 300 (place-image MISSILE 150 110 BACKGROUND))) 

;(define (render-lom lom img) img) ;stub
;< template from lom>

(define (render-lomissile lom img)
  (cond [(empty? lom) img]
        [else 
         (render-missile (first lom) 
                         (render-lomissile (rest lom) img))]))

;; Missile -> Image
;; render missile on apprpriate place on to img
(check-expect (render-missile (make-missile 150 300) BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-missile (make-missile 150 110) BACKGROUND) (place-image MISSILE 150 110 BACKGROUND))
 
;(define (render-missile m img) img) ;stub
;<template from missile>

(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img)) 

;; Tank -> Image
;; render the tank image on to BACKGROUND
(check-expect (render-tank (make-tank 100 TANK-Y)) (place-image TANK 100 TANK-Y BACKGROUND))
(check-expect (render-tank (make-tank  10 TANK-Y)) (place-image TANK  10 TANK-Y BACKGROUND))
(check-expect (render-tank (make-tank 200 TANK-Y)) (place-image TANK 200 TANK-Y BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub
;<template from tank>

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y BACKGROUND))

;; Game -> Boolean
;; produce true if one invader landed at the bottom
(check-expect (game-over? G2) false)
(check-expect (game-over? (make-game (list (make-invader 200 100 1) (make-invader 200 150 1)) empty (make-tank 100 1)))
              false)
(check-expect (game-over? (make-game (list (make-invader 200 100 1)
                                           (make-invader 200 (- HEIGHT (/ (image-height INVADER)2)) 1))
                                     empty (make-tank 100 1)))
              true)
(check-expect (game-over? (make-game (list (make-invader 200 100 1)
                                           (make-invader 200 (+ (- HEIGHT (/ (image-height INVADER)2)) 3) 1))
                                     (list (make-missile 20 100)) (make-tank 100 1)))
              true)

;(define (game-over? g) false) ;stub

(define (game-over? g)
  (invaders-landed? (game-invaders g)))

;;ListOfInvaders -> Boolean
;; produce true if one invader in lit touch/landed the bottom
(check-expect (invaders-landed? empty) false)
(check-expect (invaders-landed? (list (make-invader 100 100 1) (make-invader 200 200 -1))) false)
(check-expect (invaders-landed? (list (make-invader 100 120 1)
                                      (make-invader 20 (- HEIGHT (/ (image-height INVADER)2)) 1)))
              true)
(check-expect (invaders-landed? (list (make-invader 100 120 1)
                                      (make-invader 20 (+ (- HEIGHT (/ (image-height INVADER)2)) 2) 1)))
              true)

;(define (invaders-landed? loi) false) ;stub

(define (invaders-landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (landed? (first loi))
             true
             (invaders-landed? (rest loi)))]))

;; Invader -> Boolean
;; produce true if invader landed
(check-expect (landed? (make-invader 10 50 1)) false)
(check-expect (landed? (make-invader 20 (- HEIGHT (/ (image-height INVADER)2)) 1))       true)
(check-expect (landed? (make-invader 20 (+ (- HEIGHT (/ (image-height INVADER)2)) 2) 1)) true)

;(define (landed? i) false) ;stub

(define (landed? i)
  (>= (+ (invader-y i) (/ (image-height INVADER) 2)) HEIGHT))

;; Game ke -> Game
;; if " " is pressed the missile is fire, if left-arrow key is pressed the tankm move to the left
;; if right arrow key is pressed the tank move to the right
(check-expect (handle-key (make-game empty empty (make-tank 100  1))  "left")
              (make-game empty empty (make-tank 100 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 100 -1)) "right")
              (make-game empty empty (make-tank 100 1)))
(check-expect (handle-key (make-game (list (make-invader 20 50 1) (make-invader 50 100 1))
                                     empty
                                     (make-tank 20 1)) " ")
              (make-game (list (make-invader 20 50 1) (make-invader 50 100 1))
                         (list (make-missile 20 (- HEIGHT (image-height TANK))))
                         (make-tank 20 1)))


(define (handle-key game ke)
  (cond [(key=? ke " ") (make-game (game-invaders game) (shot-missile (game-missiles game) (game-tank game))
                                   (game-tank game))]
        [(key=? ke "left")  (make-game  (game-invaders game) (game-missiles game) (move-tank-l (game-tank game)))]
        [(key=? ke "right") (make-game  (game-invaders game) (game-missiles game) (move-tank-r (game-tank game)))]
        [else game]))

;; ListOfMissiles tank -> ListOfMissiles
;; put a new missile on lom
(check-expect (shot-missile empty (make-tank 100 1)) (list (make-missile 100 (- HEIGHT (image-height TANK)))))
(check-expect (shot-missile (list (make-missile 80 100)) (make-tank 150 -1))
              (list (make-missile 150 (- HEIGHT (image-height TANK))) (make-missile 80 100))) 

;(define (shot-missile lom t) lom) ;stub
;<template from lom>

(define (shot-missile lom t)
  (cond [(empty? lom) (cons (make-missile (tank-x t) (- HEIGHT (image-height TANK))) empty)]
        [else 
         (cons (make-missile (tank-x t) (- HEIGHT (image-height TANK)))
               lom)]))

;; Tank -> Tank
;; change dir of tank to left 
(check-expect (move-tank-l (make-tank 100 1)) (make-tank 100 -1))

;(define (move-tank-l t) t) ;stub

(define (move-tank-l t)
  (make-tank (tank-x t) -1))

;; Tank -> Tank
;; change dir of tank to right
(check-expect (move-tank-r (make-tank 100 -1)) (make-tank 100 1))

;(define (move-tank-r t) t) ;stub

(define (move-tank-r t)
  (make-tank (tank-x t) 1))