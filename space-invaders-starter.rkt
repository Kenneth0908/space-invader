;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1)
(define TANK-SPEED 5)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

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
(define TANK-HEIGHT (* TANK-HEIGHT/2 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MISSILE-Y-DEFAULT (- HEIGHT TANK-HEIGHT))


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

(define I1 (make-invader 150 0 -1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 11)) ;> landed, moving right
(define I4 (make-invader 0 0 3))    

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 100 350))

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; Functions

;; GameState -> GameState
;; Run space invaders, starting with initial game state gs
;; Start game with (main (make-game empty empty T0))
;; <no tests for main function>

(define (main gs)
  (big-bang gs
    (on-tick     next-gs)
    (to-draw     render-gs)
    (on-key      key-listener)
    (stop-when   invaders-landed?)
    ))

;; GameState -> GameState
;; advance the game as time passes
;; check-expect might fail due to the random invader
(check-expect (next-gs (make-game empty empty empty))
              (make-game empty empty empty))
(check-expect (next-gs (make-game (list (make-invader 10 10 12)) empty empty))
              (make-game (list (make-invader (+ 10 (* INVADER-X-SPEED 12)) (+ 10 (* INVADER-Y-SPEED 12)) 12)) empty empty))
(check-expect (next-gs (make-game empty (list (make-missile 150 300)) empty))
              (make-game empty (list (make-missile 150 (- 300 MISSILE-SPEED))) empty))
(check-expect (next-gs (make-game empty empty (make-tank 50 1)))                                                                        
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (next-gs (make-game (list (make-invader 10 10 12)) (list (make-missile 150 300)) (make-tank 50 1)))                  
              (make-game
               (list (make-invader (+ 10 (* INVADER-X-SPEED 12))
                                   (+ 10 (* INVADER-Y-SPEED 12))
                                   12
                                   ))
               (list (make-missile 150 (- 300 MISSILE-SPEED)))
               (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (next-gs (make-game (list (make-invader 10 10 12)) (list (make-missile 11.5 21.5)) (make-tank 50 1)))                  
              (make-game
               (list (make-invader
                      (+ 10 (* 12 INVADER-X-SPEED))
                      (+ 10 (* 12 INVADER-Y-SPEED))
                      12))
               (list (make-missile
                      11.5
                      (- 21.5 MISSILE-SPEED)))
               (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (next-gs
               (make-game
                (list (make-invader 11.5 11.5 12))
                (list (make-missile 11.5 11.5))
                (make-tank 50 1)))
              (make-game
               (list empty)
               (list (make-missile 11.5 1.5))
               (make-tank (+ 50 TANK-SPEED) 1)))

; (define (next-gs gs) gs) ;stub
(define (next-gs gs)
  (cond [(new-invader? gs)
         (make-game (move-invaders (cons (make-invader (random 300) 0 (gen-speed gs)) (game-invaders gs)) gs)
                    (move-missiles (game-missiles gs))
                    (move-tank (game-tank gs)))
         ]
        [else 
         (make-game (move-invaders (game-invaders gs) gs)
                    (move-missiles (game-missiles gs))
                    (move-tank (game-tank gs)))]))

;; GameState -> Boolean
;; generate new invaders in a 3% probability
;(define (new-invader? gs) true) ;stub
(define (new-invader? gs)
  (>= 2 (random 100)))


;; GameState -> Number
;; Random generate the direction of the invader
;(define (gen-speed gs) -1) ;stub
(define (gen-speed gs)
  (if (> 5 (random 10))
      (* -1 (+ 1 (random 5)))
      (+ 1 (random 5))))


;; GameState -> Boolean
;; produce true of the invader is hit by missile
(check-expect (move-tank (make-tank 10 1)) (make-tank (+ 10 TANK-SPEED) 1))
(check-expect (move-tank (make-tank 10 -1)) (make-tank (- 10 TANK-SPEED) -1))
(check-expect (move-tank (make-tank 300 1)) (make-tank 300 1))

(define (move-tank t)
  [cond [(empty? t) empty]
        [else
         (make-tank
          (if(< 0 (+ (tank-x t) (* TANK-SPEED (tank-dir t)))  WIDTH) 
             (+ (tank-x t) (* TANK-SPEED (tank-dir t)))
             (tank-x t))
          (tank-dir t))]])


;; ListOfInvaders GameState -> ListOfInvaders
;; take a list of invaders to output a list of invaders with updated coordinates 
(check-expect (move-invaders empty G1) empty)
(check-expect (move-invaders (cons (make-invader 150 150 10) empty) G1)
              (cons (make-invader
                     (+ 150 (* INVADER-X-SPEED 10))
                     (+ 150 (* INVADER-Y-SPEED 10)) 10) empty))
(check-expect (move-invaders (cons (make-invader 180 180 -10) empty) G1)
              (cons (make-invader
                     (+ 180 (* INVADER-X-SPEED -10))
                     (+ 180 (* INVADER-Y-SPEED 10)) -10) empty))
(check-expect (move-invaders (cons (make-invader 150 150 10) (cons (make-invader 180 180 -10) empty)) G1)
              (cons (make-invader
                     (+ 150 (* INVADER-X-SPEED 10))
                     (+ 150 (* INVADER-Y-SPEED 10)) 10) (cons (make-invader
                                                               (+ 180 (* INVADER-X-SPEED -10))
                                                               (+ 180 (* INVADER-Y-SPEED 10))
                                                               -10) empty)))

; (define (move-invaders loi gs) loi) ;stub
(define (move-invaders loi gs)
  (cond [(empty? loi) empty]
        [(empty? (first loi)) (move-invaders (rest loi) gs)]
        [else
         (if (and (empty? (first loi)) (< HEIGHT (invader-y (first loi))))
             (move-invaders (rest loi) gs)
             (cons (move-invader (first loi) gs) (move-invaders (rest loi) gs)))]))


;; Invader GameState -> Invader
;; output the new coordinate the invader given the speed
;;   return empty if invader hit by missile 
(check-expect (move-invader empty G1) empty)
(check-expect (move-invader (make-invader 100 100 10) G1)
              (make-invader 110 110 10))
(check-expect (move-invader (make-invader 100 100 -10) G1)
              (make-invader 90 110 -10))
(check-expect (move-invader (make-invader 100 100 10) (make-game (list (make-invader 100 100 10))
                                                                 (list (make-missile 100 100))
                                                                 T1))
              empty)


;(define (move-invader i gs) i);
(define (move-invader i gs)
  (cond [(empty? i) empty]
        [(missiles-hit-invader? i (game-missiles gs)) empty]
        [else
         (if (<= 0 (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) WIDTH)
             (make-invader
              (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
              (+ (invader-y i) (* (abs(invader-dx i)) INVADER-Y-SPEED))
              (invader-dx i))
             (if (> 0 (invader-dx i))
                 (make-invader
                  (- WIDTH (modulo (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) WIDTH))
                  (+ (invader-y i) (* (invader-dx i) INVADER-Y-SPEED) -1)
                  (* -1 (invader-dx i)))
                 (make-invader
                  (- WIDTH (modulo (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED)) WIDTH))
                  (+ (invader-y i) (* (invader-dx i) INVADER-Y-SPEED))
                  (* -1 (invader-dx i)))))]))


;; Invader ListOfMissiles -> Boolean 
(define (missiles-hit-invader? i lom)
  (cond [(or (empty? lom) (empty? i)) false]
        [else (if(missile-hit-invader? i (first lom))
                 true
                 (missiles-hit-invader? i (rest lom)))]))

;; Invader Missile -> Boolean
;; return true when x y coordinate of invader and missile matches within the HIT-RANGE
(define (missile-hit-invader? i m)
  (and (<= (- (missile-x m) HIT-RANGE) (invader-x i) (+ (missile-x m) HIT-RANGE))
       (<= (- (missile-y m) HIT-RANGE) (invader-y i) (+ (missile-y m) HIT-RANGE))
       ))


;; ListOfMissiles -> ListOfMissiles
;; take a list of missile coordinations to output a list of missile coordinations
; (define (move-missiles m) m) ;stub
(check-expect (move-missiles (cons (make-missile 150 -10) empty))
              empty)
(check-expect (move-missiles (cons (make-missile 150 300) empty)) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
(check-expect (move-missiles (cons (make-missile 300 300) (cons (make-missile 150 300) empty)))
              (cons (make-missile 300 (- 300 MISSILE-SPEED)) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty)))


(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (> 0 (missile-y (first lom)))
             (move-missiles (rest lom))
             (cons (move-missile (first lom)) (move-missiles (rest lom))))]))


;; Missile -> Missile
;; take a missile and output an updated missile coordinate
(check-expect (move-missile empty) empty)
(check-expect (move-missile (make-missile 150 150)) (make-missile 150 (- 150 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 150 0)) (make-missile 150 -10))

(define (move-missile m)
  (cond [(empty? m) empty]
        [else
         (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED))]))


;; GameState -> Image
;; place the tank, missiles, invaders
(check-expect (render-gs (make-game (list I1) (list M1) T1))
              (place-image
               TANK
               (tank-x T1)
               (- HEIGHT TANK-HEIGHT/2)
               (place-image
                MISSILE
                (missile-x M1)
                (missile-y M1)
                (place-image
                 INVADER
                 (invader-x I1)
                 (invader-y I1)
                 BACKGROUND))))
(check-expect (render-gs (make-game (list I1 I3) (list M1 M4) T1))
              (place-image
               TANK
               (tank-x T1)
               (- HEIGHT TANK-HEIGHT/2)
               (place-image
                MISSILE
                (missile-x M1)
                (missile-y M1)
                (place-image
                 MISSILE
                 (missile-x M4)
                 (missile-y M4)
                 (place-image
                  INVADER
                  (invader-x I1)
                  (invader-y I1)
                  (place-image
                   INVADER
                   (invader-x I3)
                   (invader-y I3)
                   BACKGROUND))))))
(check-expect (render-gs (make-game empty empty T1))
              (place-image
               TANK
               (tank-x T1)
               (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))
                

; (define (render-gs gs) BACKGROUND) ;stub
(define (render-gs gs)
  (render-invaders (game-invaders gs)
                   (render-missiles (game-missiles gs) (place-image
                                                        TANK
                                                        (tank-x (game-tank gs))
                                                        (- HEIGHT TANK-HEIGHT/2)
                                                        BACKGROUND))))
   


;; Missiles -> Image
;; place list of missiles in correct spot
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1) BACKGROUND)
              (place-image
               MISSILE
               (missile-x M1)
               (missile-y M1)
               BACKGROUND))
(check-expect (render-missiles (list M1 M4) BACKGROUND)
              (place-image
               MISSILE
               (missile-x M1)
               (missile-y M1)
               (place-image
                MISSILE
                (missile-x M4)
                (missile-y M4)
                BACKGROUND)))


(define (render-missiles m bg)
  (cond [(empty? m) bg]
        [else
         (place-image
          MISSILE
          (missile-x (first m))
          (missile-y (first m))
          (render-missiles (rest m) bg))]))

          

;(define (render-invaders i) BACKGROUND) ;stub
;; place list of invaders in correct spot
(define (render-invaders i bg) 
  (cond [(empty? i) bg]
        [else
         (if(empty? (first i))
            (render-invaders (rest i) bg)
            (place-image
             INVADER
             (invader-x (first i))
             (invader-y (first i))
             (render-invaders (rest i) bg)))]))


;; GameState KeyEvent -> GameState
;; change the tank direction when "left"/"right" arrow is pressed
(check-expect (key-listener (make-game empty empty T1) "left")
              (make-game empty empty T2))
(check-expect (key-listener (make-game empty empty T2) "right")
              (make-game empty empty T1))
(check-expect (key-listener (make-game empty empty T1) " ")
              (make-game empty (list (make-missile (tank-x T1) MISSILE-Y-DEFAULT)) T1))
(check-expect (key-listener G1 "up")
              G1)

; (define (key-listener gs ke) gs) ;stub
(define (key-listener gs ke)
  (cond [(key=? ke " ")
         (make-game
          (game-invaders gs)
          (cons (make-missile (tank-x (game-tank gs)) MISSILE-Y-DEFAULT) (game-missiles gs))
          (game-tank gs))]
        [(key=? ke "left")
         (make-game
          (game-invaders gs)
          (game-missiles gs)
          (make-tank (tank-x (game-tank gs)) -1))
         ]
        [(key=? ke "right")
         (make-game
          (game-invaders gs)
          (game-missiles gs)
          (make-tank (tank-x (game-tank gs)) 1))]
        [(key=? ke "r")
         (make-game
          (list I1)
          empty
          T1
          )]
        [else gs]))


;; GameState -> Boolean
;; output true when x-coord of invader hits the bottom (HEIGHT)
(check-expect (invaders-landed? G1) false)
(check-expect (invaders-landed? (make-game (list (make-invader 100 (+ HEIGHT 1) 10)) empty T1)) true)
 

;; produce true when any of the invader has landed
; (define (invaders-landed? gs) true) ;stub
(define (invaders-landed? gs)
  (cond [(empty? (game-invaders gs)) false]
        [else
         (if(invader-landed? (first (game-invaders gs)))
            true
            (invaders-landed? (make-game (rest (game-invaders gs)) (game-missiles gs) (game-tank gs) )))
         ]))

;; Invader -> Boolean
;; produce true when the given invader has landed
(define (invader-landed? i)
  (cond [(empty? i) false]
        [else
         (> (invader-y i) HEIGHT)]))

;; Initialize game with (main (make-game (list I1) (list M1) T1))
;(main (make-game empty empty T1))