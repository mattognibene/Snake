;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Developed December 2017 by Matthew Ognibene
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)

;Image Constants
(define GAME_WIDTH 600)
(define GAME_HEIGHT 600)
(define WORLD (rectangle GAME_WIDTH GAME_HEIGHT "solid" "black"))
(define S-WIDTH 20)
(define S-HEIGHT 20)
(define HEAD (rectangle S-WIDTH S-HEIGHT "solid" "red"))
(define BODY (rectangle S-WIDTH S-HEIGHT "solid" "lime green"))
(define APPLE (rectangle S-WIDTH S-HEIGHT "solid" "red"))
(define FONT-SIZE 30)
(define FONT-COLOR "white")


;A GameState is one of the following:
; - 'new
; - Game
; - GameOver

;A Game is a (make-game Snake Vel Posn)
(define-struct game[snake v apple])

;A Snake is (non-empty [List-of Posn] where the last of the list is always the
;position of the head

;A Vel is (make-vel Number Number]
(define-struct vel[dx dy])

;A GameOver is a (make-gameover Snake)
;Signifies the gmae has ended
(define-struct gameover[end-state])

;Game Constants
(define VEL 10)
(define START (make-game (list (make-posn (/ GAME_WIDTH 2) (/ GAME_HEIGHT 2))
                               (make-posn (- (/ GAME_WIDTH 2) VEL) (/ GAME_HEIGHT 2)))
                         (make-vel VEL 0)
                         (make-posn (random GAME_WIDTH) (random GAME_HEIGHT))))

;main
;GameState -> GameState
;big bang function for the game
(define (main gs)
  (big-bang gs
            [on-tick update]
            [to-draw draw]
            [on-key key-listener]))

;Top Big Bang Functions
;GameState->GameState
(define (update gs)
  (cond
    [(symbol? gs)gs]
    [(game? gs) (update/game gs)]
    [(gameover? gs) gs]))

;GameState->Image
(define (draw gs)
  (cond
    [(symbol? gs)(draw-new gs)]
    [(game? gs) (draw-game gs)]
    [(gameover? gs) (draw-gameover gs)]))

;GameState Key -> GameState
(define (key-listener gs k)
  (cond
    [(symbol? gs) (start-game gs k)]
    [(game? gs) (key-listener/game gs k)]
    [(gameover? gs) (start-game gs k)]))
;BigBang Functions for Game GameState
;Game->GameState
(define (update/game g)
  (local( 
         (define new-game (update/apple(update/snake g))));
    (if (snake-collided? new-game)
        (make-gameover new-game)
        new-game)))

;Game -> Game
;Updates the snake by:
; - moving the head one pixel in the given direction
; - moving each part in the direction of the next part by one pixel
; - fixing if it has gone out of bounds
(define (update/snake g)
  (local(
         ;Snake->Snake
         ;updates the snakes position
         (define (move-snake s)
           (cond
             [(empty? (rest s)) (cons (make-posn (+ (vel-dx(game-v g)) (posn-x (first s)))
                                                 (+ (vel-dy(game-v g)) (posn-y (first s))))
                                      '())]
             [(cons? s)(cons (make-posn (posn-x (second s)) (posn-y (second s)))
                             (move-snake (rest s)))])))
    (make-game (fix-oob(move-snake (game-snake g)))
               (game-v g)
               (game-apple g))))

;Game->Game
;Adds a Part to the snake and creates a new apple if the head is on the current apple
(define (update/apple g)
  (if (collision? (game-apple g) (last (game-snake g)))
      (make-game (add-part(game-snake g) (game-v g))
                 (game-v g)
                 (make-posn (random GAME_WIDTH) (random GAME_HEIGHT)))
      g))

;Posn Posn -> Boolean
;checks to see if the snake has crashed into itself
(define (collision? a b)
  (and(<(abs(- (posn-x a)
               (posn-x b)))
        10)
      (<(abs(- (posn-y a)
               (posn-y b)))
        10)))

;Game -> Boolean
(define (snake-collided? g)
  (cond
    [(empty? (rest (game-snake g))) #false]
    [(cons? (game-snake g)) (if (collision? (last (game-snake g)) (first (game-snake g)))
                                #true
                                (snake-collided? (make-game (rest (game-snake g))
                                                            (game-v g)
                                                            (game-apple g))))]))

;Snake Vel -> Snake
;adds a part to the tail of the snake
(define (add-part s v)
  (cond
    [(=(posn-x (first s)) (posn-x (second s)))
     (cons (make-posn (- (posn-x (first s)) (vel-dx v)) (posn-y (first s)))
           s)]
    [(=(posn-y (first s)) (posn-y (second s)))
     (cons (make-posn(- (posn-y (first s)) (vel-dy v)) (posn-y (first s)))
           s)]))

;Snake->Snake
;takes into account the snake going out of bound
(define (fix-oob s)
  (local(
         ;Posn->Posn
         (define (fix-oob/help p)
           (cond
             [(< (posn-x p)0) (make-posn GAME_WIDTH (posn-y p))]
             [(> (posn-x p) GAME_WIDTH) (make-posn 0 (posn-y p))]
             [(< (posn-y p)0) (make-posn (posn-x p) GAME_HEIGHT)]
             [(> (posn-y p) GAME_HEIGHT) (make-posn (posn-x p) 0)]
             [else p])))
    (map fix-oob/help s)))
#;(check-expect (fix-oob (list (make-posn -1 3) (make-posn 3 -3) (make-posn 800 3) (make-posn 3 800)))
                (list (make-posn GAME_WIDTH 3) (make-posn 3 GAME_HEIGHT)
                      (make-posn 0 3) (make-posn 3 0)))

;Game->Image
(define (draw-game g)
  (place-image APPLE
               (posn-x (game-apple g))
               (posn-y (game-apple g))
               (draw-snake (game-snake g))))

;Snake -> Image
(define (draw-snake s)
  (foldr (lambda (x img) (place-image BODY (posn-x x) (posn-y x) img)) WORLD s))

;Game KeyEvent ->Game
(define (key-listener/game g key)
  (cond
    [(string=? key "w")
     (make-game (game-snake g) (make-vel 0 (* -1 VEL)) (game-apple g))]
    [(string=? key "a")
     (make-game (game-snake g) (make-vel (* -1 VEL) 0)(game-apple g))]
    [(string=? key "s")
     (make-game (game-snake g) (make-vel 0 VEL)(game-apple g))]
    [(string=? key "d")
     (make-game (game-snake g) (make-vel VEL 0) (game-apple g))]
    [else g]))

;Gameover Bigbang functions

;Gameover -> Image
(define (draw-gameover go)
  (place-image (above (text "GAME OVER" FONT-SIZE FONT-COLOR)
                      (text (string-append "SCORE: "
                                           (number->string
                                            (length(game-snake(gameover-end-state go)))))
                            FONT-SIZE FONT-COLOR)
                      (text "PRESS ANY KEY TO START NEW GAME" FONT-SIZE FONT-COLOR))
               (/ GAME_WIDTH 2)
               (* 3 FONT-SIZE)
               (draw-snake (game-snake(gameover-end-state go)))))

;GameState Key -> GameState
(define (start-game gs key)
  START)

;New BigBang functions
;New -> Image
(define (draw-new n)
  (place-image (above(text "SNAKE!" FONT-SIZE FONT-COLOR)
                     (text "PRESS ANY KEY TO START" FONT-SIZE FONT-COLOR))
               (/ GAME_WIDTH 2)
               FONT-SIZE
               (draw-snake (game-snake START))))


; BEGIN GAME ;
(main 'new)