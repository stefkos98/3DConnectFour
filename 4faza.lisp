;;;                                                    Kreiranje igre
(defun initialization-game ()
    (let* ((n (table-size ))
            (mode (game-mode ))
            (XO 
                (cond ((equal mode 1) 1)
                (t (first-player))))
           (state (initial (* n n) n))
           (player1  (cond ((equal mode 1) (player-name)) ((equal XO 1 ) (player-name)) (t "Robot")))
           (player2  (cond  ((equal mode 1) (player-name)) ((equal XO 2 ) (player-name)) (t "Robot")))
        )
       (cons state (list XO 'x mode n player1 player2 0)))   
  )

;;;                                                   Ucitavanje i Odigravanje poteza
(defun make-move(state xo k name)(let* ((result (format t"~% Player: ~a ~%" name))
                                            (move (read-move k))
                                           )
                                (cond ((validp state move k) (new-move state xo move))
                                      (t (format t "~% Potez nije validan~%")(make-move state xo k name)))))

;(defun read-move(k)(format t "Unesite potez (0-~X) HEX:" (1- k))
; (parse-integer (write-to-string (read)) :radix 16))

;;;                                                   Unos poteza i provera da li je unet broj
(defun read-move(k)(format t "Unesite potez (0-~X) HEX:" (1- k))
  (let* ((move (write-to-string (read))))
    (cond ((regular (coerce move 'list)) (parse-integer move :radix 16))
          (t (read-move k))))
  )

;;;                                                   Provera da li je uneti potez hex broj
(defun regular(clist)
  (cond 
        ((null clist) t)
        ((or (and (char-not-lessp (car clist) #\0) (char-not-greaterp (car clist) #\9)) 
             (and (char-not-lessp (car clist) #\a) (char-not-greaterp (car clist) #\f))) (regular (cdr clist)))
         (t '())
   ))
(defun new-move(state xo move)(cond ((zerop move) (cons (insert (car state) xo) (cdr state)))
                                         (t (cons (car state) (new-move (cdr state) xo (1- move))))))

(defun insert(stick xo)(cond ((equal (car stick) "-")(cons xo (cdr stick)))
                                 (t (cons (car stick) (insert (cdr stick) xo)))))

;;;                                                   Provera da li je potez validan           
(defun validp (state move k) ;k=n*n 0<=move<=k-1
  (cond 
        ((not (numberp move))'())
        ((or (< move 0) (> move (1- k))) '())
        (t (not (stick-fullp (nth move state))))
        ))
;(validp '((x 0 "-") (x o x) (x o "-")) 2 3)

;;;                                                   Provera da li je kraj igre
(defun final-statep (state)
  (cond ((null state) t)
        ((stick-fullp (car state)) (final-statep (cdr state)))
        (t '())
        ))
;(final-statep '((x o x o) (x o x o) (x o x "-")))

;;;                                                   Provera da li je stapic pun
(defun stick-fullp (stick) ; da li stick ima bar jedno "-" i ako ima vraca null
  (cond ((null stick) t)
        ((equal (car stick) "-") '())
        (t (stick-fullp (cdr stick)))
        ))
;(stick-fullp '(x o x x o o "-"))

;;;                                                   Inicijalizacija stapica
(defun stick (n)
  (cond ((= n 0) '())
        (t (cons "-" (stick (1- n))  ))
        ))

(defun initial(k n)
  (cond ((= k 0) '())
        (t (cons (stick n) (initial (1- k) n)))
        ))

;;;                                                   Unos imena igraca
(defun player-name()
        (format t "Unesite ime igraca:")
  (read))

;;;                                                   Inicijalizacija table
(defun table-size ()
    (format t "Unesi velicinu table (4 ili 6): ")
    (let*
        ((size (read )))
        (cond ((not (numberp size)) (table-size))
              ((or (equal size 4) (equal size 6)) size)
              (t table-size))
        ))
;;;                                                   Ko igra prvi
(defun first-player ()
    (format t "Ako prvi igra covek unesi 1, ako prvi igra kompjuter unesi 2 : ")
    (let*
        ((player (read )))
     (cond ((not (numberp player)) (first-player))
           ((or (equal player 1) (equal player 2)) player)
           (t (first-player)))
        ))

;;;                                                   Izbor moda igre
(defun game-mode ()
    (format t "Unesi 1 za igru Covek protiv Coveka, 0 za Covek protiv Kompjutera: ")
      (let*
        ((mode (read )))
     (cond ((not (numberp mode)) (game-mode))
           ((or (equal mode 1) (equal mode 0)) mode)
           (t (game-mode)))
        ))

;;;                                                   Stampanje table
(defun print-state (state k m n)
  (cond ((zerop m) (format t "~%")(print-table (reverse-sticks state 0 n) k 0 0 ))
        ((< (- k m) 16) (format t "~X  " (- k m))(print-state state k (1- m) n))
        (t (format t "~X " (- k m))(print-state state k (1- m) n))))

(defun print-state-end (state k m)
  (cond ((zerop m) (format t "~%")'())
        ((< (- k m) 16) (format t "~X  " (- k m))(print-state-end state k (1- m)))
        (t (format t "~X " (- k m))(print-state-end state k (1- m)))
        ))

(defun print-table(state k i j)
  (cond ((equal i (1- (* 2 (truncate (sqrt k))))) (print-state-end state k k))
        ((equal j k)(format t "~%")(print-table state k (1+ i) 0))
        ;((> j 15) (format t "~a  " (nth i (nth j state)))(print-table state k i (1+ j)))
        (t (format t "~a  " (nth i (nth j state)))(print-table state k i (1+ j)))
        ))

(defun insert-stick (stick br n)  ;dodaje stapicu sa leve strane n-1-(br mod n) praznih polja, a sa desne dodaje (br mod n)
  (append (empty-stick (- (1- n) (mod br n))) (append (reverse stick) (empty-stick (mod br n)))) )

(defun empty-stick (l)
  (cond ((= l 0) '())
        (t (cons " " (empty-stick (1- l))))
        ))

(defun reverse-sticks (state br n)
  (if (null state) '() (cons (insert-stick (car state) br n) (reverse-sticks (cdr state) (1+ br) n))))
;(reverse-sticks '((x o x o) (x o x o) (x o x "-")) 0 4)    

;;;                                                   Pocetak rada igre

(defun main(game)
    (let*
        (
            (state (car game))
            (n (nth 4 game))
            (k (* n n))
            (player1 (nth 5 game))
            (player2 (nth 6 game))
            (printed (print-state state k k n))
            (points (winner state player1 player2 n k nil))
            (xo (nth 2 game))
            (num (nth 7 game))
            (newstate (cond ((= (nth 3 game) 1) (make-move state xo k (if  (equalp xo 'x) player1 player2)))
                            ((or (and (equalp xo 'x) (equalp player1 "Robot")) (and (equalp xo 'o) (equalp player2 "Robot"))) (car (min-max state n (depth state n num) (list '() -9999) (list '() 9999) 1 xo xo)))
                            (t (make-move state xo k (if  (equalp xo 'x) player1 player2)))))
            (test (final-statep newstate))
        )
            (cond
            ( test (format t "KRAJ IGRE ~%")(let* ( (stanje (print-state newstate k k n)) (win (winner newstate player1 player2 n k t)))))
            ((equalp xo 'x) (main (cons newstate (list (if (equal (nth 1 game) 1) 2 1) 'o (nth 3 game) n player1 player2 (1+ num)))))
            ((equalp xo 'o) (main (cons newstate (list (if (equal (nth 1 game) 1) 2 1) 'x (nth 3 game) n player1 player2 (1+ num)))))
        )
    )
  )



;;;                                                      II FAZA

;;;                                                      Formiranje liste svih mogucih stanja u kocki

;(defun new-move(state xo move)    ima gore vec
;  (cond ((zerop move) (cons (insert (car state) xo) (cdr state)))
;        (t (cons (car state) (new-move (cdr state) xo (1- move))))))

(defun generate-all-states (state xo k move) ;move krece od 0
  (cond ((= move k) '())
        ((validp state move k) (cons (new-move state xo move) (generate-all-states state xo k (1+ move))))
        (t (generate-all-states state xo k (1+ move)))
        ))
;(generate-all-states '(("-" "-" "-" "-")(x o "-" "-")(x o x o)(o o "-" "-")) 'x 4 0)

(defun generate-all-moves (state xo k move) ;move krece od 0, 
  (cond ((= move k) '())
        ((validp state move k) (cons move (generate-all-moves state xo k (1+ move))))
        (t (generate-all-moves state xo k (1+ move)))
        ))
;(generate-all-moves '(("-" "-" "-" "-")(x o "-" "-")(x o x o)(o o "-" "-")) 'x 4 0)

;;;                                                      Odredjivanje pobednika u igri

(defun winner (state player1 player2 n k end)
  (let* (
         (x (+ (horizontal state 'x n 0) (vertical state 'x k 0) (diagonal-2d state 'x n k) (diagonal-3d state 'x n)))
         (o (+ (horizontal state 'o n 0) (vertical state 'o k 0) (diagonal-2d state 'o n k) (diagonal-3d state 'o n)))
         )
    (cond (end (cond ((> x o) (format t "  ~a(X): ~a  ~a(O): ~a ~% ~a je pobedio!" player1 x player2 o player1))
                      ((< x o) (format t " ~a(X): ~a  ~a(O): ~a ~% ~a je pobedio!" player1 x player2 o player2))
                      (t (format t " X: ~a  O: ~a ~% Nereseno!" x o)))
               )
          (t  (format t " ~a(X): ~a  ~a(O): ~a ~%" player1 x player2 o)))))

;;;                                                      Pomocna funkcija kojom se iz prosledjene liste broji 4 uzastopna pojavljivanja iksa ili oksa

(defun max-4 (l xo a b c) ;na pocetku su a, b i c '() 
  (cond ((null l) 0)
        ((not(equal xo (car l))) (max-4 (cdr l) xo b c '()))
        ((and a b c) (1+ (max-4 (cdr l) xo a b c)))        
        (t (max-4 (cdr l) xo b c t))
        ))

;;;                                                      Odredjivanje poena po vertikali

(defun vertical (state xo k m) ;m=0 na pocetku
  (cond ((equal m k) 0)
        (t (+ (max-4 (nth m state) xo '() '() '()) (vertical state xo k (1+ m))))
        ))
;(vertical '((x o x x x x)(o o o o o o)(x o x o x o)(o o x x x x)(x x x x x x)(x x x x x x)(o o o o o o)(x x x x o o)(o x x x x o)(o o o o o o)("-" "-" "-" "-" "-" "-")(x x x x x x)(o o o o o o)) 'x 13 0)

;;;                                                      Odredjivanje poena po horizontali

(defun horizontal (state xo n m) ;m=0 na pocetku, kaze koju matricu gledamo, krece od nulte, do n-1. (m-tu matricu cine svi m-ti elementi u stapicima)
  (cond ((equal m n) 0)
        (t (+ (vrste-kolone state xo n m 0) (horizontal state xo n (1+ m))))
        ))

(defun vrsta (state n vr br m) ;n*vr+br,  br=0 na pocetku
  (cond ((equal n br) '()) 
        (t (cons (nth m (nth (+ (* vr n) br) state)) (vrsta state n vr (1+ br) m)))
        ))

(defun vrste-kolone (state xo n m br) ;br=0 na pocetku
  (cond ((equal br n) 0)
        (t (+ (max-4(vrsta state n br 0 m) xo '() '() '()) (max-4(kolona state n br 0 m) xo '() '() '()) (vrste-kolone state xo n m (1+ br))))
        ))
;(vrste-kolone '((x x x o)(x o x o)(x o x o)(x o x o)(x x x x)(x x x x)(x o x o)(x x x x)(x x x x)(x o x o)(x "-" x "-")(x x x x)(x o x o)(x x x o)(x x x o)(x o x o)) 'x 4 0 0)

(defun kolona (state n kol br m) ;kol+br*n,  br=0 na pocetku
  (cond ((equal n br) '()) 
        (t (cons (nth m (nth (+ (* br n) kol) state)) (kolona state n kol (1+ br) m)))
        ))

;(vrste-kolone '((x x x x)(o o o o)(x o x o)(x x o x)(x x x x)(o o o o)(x o x o)(x x o x)(x x x x)(o o o o)(x o x o)(x x o x)(x x x x)(o o o o)(x o x o)(x x o x)) 'x 4 2 0)
;(horizontal '((x x x x)(o o o o)(x o x o)(x x o x)(x x x x)(o o o o)(x o x o)(x x o x)(x x x x)(o o o o)(x o x o)(x x o x)(x x x x)(o o o o)(x o x o)(x x o x)) 'x 4 0)


;;;                                                      Odredjivanje poena po dijagonali-2D
(defun diagonal-2d (state xo n k)
  
    (cond ((null state ) '())
          (t (+ (diagonal-2dGore_Dole state xo n k ) (diagonal-2dLevo_Desno state xo n k ) (diagonal-2dNapred_Nazad state xo n k )))
    )
)

;;;-------------------------------------------------------------------------------GoreDole------------------------------------------------------------------

(defun diagonal-2dGore_Dole (state xo n k)
        (cond((null state) '())
        (t (+ (napred (napravi_state_za_GoreDole state n 0 k) xo n k) (napred (reverse (napravi_state_za_GoreDole state n 0 k)) xo n k) ))
        )
)

(defun napravi_state_za_GoreDole (state n element k)

  (cond
      ((= k 0) '())
      (t (append (napravi_matricu_za_GoreDole state n element) (napravi_state_za_GoreDole state n (1+ element) (- k n)) )) 
  )

)

(defun napravi_matricu_za_GoreDole (state n element)

  (cond
      ((null state) '())
      (t (cons (napravi_stapic_prvi state n element) (napravi_matricu_za_GoreDole (skini_n_susednih state n) n element) )) 
  )

)

(defun napravi_stapic_prvi (state n element) ; pravi listu prvih elemenata od 4 redna stapica
  (cond
      ((equal n 0) '())
      ((> n 0) (cons (nth element (car state)) (napravi_stapic_prvi (cdr state) (1- n) element)))
  )
)
;;;---------------------------------------------------------------------------------LevoDesno----------------------------------------------------------------

(defun diagonal-2dLevo_Desno (state xo n k)
        (cond((null state) '())
             (t (+ (napred (napravi_state_za_LevoDesno state n k n) xo n k) (napred (reverse (napravi_state_za_LevoDesno state n k n)) xo n k) ))      
        )
)

(defun napravi_state_za_LevoDesno (state n k br) ; kreira rekombinovanu listu state, svi cetvrti stapici ce biti jedan do drugog

(cond((null state) '())
          ((> k (- (* br br) n))(append (izdvoji_svaki_nti state n 0 k) (napravi_state_za_LevoDesno (cdr state) n (- k 1) br)  ))
          ((= k (- (* br br) n)) '())
    )

)

(defun izdvoji_svaki_nti (state n element k) ; vraca listu gde je svaki n-ti stapic spojen jedan do drugog 
    (cond((null state) '())
          ((< element k)(cons (nth element state) (izdvoji_svaki_nti  state n (+ element n) k) ))
          ((= element (1- k)) '())
    )
)

;;;-----------------------------------------------------Napred-nazad-------------------------------------------------------------------------

(defun diagonal-2dNapred_Nazad (state xo n k)
       
       (cond  
              ((null state) '())
              (t (+ (napred state xo n k) (napred (reverse state) xo n k) ))
       )
)

(defun napred (state xo n k)

 (cond   ((null state) 0)
          ((equal k 0) 0)
         (t (+ (dijagonala_ispod state xo n) (iznad_dijagonale state xo (1- n) 1) (napred (skini_n_susednih state n) xo n (- k n)) ))             
  )
)

(defun skini_n_susednih (state n)

  (cond ((null state) '())
        ((> n 0) (skini_n_susednih (cdr state) (1- n) ))
        ((equal n 0) state)
  )
)

(defun iznad_dijagonale (state xo n element)

        (cond((null state) '())
        
             ((> n 3) (+ (max-4 (napravi_stapic state n element) xo '() '() '()) (iznad_dijagonale  state xo (1- n) (1+ element) )))
             ((equal n 3) 0)
        )
)

(defun dijagonala_ispod (state xo n)

        (cond((null state) '())
        
             ((> n 3) (+ (max-4 (napravi_stapic state n '0) xo '() '() '()) (dijagonala_ispod (cdr state) xo (1- n))))
             ((equal n 3) 0)
        )
)

(defun napravi_stapic (state n element) ; pravi listu dijagonalnih elemenata od 4 redna stapica
  (cond
      ((equal n 0) '())
      ((> n 0) (cons (nth element (car state)) (napravi_stapic (cdr state) (1- n) (1+ element))))
  )
)
;(print (diagonal-2d '((x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)) 'x 6 36))
;(print (napravi_state_za_GoreDole '((x x x x)(o o o o)(x x x x)(o o o o)(x x x x)(o o o o)(x x x x)(o o o o)(x x x x)(o o o o)(x x x x)(o o o o)(x x x x)(o o o o)(x x x x)(o o o o)) 4 0 16))

;;;                                                      Odredjivanje poena po dijagonali-3D


(defun diagonal11 (state n i br br1 m); br=0 na pocetku
  (cond ((equal br br1) '())
        ((< m 0) (diagonal11 state n i (1+ br) br1 (1+ m)))
        (t (cons (nth m (nth (+ i (* n br) br) state)) (diagonal11 state n i (1+ br) br1 (1+ m))))
        ))

(defun diagonal12 (state n i br br1 m); br=0 na pocetku
  (cond ((equal br br1) '())
        ((equal m -1) '())
        ((> m (1- n)) (diagonal12 state n i (1+ br) br1 (1- m)))
        (t (cons (nth m (nth (+ i (* n br) br) state)) (diagonal12 state n i (1+ br) br1 (1- m))))
        ))

(defun diagonal13 (state n i br1 m xo) ;m ide od (n-4) do -(n-4)
  (cond ((< m (- 4 n)) 0)
        (t (+ (max-4 (diagonal11 state n i 0 br1 m) xo '() '() '()) (max-4 (diagonal12 state n i 0 br1 (- n 1 m)) xo '() '() '()) (diagonal13 state n i br1 (1- m) xo)))
        ))

(defun diagonal1 (state n i xo); i=0 na pocetku
  (cond ((> i (- n 4)) 0)
        ((equal i 0) (+ (diagonal13 state n 0 n (- n 4) xo) (diagonal1 state n (1+ i) xo)))
        (t (+ (diagonal13 state n i (- n i) (- n 4) xo) (diagonal13 state n (* n i) (- n i) (- n 4) xo) (diagonal1 state n (1+ i) xo)))
        ))

(defun diagonal21 (state n i br br1 m); br=0 na pocetku 
  (cond ((equal br br1) '())
        ((< m 0) (diagonal21 state n i (1+ br) br1 (1+ m)))
        (t (cons (nth m (nth (- (+ i (* n br)) br) state)) (diagonal21 state n i (1+ br) br1 (1+ m))))
        ))

(defun diagonal22 (state n i br br1 m); br=0 na pocetku 
  (cond ((equal br br1) '())
        ((equal m -1) '())
        ((> m (1- n)) (diagonal22 state n i (1+ br) br1 (1- m)))
        (t (cons (nth m (nth (- (+ i (* n br)) br) state)) (diagonal22 state n i (1+ br) br1 (1- m))))
        ))

(defun diagonal23 (state n i br1 m xo) ;m ide od (n-4) do -(n-4)
  (cond ((< m (- 4 n)) 0)
        (t (+ (max-4 (diagonal21 state n i 0 br1 m) xo '() '() '()) (max-4 (diagonal22 state n i 0 br1 (- n 1 m)) xo '() '() '()) (diagonal23 state n i br1 (1- m) xo)))
        ))

(defun diagonal2 (state n i xo); i=n-1 na pocetku
  (cond ((< i (- n 1 (- n 4))) 0) 
        ((equal i (1- n)) (+ (diagonal23 state n i n (- n 4) xo) (diagonal2 state n (1- i) xo)))
        (t (+ (diagonal23 state n i (1+ i) (- n 4) xo) (diagonal23 state n (1- (* (- n i) n)) (1+ i) (- n 4) xo) (diagonal2 state n (1- i) xo)))
        ))

(defun diagonal-3d (state xo n) (+ (diagonal1 state n 0 xo) (diagonal2 state n (1- n) xo)))
 
;(diagonal-3d '((x x x x)(o o o o)(x o x o)(x x o x)(x x x x)(o x x o)(x x x o)(x x o x)(x x x x)(o x x o)(x x x o)(x x o x)(x x x x)(o o o o)(x o x o)(x x o x)) 'x 4)
;(diagonal-3d '((x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)) 'x 6)



;;;                                                      III FAZA



(defun min-max (state n depth alfa beta minmax player hplayer)
     (cond
        ((= depth 0) (heuristika state n (* n n) hplayer))
        ((final-statep state) (heuristika state n (* n n) hplayer))
        (
            (= minmax 1)
            (let*
                (
                    (value (list '() -9999))
                    (children (generate-all-states state player (* n n) 0))
                )
                (min-max2 children n depth alfa beta minmax player value hplayer)                
            )
            
        )
        (
            (= minmax 0)
            (let*
                (
                    (value (list '() 9999))
                    (children (generate-all-states state player (* n n) 0))
                )
                (min-max2 children n depth alfa beta minmax player value hplayer)
            )
           
        )))


(defun min-max2 (list-states n depth alfa beta minmax player value hplayer)
    (cond
        ((null list-states) value)
        (
            (= minmax 1)
            (let*
                (
                    (new-value (greater-or-smaller '> (list (car list-states) (nth 1 value)) (min-max (car list-states) n (1- depth) alfa beta 0 (switch-player player) hplayer)))
                    (new-alfa (greater-or-smaller '> alfa new-value))
                )
                (cond
                    ( (>= (nth 1 new-alfa) (nth 1 beta)) (cond ((> (nth 1 new-value) (nth 1 value)) new-value) (t value))) ; beta odsecanje
                    (t (min-max2 (cdr list-states) n depth new-alfa beta minmax player (cond ((> (nth 1 new-value) (nth 1 value)) new-value) (t value)) hplayer))
                )
            )
        )
        (
            (= minmax 0)
             (let*
                (
                    (new-value (greater-or-smaller '< (list (car list-states) (nth 1 value)) (min-max (car list-states) n (1- depth) alfa beta 1 (switch-player player) hplayer)))
                    (new-beta (greater-or-smaller '< beta new-value))
                )
                (cond
                    ( (>= (nth 1 alfa) (nth 1 new-beta)) (cond ((< (nth 1 new-value) (nth 1 value)) new-value) (t value))) ; alfa odsecanje
                    (t (min-max2 (cdr list-states) n depth alfa new-beta minmax player (cond ((< (nth 1 new-value) (nth 1 value)) new-value) (t value)) hplayer))
                )
            )
        )
    )
)

;(defun heuristika (state n k xo)
 ;   (let* ((x (+ (horizontal state 'x n 0) (vertical state 'x k 0) (diagonal-2d state 'x n k) (diagonal-3d state 'x n)))
 ;          (o (+ (horizontal state 'o n 0) (vertical state 'o k 0) (diagonal-2d state 'o n k) (diagonal-3d state 'o n))))
 ;     (cond
 ;       ((equalp xo 'x) (list state (- x o)))
 ;       ((equalp xo 'o) (list state (- o x)))
 ;   ))
;)

(defun greater-or-smaller (op h1 h2)
    (cond
        ((apply op (list (nth 1 h2) (nth 1 h1))) (list (car h1) (nth 1 h2)))
        (t h1)
    )
)

(defun switch-player (player)
    (cond 
        ((equalp player 'x) 'o)
        ((equalp player 'o) 'x)
    )
)

(defun depth (state n num) ; num/n*n*n*100 procenat popunjenosti 
  (let* ((percent (* (/ (float num) (* n n n)) 100))
         (minus (if (> n 4) 1 0)))
    (cond
        ((< percent 40) (- 2 minus))
        ((< percent 60) (- 3 minus))
        ((< percent 75) (- 4 minus))
        ((< percent 90) (- 5 minus))
        (t (- 6 minus))
    ))
  )


;(min-max '((x x x x)(o o o o)(x o x "-")(x x o x)(x x x x)(o x x o)(x x o "-")(x x o x)(x x x x)(o x x o)(x x x o)(x x o "-")(x x x x)(o o o o)(x o x o)(x x o x)) 4 3 (list '() -9999) (list '() 9999) 1 'x 'x)



;;;                                                      IV FAZA



(defun =add (i l) (+ i l))
(defun =add3 (i j k) (+ i j k))
(defun =add4 (i j k l) (+ i j k l))
(defun =add5 (i j k l m) (+ i j k l m))
(defun !eq (a b)
  (equal a b))

(defun !ne (a b)
  (not (equal a b)))

(defparameter *T1-RULES* '(
                           (if (and (on ?x ?i ?j)(!eq ?x "x")(on ?x ?i (=add ?j 1))(on ?x ?i (=add ?j 2))(on ?x ?i (=add ?j 3))) then (Vertikala ?x ?i))
                           (if (and (on ?x ?i ?j)(!eq ?x "o")(on ?x ?i (=add ?j 1))(on ?x ?i (=add ?j 2))(on ?x ?i (=add ?j 3))) then (VertikalaM ?x ?i))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(on ?x (=add ?i ?n) ?j)(on ?x (=add3  ?i ?n ?n) ?j)(on ?x (=add4 ?i ?n ?n ?n) ?j)) then (Horizontala1 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(on ?x (=add ?i ?n) ?j)(on ?x (=add3 ?i  ?n ?n) ?j)(on ?x (=add4 ?i ?n ?n ?n) ?j)) then (Horizontala1M ?x ?i ?j)) 
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj (=add ?i 3))(on ?x (=add ?i 1) ?j)(on ?x (=add ?i 2) ?j)(on ?x (=add ?i 3) ?j)) then (Horizontala2 ?x ?i ?j)) 
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj (=add ?i 3))(on ?x (=add ?i 1) ?j)(on ?x (=add ?i 2) ?j)(on ?x (=add ?i 3) ?j)) then (Horizontala2M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj (=add ?i 3))(on ?x (=add3 ?i ?n 1) ?j)(on ?x (=add4 ?i ?n ?n 2) ?j)(on ?x (=add5 ?i ?n ?n ?n 3) ?j)) then (Dijagonala1 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj (=add ?i 3))(on ?x (=add3 ?i ?n 1) ?j)(on ?x (=add4 ?i ?n ?n 2) ?j)(on ?x (=add5 ?i ?n ?n ?n 3) ?j)) then (Dijagonala1M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj ?i)(on ?x (=add3 ?i ?n -1) ?j)(on ?x (=add4 ?i ?n  ?n -2) ?j)(on ?x (=add5 ?i ?n ?n ?n -3) ?j)) then (Dijagonala2 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj ?i)(on ?x (=add3 ?i ?n -1) ?j)(on ?x (=add4 ?i ?n  ?n -2) ?j)(on ?x (=add5 ?i ?n ?n ?n -3) ?j)) then (Dijagonala2M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj (=add ?i 3))(on ?x (=add ?i 1) (=add ?j 1))(on ?x (=add ?i 2) (=add ?j 2))(on ?x (=add ?i 3) (=add ?j 3))) then (Dijagonala3 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj (=add ?i 3))(on ?x (=add ?i 1) (=add ?j 1))(on ?x (=add ?i 2) (=add ?j 2))(on ?x (=add ?i 3) (=add ?j 3))) then (Dijagonala3M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj (=add ?i 3))(on ?x (=add ?i 1) (=add ?j -1))(on ?x (=add ?i 2) (=add ?j -2))(on ?x (=add ?i 3) (=add ?j -3))) then (Dijagonala4 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj (=add ?i 3))(on ?x (=add ?i 1) (=add ?j -1))(on ?x (=add ?i 2) (=add ?j -2))(on ?x (=add ?i 3) (=add ?j -3))) then (Dijagonala4M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(on ?x (=add ?i ?n) (=add ?j 1))(on ?x (=add3 ?i  ?n ?n) (=add ?j 2))(on ?x (=add4 ?i  ?n  ?n ?n) (=add ?j 3))) then (Dijagonala5 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(on ?x (=add ?i ?n) (=add ?j 1))(on ?x (=add3 ?i  ?n ?n) (=add ?j 2))(on ?x (=add4 ?i  ?n  ?n ?n) (=add ?j 3))) then (Dijagonala5M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(on ?x (=add ?i ?n) (=add ?j -1))(on ?x (=add3 ?i  ?n ?n) (=add ?j -2))(on ?x (=add4 ?i  ?n  ?n ?n) (=add ?j -3))) then (Dijagonala6 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(on ?x (=add ?i ?n) (=add ?j -1))(on ?x (=add3 ?i  ?n ?n) (=add ?j -2))(on ?x (=add4 ?i  ?n  ?n ?n) (=add ?j -3))) then (Dijagonala6M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj (=add ?i 3))(on ?x (=add3 ?i  ?n 1) (=add ?j 1))(on ?x (=add4 ?i  ?n ?n 2) (=add ?j 2))(on ?x (=add5 ?i ?n ?n ?n 3) (=add ?j 3))) then (Dijagonala7 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj (=add ?i 3))(on ?x (=add3 ?i  ?n 1) (=add ?j 1))(on ?x (=add4 ?i ?n  ?n 2) (=add ?j 2))(on ?x (=add5 ?i  ?n ?n ?n 3) (=add ?j 3))) then (Dijagonala7M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj (=add ?i 3))(on ?x (=add3 ?i  ?n 1) (=add ?j -1))(on ?x (=add4 ?i  ?n  ?n 2) (=add ?j -2))(on ?x (=add5 ?i  ?n  ?n  ?n 3) (=add ?j -3))) then (Dijagonala8 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj (=add ?i 3))(on ?x (=add3 ?i  ?n 1) (=add ?j -1))(on ?x (=add4 ?i  ?n  ?n 2) (=add ?j -2))(on ?x (=add5 ?i  ?n  ?n  ?n 3) (=add ?j -3))) then (Dijagonala8M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj ?i)(on ?x (=add3 ?i  ?n -1) (=add ?j 1))(on ?x (=add4 ?i  ?n  ?n -2) (=add ?j 2))(on ?x (=add5 ?i  ?n  ?n  ?n -3) (=add ?j 3))) then (Dijagonala9 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj ?i)(on ?x (=add3 ?i  ?n -1) (=add ?j 1))(on ?x (=add4 ?i  ?n  ?n -2) (=add ?j 2))(on ?x (=add5 ?i  ?n  ?n  ?n -3) (=add ?j 3))) then (Dijagonala9M ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "x")(kraj ?i)(on ?x (=add3 ?i  ?n -1) (=add ?j -1))(on ?x (=add4 ?i  ?n  ?n -2) (=add ?j -2))(on ?x (=add5 ?i  ?n  ?n  ?n -3) (=add ?j -3))) then (Dijagonala10 ?x ?i ?j))
                           (if (and (Velicina ?n)(on ?x ?i ?j)(!eq ?x "o")(kraj ?i)(on ?x (=add3 ?i  ?n -1) (=add ?j -1))(on ?x (=add4 ?i  ?n  ?n -2) (=add ?j -2))(on ?x (=add5 ?i  ?n  ?n  ?n -3) (=add ?j -3))) then (Dijagonala10M ?x ?i ?j))
))


;(defparameter *T1-FACTS* (state-to-facts state n 0))

(defun state-to-facts (state n i) ; i=0 na pocetku
  (cond ((null state) (list (list 'Velicina n)))
        ((and (>= (mod i n) 3) (<= (mod i n) 5)) (append (list (list 'kraj i)) (append (stick-to-facts (car state) i 0) (state-to-facts (cdr state) n (1+ i)))))
        (t (append (stick-to-facts (car state) i 0) (state-to-facts (cdr state) n (1+ i))))
        ))

(defun stick-to-facts (stick i j) ; j=0 na pocetku
  (cond ((null stick) '())
        ((equal (car stick) "-") '())
        ((equal (car stick) 'x) (cons (list 'on "x" i j) (stick-to-facts (cdr stick) i (1+ j))))
        (t (cons (list 'on "o" i j) (stick-to-facts (cdr stick) i (1+ j))))
        ))

;(defparameter *T1-FACTS* (state-to-facts '((x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)(x x x x x x)) 6 0))
;(print *T1-FACTS*)
;(defparameter *T1-FACTS* (state-to-facts '((x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)(x x x x)) 4 0))
;(prepare-knowledge *T1-RULES* *T1-FACTS* 10)

;(count-results '(Vertikala "x" ?x))
;(count-results '(Horizontala2 "x" ?x ?y))
;(count-results '(Dijagonala1 "x" ?x ?y))


(defun heuristika (state n k xo)
  (let* (  (*T1-FACTS* (state-to-facts state n 0))
           (knowledge (prepare-knowledge *T1-RULES* *T1-FACTS* 9))
           (x (+ (count-results '(Vertikala "x" ?x)) (* 11 (count-results '(Horizontala1 "x" ?x ?y))) (* 6(count-results '(Horizontala2 "x" ?x ?y))) (* 26 (count-results '(Dijagonala1 "x" ?x ?y))) (* 26 (count-results '(Dijagonala2 "x" ?x ?y))) (* 16 (count-results '(Dijagonala3 "x" ?x ?y))) (* 16 (count-results '(Dijagonala4 "x" ?x ?y))) (* 21 (count-results '(Dijagonala5 "x" ?x ?y)))(* 21 (count-results '(Dijagonala6 "x" ?x ?y)))  (* 31 (count-results '(Dijagonala7 "x" ?x ?y))) (* 31 (count-results '(Dijagonala8 "x" ?x ?y))) (* 31 (count-results '(Dijagonala9 "x" ?x ?y))) (* 31 (count-results '(Dijagonala10 "x" ?x ?y))) ))
           (o (+ (count-results '(VertikalaM "o" ?x)) (* 11 (count-results '(Horizontala1M "o" ?x ?y))) (* 6(count-results '(Horizontala2M "o" ?x ?y))) (* 26 (count-results '(Dijagonala1M "o" ?x ?y))) (* 26 (count-results '(Dijagonala2M "o" ?x ?y))) (* 16 (count-results '(Dijagonala3M "o" ?x ?y))) (* 16 (count-results '(Dijagonala4M "o" ?x ?y))) (* 21 (count-results '(Dijagonala5M 'o ?x ?y))) (* 21 (count-results '(Dijagonala6M "o" ?x ?y))) (* 31 (count-results '(Dijagonala7M "o" ?x ?y))) (* 31 (count-results '(Dijagonala8M "o" ?x ?y))) (* 31 (count-results '(Dijagonala9M "o" ?x ?y))) (* 31 (count-results '(Dijagonala10M "o" ?x ?y))) ))
      )(cond
        ((equalp xo 'x) (list state (- x o)))
        ((equalp xo 'o) (list state (- o x)))
       )
         )
)


(main (initialization-game))
