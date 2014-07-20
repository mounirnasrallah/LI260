;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utile_jeu.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require (lib "struct.ss"))

;;; Fonctions utiles
;;;;;;;;;;;;;;;;;;;;


;;; accesV : Accede au ième élement de la liste V ( Vecteur ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accesV V i)
  (if (= i 1) (car V) (accesV (cdr V) (- i 1))))


;;; accesM : Accede au ième élement de la jème liste M ( Matrice ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accesM M i j)
  (if (= i 1) (accesV (car M) j)
      (accesM (cdr M) (- i 1) j)))  


;;; cree-liste : Créé une liste de M élements, dont les valeurs sont de 1 à M 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cree-liste M)
  (if (= M 0) () (append (cree-liste (- M 1)) (list M)) )) 


;;; cree-liste-cases : Créé une liste de N liste ayant chacune M élements dont les valeurs sont de 1 à M (cree-liste-cases 2 4) -> ((1 2 3 4) (1 2 3 4)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cree-liste-cases N M)
  (if (= N 0) ()
      (cons (cree-liste M) (cree-liste-cases (- N 1) M)))) 


;;; init-ligne : Modifie/Initialise le Vecteur V, en changeant la valeur de tout ses élements par X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (init-ligne V X)
  (if (equal? V ()) ()
      (cons X (init-ligne (cdr V) X))))


;;; initialise : Modifie/Initialise la Matrice M ( liste de liste ), en changeant la valeur de tout ses élements par X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialise M X)
  (if (equal? M ()) ()
      (begin
        (let ((l (init-ligne (car M) X)))
          (cons l (initialise (cdr M) X)))
        )
      )
  )


;;; majLR : Mise a jour de la valeur de la colonne "col" du vecteur gr par v.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (majLR gr col v)
  (if (= col 1)
      (cons v (cdr gr))
      (cons (car gr) (majLR (cdr gr) (- col 1) v))))

 
;;; majR :  Mise a jour de la valeur de l'element ayant pour ligne (car cel) et pour colonne (cdr cel) par la valeur v
;;; En somme, mise à jour de l'element i,j par la valeur v dans la matrice gr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (majR gr cel v)
  (if (= (car cel) 1) (cons (majLR (car gr) (cdr cel) v) (cdr gr) )
      (cons (car gr) (majR (cdr gr) (cons (- (car cel) 1) (cdr cel)) v))))


;;; Creation de la structure etatdejeu 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-struct etatdejeu-perso (plateau coups-joues scores joueur coup))


;;; display-plateau : Affichage du plateau de jeux 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-plateau ej)
  (let ((plateau (etatdejeu-plateau ej)))
    (newline)
    (display "--> ")
    (display (etatdejeu-joueur ej))
    (newline)
    (display (car plateau))
    (display "    -->")
    (display (car (etatdejeu-scores ej) ))    
    (newline)
    (display (cadr plateau))
    (display "    -->")
    (display (cdr (etatdejeu-scores ej) ))
    (newline)
    (display "Liste des coups valides : ")
    (display (liste-coups-valides ej (etatdejeu-joueur ej)))
    (newline)
    (display "---------------------------------------")))    


;;; initialise-jeu : Initialise le jeu en créant les "objets" necessaire au jeu
;;; la structure etatdejeu contient donc :
;;;
;;; plateau -> Qui est une matrice (list (list ..) (list ...)), sa valeur par defaut est : (list (list 4 4 4 4 4 4) (list 4 4 4 4 4 4))
;;; coupsjoues -> est une liste, sa valeur par defaut est : () ,donc une liste vide
;;; scores -> est une liste de deux nombres, ayant par defaut (0 0)
;;; joueur -> est un numéro, le numéro du joueur, étant dans l'ensemble {1,2}, par defaut le joueur 1 commence la partie
;;; coup -> est une liste, sa valeur par defaut est une liste vide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialise-jeu )
  (let* ( 
         (plateau (initialise (cree-liste-cases 2 6) 4))
         (coupsjoues ())
         (scores (cons 0 0))
         (joueur 1)
         (coup ()))
    (make-etatdejeu plateau coupsjoues scores joueur coup))) 


;;; casevalide? :  Verifie que la cas appartient bien au joueur numero j. donc que (car c) == j, renvoie true ou false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (casevalide? c j)
  (= (car c) j)   
  )

;;; casenonvide? : Verifie si la case ayant pour ligne (car c) et pour colonne (cdr c) est non vide, 
;;; donc plus grand à 0, dans le "plateau" de l'argument ej de type etatdejeu  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (casenonvide? ej c)
  (> (accesM (etatdejeu-plateau ej) (car c) (cdr c)) 0)
  )

;;; existecoupnourrit? : 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (existecoupnourrit? ej j)
  (let (
        (lc (cases ej j))
        )
    (existecoupnourritR? ej lc j)
    )
  )


;;; existecoupnourritR? : 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (existecoupnourritR? ej lc j)
  (if (= () lc)
      #f
      (if (nourrit ej c j)  #t 
          (existecoupnourritR? ej (cdr lc) j )
          )
      )
  )

;;; sommeliste : Calcule la somme des elements de la liste l 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sommeliste l)
  (if (pair? l)
      (+ (car l) (sommeliste (cdr l)))
      0
      )
  )

;;; affame? : Renvoie Vrai : Si le joueur 2 affame le joueur 1 ( la somme du plateau du joueur 1 est nul ) 
;;;           Renvoie Vrai : Si le joueur 1 affame le joueur 2 ( la somme du plateau du joueur 2 est nul )
;;;           Renvoie Faux Sinon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (affame? ej j)
  (or (and (= j 1) (= 0 (sommeliste (car (etatdejeu-plateau ej))))) (and (= j 2) (= 0 (sommeliste (cadr (etatdejeu-plateau ej))))))
  )

;;; autre-joueur : Renvoie le numéro du joueur adversaire 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (autre-joueur joueur)
  (if (= joueur  1) 
      2
      1
      ))

;; coup-valide? : Pair[nat nat] * etatdejeu * nat -> bool
;; (coup-valide? coup etat-jeu joueur) retourne vrai si :
;;             - La case correspondant au coup appartient bien au joueur joueur
;;             - La case correspondant au coup contient au moins une graine
;;             - Le coup n'affame pas l'adversaire de joueur. Cad, si (affame-adversaire? coup etat-jeu joueur) retourne faux 

(define (coup-valide? ej c j)
  (if (= (car c) j)
      (if (and (number? (cdr c)) (< (cdr c) 7) )
          (begin
            (if (casevalide? c j)
                (if (casenonvide? ej c)
                    (if (affame? ej (autre-joueur j)) 
                        (if (existecoupnourrit? ej c j)
                            #t 
                            #f
                            )
                        #t)
                    #f)
                #f)
            )
          #f)
      #f)
  )
;(define (coup-valide? ej c j)
 ; (if (= (car c) j)
  ;    (if (and (number? (cdr c)) (< (cdr c) 7) )
   ;       (begin
    ;        (if (casevalide? c j)
     ;           (if (casenonvide? ej c)
      ;              (if (affame? ej (autre-joueur j)) 
       ;                 ;;;; !!!!!!!!!!!!!!!!!!!!!!!! nourrit? OU EXITE T IL !!!!!!!!!!!!!!!!!!!!!!!!
        ;                (if (existecoupnourrit? ej c j)
         ;                   #t 
          ;                  #f
           ;                 )
             ;           #t)
            ;        #f)
    ;            #f)
   ;         )
  ;        #f)
   ;   #f)
 ; )

;;; change-joueur : Change le numéro du joueur courant dans etatdejeu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define (change-joueur ej)
  (if (= (etatdejeu-joueur ej) 1) 
      (begin
        (set-etatdejeu-joueur! ej 2))
      (begin
        (set-etatdejeu-joueur! ej 1)))
  )
 
;;; caseprecedente : Renvoie le couple des coordonnees de la case precedente ayant comme coordonnee (ligne,colonne)->((car case),(cdr case)) sous forme d'une liste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (caseprecedente case)
  (let ((l (car case)) (c (cdr case))) 
    (if (= l 1)
        (if (< c 6) (cons l (+ 1 c))
            (cons 2 6)
            )
        (if (> c 1) (cons 2 (- c 1))
            (cons 1 1)
            )
        ))) 

;;; casesuivante : Renvoie le couple des coordonnees de la case suivante ayant comme coordonnee (ligne,colonne)->((car case),(cdr case)) sous forme d'une liste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (casesuivante case)
  (let ((l (car case)) (c (cdr case)))
    (if (= l 1)
        (if (> c 1) 
            (cons 1 (- c 1))
            (cons 2 1)
            )
        (if (< c 6) 
            (cons l (+ 1 c))
            (cons 1 6)
            )
        ))) 


;;; majscore : Met a jour le score dans etatdejeu, selon le numéro du joueur
;;; Si le joueur est joueur 1, alors on met à jour le score du joueur 1 en faisant : nouveau score joueur 1 = ancien score joueur 1 + n
;;; Si le joueur est joueur 2, alors on met à jour le score du joueur 2 en faisant : nouveau score joueur 2 = ancien score joueur 2 + n
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (majscore ej n)
  (let* ((scores (etatdejeu-scores ej))
         (s1 (car scores))
         (s2 (cdr scores))
         (j (etatdejeu-joueur ej)))
    (if (= j 1)
        (set-etatdejeu-scores! ej (cons (+ s1 n) s2)) 
        (set-etatdejeu-scores! ej (cons s1 (+ s2 n)))
        )
    )
  )

;;; saisir-coup1 : Demande la saisie du coup à jouer pour le joueur numero 1 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (saisir-coup1 ej)
  (cons 1  (read))  
  )

;;; saisir-coup2 : Demande la saisie du coup à jouer pour le joueur numero 1 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (saisir-coup2 ej)
  (cons 2  (read))  
  ) 


;;; razl : Met a zero la valeur du jeme element du vecteur l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (razl j l) 
  (if (= j 1) (cons 0 (cdr l))
      (cons (car l) (razl (- j 1) (cdr l)))
      )
  )


 
;;; razl : Met a zero la valeur de l'element ayant pour coordonnees i,j de la matrice m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (razm i j m)
  (if (= i 1) (cons (razl j (car m)) (cdr m))
      (cons (car m) (razm (- i 1) j (cdr m)))
      )
  )

;;; mangecase? : Verifie si l'on peut manger une case, donc si il y a 2 ou 3 graines et que ca appartient au joueur adversaire
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mangecase? ej j i)
  (let ((x (accesM (etatdejeu-plateau ej) j i)))
    (and  (not (= j (etatdejeu-joueur ej))) (or  (= x 3) (= x 2))))
  )

;;; mange : Permet de manger une case de coordonnees (car case) (cdr case) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mange ej case cpt)
  (if (< cpt 4) 
      (if (mangecase? ej (car case) (cdr case))
          (begin
            (let ((n (accesM (etatdejeu-plateau ej) (car case) (cdr case)))
                  )
              (set-etatdejeu-plateau! ej (razm (car case) (cdr case) (etatdejeu-plateau ej)))
              (majscore ej n)
              (mange ej (caseprecedente case) (+ cpt 1))
              )))))

;;; majLR : 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define  (egraine ej case nbg cpt)
  (if (> nbg 0)
      (if (< cpt 12)
          (let ((c case))
            (set-etatdejeu-plateau! ej (majR (etatdejeu-plateau ej) case (+ 1 (accesM (etatdejeu-plateau ej) (car case) (cdr case)))))
            (egraine ej (casesuivante case) (- nbg 1 ) (+ cpt 1))
            
            )
          (egraine ej (casesuivante case) (- nbg 1 ) (+ cpt 1))
          )
      (caseprecedente case)
      )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; joue-coupN : etatdejeu * Pair[nat nat] -> etatdejeu
;; (joue-coupN ej coup) : joue le coup coup (grâce à la fonction distribue) sur l'etat de jeu ej (permet de simuler l'execution d'un coup). Retourne la copie ainsi modifiée
(define (joue-coupN ej coup) 
  (begin 
    (let* (
           (nbgraines (accesM (etatdejeu-plateau ej) (car coup) (cdr coup)))
           )
      (set-etatdejeu-plateau! ej (razm (car coup) (cdr coup) (etatdejeu-plateau ej)))
      (mange ej (egraine ej (casesuivante coup) nbgraines 0) 0)
      (set-etatdejeu-coup! ej coup )
      )))

;; joue-coup : etatdejeu * Pair[nat nat] -> etatdejeu
;; (joue-coup etat-jeu coup) : joue le coup coup (grâce à la fonction distribue) sur l'etat de jeu ej, puis enregistre le coup dans sa liste de coups-jouées. Retourne l'etat de jeu ainsi modifié
(define (joue-coup ej coup) 
  (begin 
    (let* (
           (nbgraines (accesM (etatdejeu-plateau ej) (car coup) (cdr coup)))
           (listedescoupsjouee (etatdejeu-coupsjoues ej))
           )
      (set-etatdejeu-plateau! ej (razm (car coup) (cdr coup) (etatdejeu-plateau ej)))
      (mange ej (egraine ej (casesuivante coup) nbgraines 0) 0)
      (set-etatdejeu-coupsjoues! ej (cons coup listedescoupsjouee))
      (set-etatdejeu-coup! ej coup )
      (set-etatdejeu-joueur! ej (autre-joueur (etatdejeu-joueur ej)) )
      ej
      )))


;;; fin-jeu? : retourne un booléen informant du fait que la partie est terminée ou non
;; note : la partie est terminée si le joueur j n'a aucun coup valide à jouer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fin-jeu? ej joueur)
  (or (and (= 1 joueur) (= 0 (sommeliste (car (etatdejeu-plateau ej)))))  (and (= 2 joueur) (= 0 (sommeliste (cadr (etatdejeu-plateau ej))))))
  )

;; Fonction recursive qui donne la liste de tout les coups possibles
(define (liste-coups-validesR ej j coup)
  (let* ((coupajouer (cons j coup)))
    (if (> coup 6)
        (begin
          (list )
          )
        (begin
          (if (coup-valide? ej coupajouer j)
              (begin
                (cons coupajouer (liste-coups-validesR ej j (+ 1 coup)))
                )
              (begin 
                (liste-coups-validesR ej j (+ 1 coup))
                )
              )
          )))
  )
 

;; liste-coups-valides :  etatdejeu * nat ->  List[Pair[nat nat]]
;; (liste-coups-valides ej j) -> retourne la liste de coups valides  pour le joueur j selon l'etat de jeu ej
;; note : utilise coups-valides et coups
(define (liste-coups-valides ej j)
  (liste-coups-validesR ej j 1)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; retourne-gagnant : etatdejeu -> nat
;; (retourne-gagnant R) retourne le joueur ayant le meilleur score
(define (retourne-gagnant ej)
  (let* (
         (scores (etatdejeu-scores ej))
         )
    (if (< (car score) (cdr score))
        1
        2
        )
    )
  )

;; finalise-partie : etatdejeu -> etatdejeu
;; (finalise-partie ej) ajoute les graines restantes dans chaque camp au score du joueur correspondant, puis retourne l'etat de jeu ainsi modifié
(define (finalise-partie ej)
  (let* (
         (plateau (etatdejeu-plateau ej))
         (graineResteJoueur1 (sommeliste (car plateau)))
         (graineResteJoueur2 (sommeliste (car plateau)))
         )
    
    (set-etatdejeu-plateau! ej (initialise plateau 0))
    
    (set-etatdejeu-joueur! ej 1)
    (majscore ej graineResteJoueur1)
    
    (set-etatdejeu-joueur! ej 2)
    (majscore ej graineResteJoueur2)
    )
  )


;;; joue : Fonction joue ( fonction principale )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (joue ej j)
  (display-plateau ej)
  
  (if (fin-jeu? ej)
      
      (begin (finalise-partie ej)
             (retourne-gagnant ej)
             )
      
      (begin
        (set-etatdejeu-joueur! ej j)
        (if (= j 1)
            (begin
              (let (( c (saisir-coup1 ej)))
                (if (coup-valide? ej c j)
                    (begin
                      (joue-coup ej c)
                      (change-joueur ej)
                      (joue ej (etatdejeu-joueur ej))
                      )
                    (begin (display "coup invalide")
                           (joue ej j)))))
            
            
            (begin
              (let ((c (saisir-coup2 ej)))
                (if (coup-valide? ej c j)
                    (begin
                      (joue-coup ej c)
                      (change-joueur ej)
                      (joue ej (etatdejeu-joueur ej))
                      )))
              (begin (display "coup invalide")
                     (joue ej j))
              )))))


;;; Initialisation du jeu, et lancement du jeu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(joue (initialise-jeu) 1)


;;;; *************************************************************************************************************************** ;;;;
;;;; *************************************************************************************************************************** ;;;;
;;;; *************************************************************************************************************************** ;;;;
;;;; *************************************************************************************************************************** ;;;;
;;;; *************************************************************************************************************************** ;;;;

