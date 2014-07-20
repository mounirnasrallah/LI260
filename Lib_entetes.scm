(require (lib "struct.ss"))

;;;;;;;;;;;;;;;;;;;

;; accesV : List[alpha] * nat -> alpha
;;(accesV V i) retourne l'élément en position i de la liste V
;; Hypothese : V contient au moins i entiers naturels
(define (accesV V i)

;; accesM : List[List[alpha]] * nat * nat -> alpha
;;(accesM M i j) retourne le contenu de la case (i,j), avec i l'index de la liste dans M et j l'index de l'élément visé dans cette i-ème liste
;; Hypothese : M contient au moins i listes contenant au moins j éléments chacune
(define (accesM M i j)

;; cree-liste : nat -> List[nat]
;; (cree-liste M) crée une liste de N listes de nombres de 1 à M.
;; Hypothèse : M>=0   (si M=0, alors on retourne la liste vide)
(define (cree-liste M)

;; cree-liste-cases : nat * nat -> List[List[nat]]
;; (cree-liste-cases N M) crée une liste de N listes contenant des nombres de 1 à M
;; Hypothèse : N>=0 et M>=0   (si M=0, alors on retourne un liste de N listes vides) 
(define (cree-liste-cases N M)

;; init-ligne : List[alpha] * beta -> List[beta]
;; (init-ligne V X) retourne une liste contenant n fois X, où n = taille de V
(define (init-ligne V X)

;; initialise : List[List[alpha]] * beta -> List[List[beta]]
;; (initialise M X) retourne une liste de m listes contenant n_i fois X chacune, où m=nombre de listes dans M et n_i = taille de la liste d'indice i dans M
(define (initialise M X)

;;; etatdejeu : List[List[nat]] * List[Pair[nat nat]] * Pair[nat nat] * nat * Pair[nat nat] 
;;; Structure de données pour le Jeu :
;;; plateau : liste de listes (lignes du plateau) d'entiers correspondant aux valeurs (graines contenues) des cases du plateau
;;; coups-joues : liste de tous les coups ayant été joués
;;; scores : paire de scores (score-joueur1,score-joueur2)
;;; joueur : joueur courant (celui qui joue) = 1 ou 2
;;; coup : non utilisé
(define-struct etatdejeu (plateau coups-joues scores joueur coup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initialise-jeu : void -> etatdejeu
;; (initialise-jeu) retourne l'etat de jeu suivant :
;; plateau = '('(4 4 4 4 4 4) (4 4 4 4 4 4))
;; coups-joues = '()
;; scores = (0,0)
;; joueur = 1
;; coup = '()
(define (initialise-jeu )
  
;; display-plateau : etatdejeu -> void
;; (display-plateau ej) affiche l'etat de jeu ej :
;;           --> <joueur courant>
;;           <cases du joueur 1> --> <score-joueur1>
;;           <cases du joueur 2> --> <score-joueur2>
;;           <Somme scores + graines sur plateau (normallement 64)> 
;;           -------------------------------------------------------
(define (display-plateau ej) 
  
 
;; autre-joueur : nat -> nat
;;(autre-joueur joueur) retourne l'adversaire de joueur (si 1 => 2, si 2 => 1)
(define (autre-joueur joueur)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; liste-coups-valides :  etatdejeu * nat ->  List[Pair[nat nat]]
;; (liste-coups-valides ej j) -> retourne la liste de coups valides  pour le joueur j selon l'etat de jeu ej
;; note : utilise coups-valides et coups
(define (liste-coups-valides ej j)

;; coup-valide? : Pair[nat nat] * etatdejeu * nat -> bool
;; (coup-valide? coup etat-jeu joueur) retourne vrai si :
;;             - La case correspondant au coup appartient bien au joueur joueur
;;             - La case correspondant au coup contient au moins une graine
;;             - Le coup n'affame pas l'adversaire de joueur. Cad, si (affame-adversaire? coup etat-jeu joueur) retourne faux 
(define (coup-valide? coup etat-jeu joueur)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;; joue-coupN : etatdejeu * Pair[nat nat] -> etatdejeu
;; (joue-coupN ej coup) : joue le coup coup (grâce à la fonction distribue) sur l'etat de jeu ej (permet de simuler l'execution d'un coup). Retourne la copie ainsi modifiée
(define (joue-coupN ej coup)

;; joue-coup : etatdejeu * Pair[nat nat] -> etatdejeu
;; (joue-coup etat-jeu coup) : joue le coup coup (grâce à la fonction distribue) sur l'etat de jeu ej, puis enregistre le coup dans sa liste de coups-jouées. Retourne l'etat de jeu ainsi modifié
(define (joue-coup etat-jeu coup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; retourne-gagnant : etatdejeu -> nat
;; (retourne-gagnant R) retourne le joueur ayant le meilleur score
(define (retourne-gagnant R)

;; fin-jeu? : etatdejeu * nat -> bool
;; (fin-jeu? ej j) retourne un booléen informant du fait que la partie est terminée ou non
;; note : la partie est terminée si le joueur j n'a aucun coup valide à jouer
(define (fin-jeu? ej j)

;; finalise-partie : etatdejeu -> etatdejeu
;; (finalise-partie ej) ajoute les graines restantes dans chaque camp au score du joueur correspondant, puis retourne l'etat de jeu ainsi modifié
(define (finalise-partie ej)


