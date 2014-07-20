
(load-extension "Lib.so") 

(load "fonctions_jeu.scm")
(load "h1.scm")
(load "minimax.scm")
(load "alphabeta.scm")
(load "alphabetaAvecStrategieDebutFin.scm")

;; votre fichier joueur (qui doit avoir une fonction saisie-coup1)
(load "joueur1.scm") ; votre fichier joueur

;; fonction generale de saisie coup (fait jouer un joueur à la suite de l'autre)
(define (saisie-coup etat-jeu)
  (let (( joueur (etatdejeu-joueur etat-jeu)))
    (if (= joueur 1) 
        (saisie-coup-alphabeta etat-jeu 5) ;; votre fonction saisie-coup
        (saisie-coup-alphabetaStratege etat-jeu 5)))) ;; fonction saisie-coup de l'alpha beta


(define (joue etat-jeu I)  
  (if (< I 1500)
      (let (( joueur (etatdejeu-joueur etat-jeu)))
        (if (not (fin-jeu? etat-jeu joueur))
            (begin 
              (let ((coup (saisie-coup etat-jeu )))
               (newline)
               (display "coup ---")(display coup)(display "---- ")
                (if (coup-valide? coup etat-jeu joueur)
                    (begin 
                      (joue-coup etat-jeu coup)
                      (set-etatdejeu-joueur! etat-jeu (autre-joueur joueur))
                      (display-plateau  etat-jeu) 
                      (joue etat-jeu (+ I 1))
                      etat-jeu)
                (begin 
                 (display "coup invalide")
                 (newline)
                  (joue etat-jeu I)                         
                  ) )))
            (finalise-partie etat-jeu)))
      (finalise-partie etat-jeu)))

(define (jeu-deux-ordis)
  (let ((etat-jeu (initialise-jeu)))    
   (display-plateau etat-jeu)
    (joue etat-jeu 1)
    (newline)
    (retourne-gagnant etat-jeu)
    ))


(define v (jeu-deux-ordis))
;(define v (car r))
(if (= v 1)
    (display "Le joueur 1 gagne")
    (display "Le joueur 2 gagne") 
    )


