
(load-extension "Lib.so") ; 
(load "alphabetaAvecStrategieDebutFin.scm")
(load "alphabeta.scm")
(load "minimax.scm")
(load "h1.scm")
;; votre fichier joueur (qui doit avoir une fonction saisie-coup1)
;(load "joueur1.scm") ; votre fichier joueur

;; fonction generale de saisie coup (fait jouer un joueur à la suite de l'autre)
(define (saisie-coup etat-jeu)
  (let (( joueur (etatdejeu-joueur etat-jeu)))
    (if (= joueur 1) 
       (saisie-coup-alphabetaStratege etat-jeu 5);; votre fonction saisie-coup 
   (saisie-coup-minimax etat-jeu 5)))); (saisie-coup2 etat-jeu)))) ;; fonction saisie-coup de l'alpha beta


(define (joue etat-jeu I)  
  (if (< I 500)
      (let (( joueur (etatdejeu-joueur etat-jeu)))
        (if (not (fin-jeu? etat-jeu joueur))
            (begin
 
              (let ((coup (saisie-coup etat-jeu )))
               (newline) 
               (display coup)
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
  (let ((etat-jeu (initialise-jeu))
        (somme 0)
        (nb 0)
        )
    (set! nb (+ nb 1))
   (display-plateau  etat-jeu)
    (joue etat-jeu 1)
    (newline)
    (set! somme (+ somme (length (etatdejeu-coups-joues etat-jeu))))
    (display "nombre ")
    (display nb)
    (display " nombreCoups ")
    (display somme)
    (retourne-gagnant etat-jeu)
    ))

(define (test2joueurs N)
  (if (= N 0) (list 0 0)
      (let* ((vict (test2joueurs(- N 1)))
             (vict1 (car vict))
             (vict2 (cadr vict)))
        (if (= (jeu-deux-ordis) 1)
            (begin 
              (display " 1-")
            (list (+ 1 vict1) vict2))
            (begin
              (display " 2-") 
              (list vict1 (+ 1 vict2)))))))

(define N 1);; le nombre de parties que l'on fait jouer 
(define stats_2joueurs (test2joueurs N)) 
(display stats_2joueurs)
