(load "structureIA.scm")

;(load "fonctions_jeu.scm")

(define (saisie-coup-alphabeta ej profondeur)
  (let (
        (joueur (etatdejeu-joueur ej))
        (racine (make-noeudIA ej 0 (cons 0 0) #t))
        )
  (cdr (alphabeta racine profondeur (cons -9999 0) (cons 9999 0) joueur))
  )
 )


(define (alphabeta noeud profondeur alpha beta joueur) 
  (if (= profondeur 0)
      (begin
        (evaluation-IA (noeudIA-etatdejeu noeud) (noeudIA-coup noeud))
        )
      (begin 
        (let* (
               (ej (noeudIA-etatdejeu noeud))
               (listeDeCoups (liste-coups-valides ej (etatdejeu-joueur ej)))
               (listEval (list))  
               )
          (if (pair? listeDeCoups)
              (begin
                (if (= (etatdejeu-joueur ej) joueur)
                  (begin
                      (do ( ( coup (car listeDeCoups) (set! listeDeCoups (cdr listeDeCoups))  ))  
                        ( (or (not (pair? listeDeCoups)) (<= (car beta) (car alpha) ) ) )  
                        (let* (
                               (coup (car listeDeCoups))
                               (ejCopy (copy-struct etatdejeu (noeudIA-etatdejeu noeud)))
                               (nouveauEj (joue-coup ejCopy coup))
                               )
                          (set-etatdejeu-joueur! nouveauEj (autre-joueur (etatdejeu-joueur ej)))
                          (let (
                                (nouveauNoeud (make-noeudIA nouveauEj 0 coup #f))
                                )
                          (set! listEval (cons (alphabetaMilieu nouveauNoeud (- profondeur 1) alpha beta joueur) listEval))
                          )
                            (set! alpha (max-list listEval))
                          )
                        )
                      (if (noeudIA-root? noeud) 
                          (begin
                            (display listEval)
                            (max-list listEval)
                          )
                          (cons (car alpha) (noeudIA-coup noeud))
                          )
                      )  
                    (begin
                      (do ( (coup (car listeDeCoups)  (set! listeDeCoups (cdr listeDeCoups))  ))  
                        ( (or (not (pair? listeDeCoups)) (<= (car beta) (car alpha)) ) )  
                        (let* (
                               (coup (car listeDeCoups))
                               (ejCopy (copy-struct etatdejeu (noeudIA-etatdejeu noeud)))
                               (nouveauEj (joue-coup ejCopy coup))
                               )
                          (set-etatdejeu-joueur! nouveauEj (autre-joueur (etatdejeu-joueur ej)))
                          (let (
                               (nouveauNoeud (make-noeudIA nouveauEj 0 coup #f))
                               )
                          (set! listEval (cons (alphabetaMilieu nouveauNoeud (- profondeur 1) alpha beta joueur) listEval))
                          (set! beta (min-list listEval))
                          )
                        )
                        )
                      (cons (car beta) (noeudIA-coup noeud))
                      )
                    )
                )
             (evaluation-IA (noeudIA-etatdejeu noeud) (noeudIA-coup noeud))
              )
          )
        )
      )
  )