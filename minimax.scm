(load "structureIA.scm")

(define (saisie-coup-minimax ej profondeur)
  (let (
        (joueur (etatdejeu-joueur ej))
        (racine (make-noeudIA ej 0 (cons 0 0) #t))
        )
  (cdr (minimax racine profondeur joueur))
          
 )
)

(define (minimax noeud profondeur joueur)
  (if (= profondeur 0)
      (evaluation-IA (noeudIA-etatdejeu noeud) (noeudIA-coup noeud))
      (begin 
        (let* (
               (ej (noeudIA-etatdejeu noeud))
               (listeDeCoups (liste-coups-valides ej (etatdejeu-joueur ej)))
               (listEval (list)) 
               )
          (if (pair? listeDeCoups)
              (begin
                (do ( (coup (car listeDeCoups)  (set! listeDeCoups (cdr listeDeCoups))  ))  
                  ( (not (pair? listeDeCoups)))  
                  (let* (
                         (coup (car listeDeCoups))
                         (ejCopy (copy-struct etatdejeu (noeudIA-etatdejeu noeud)))
                         (nouveauEj (joue-coup ejCopy coup))
                         (nouveauNoeud (make-noeudIA nouveauEj 0 coup #f))
                         )
                    (set! listEval (cons (minimax nouveauNoeud (- profondeur 1) joueur) listEval))
                    )
                  ) 
                (if (= (etatdejeu-joueur ej) joueur)
                    (begin
                      (if (noeudIA-root? noeud)
                          (max-list listEval)
                          (cons (car (max-list listEval)) (noeudIA-coup noeud))
                          )
                      )
                    (begin
                      (cons (car (min-list listEval)) (noeudIA-coup noeud))     
                      )
                    )
                )
              (evaluation-IA (noeudIA-etatdejeu noeud) (noeudIA-coup noeud))
              )
          )
        )
      )
  )
