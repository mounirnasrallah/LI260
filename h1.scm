(load "structureIA.scm")
(load-extension "Lib.so") 


(define (saisie-coup-horizonUn ej)
  (let (
        (joueur (etatdejeu-joueur ej))
        )
   (cdr (horizonUn ej))
  )
 )


(define (horizonUn ej)
  (let* (
        (j (etatdejeu-joueur ej))
        (listeDeCoups (liste-coups-valides ej j))
        (meilleurCoup (list 0 (cons 0 0))) 
        ) 
          (set! meilleurCoup (evaluation-IA (joue-coup (copy-struct etatdejeu ej) (car listeDeCoups)) (car listeDeCoups)) )
          (do ( (coup (car listeDeCoups)  (set! listeDeCoups (cdr listeDeCoups))  ))  
                       ( (not (pair? listeDeCoups))) 
                        (let* (
                               (coup (car listeDeCoups)) 
                               (copieEtatDeJeu (copy-struct etatdejeu ej)) 
                               (nouveauEj (joue-coup copieEtatDeJeu coup))
                               (eval (evaluation-IA nouveauEj coup))
                               )
                          (if (> (car eval) (car meilleurCoup)) 
                              (begin 
                                (set! meilleurCoup eval)
                                ) 
                              )
                          )
            )
          meilleurCoup
    )
  )