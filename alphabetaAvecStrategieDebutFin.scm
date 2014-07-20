(load "structureIA.scm")
;(load "fonctions_jeu.scm")
(load-extension "Lib.so") 
 
 
(define (saisie-coup-alphabetaStratege ej profondeur)
  (let (
        (joueur (etatdejeu-joueur ej))
        (racine (make-noeudIA ej 0 (cons 0 0) #t))
        )
    (if (> 3 (nombreCoupJoue ej))
        (begin
          (cdr (strategieDebutDeJeu ej profondeur))
          )
        (begin
           (cdr (alphabetaMilieu racine profondeur (cons -9999 0) (cons 9999 0) joueur))
        )
    )
  )
  )


(define (tailleListe L)
  (if (pair? L)
      (+ 1 (tailleListe (cdr L)))
      0)
  )

(define (nombreCoupJoue ej)
  (tailleListe (etatdejeu-coups-joues ej))
  )

(define (compteConsecutif liste)
  (let (  
        (compteur 0)
        )
    (do ( (case (car liste) ( set! liste (cdr liste) )))
      ( (not (pair? liste)) )
      (let (
            (case (car liste))
            )
        (if (or (= case 1) (= case 2)) 
            (+ compteur case)
            )
        )
      )
    compteur
    )
  )

(define (comptePlus12 liste)
  (let (
        (compteur 0)
        )
    (do ( (case (car liste) ( set! liste (cdr liste) )))
      ( (not (pair? liste)) )
      (let (
            (case (car liste))
            )
        (if (and (> case 12) (> case (+ 11 (- (tailleListe liste) 1)) )) 
            (+ compteur (- case (+ 11 (- (tailleListe liste) 1)) ))
            )
        )
      )
    compteur
    )
  )

(define (strategiePlus12 ej)
  (let* (
        (plateau (etatdejeu-plateau ej))
        (plateauJoueurUn (inverserListe (car plateau) '()))
        (plateauJoueurDeux (car (cdr plateau)))
        )
    (if (= (etatdejeu-joueur ej) 1) 
        (- (comptePlus12 plateauJoueurUn) (comptePlus12 plateauJoueurDeux))
        (- (comptePlus12 plateauJoueurDeux) (comptePlus12 plateauJoueurUn))
        )
    )
  )

(define (inverserListe liste listeInverse)
  (if (pair? liste)
      (inverserListe (cdr liste) (cons (car liste) listeInverse))
      listeInverse
  )
 )


(define (strategieDeuxTroisConsecutif ej)
  (let* (
        (plateau (etatdejeu-plateau ej))
        (plateauJoueurUn (inverserListe (car plateau) '()))
        (plateauJoueurDeux (car (cdr plateau)))
        )
    (if (= (etatdejeu-joueur ej) 2) 
        (begin  
            (- (compteConsecutif plateauJoueurDeux) (compteConsecutif plateauJoueurUn))
          )
        (begin
            (- (compteConsecutif plateauJoueurUn) (compteConsecutif plateauJoueurDeux))
            )
        )
    )
  )

 
        
(define (evaluation-IA-stratege ej joueur)
  (let* (
         (scores (etatdejeu-scores ej))
         (coup (etatdejeu-coup ej))
         )
    (if (= joueur 1) 
        (cons (+ (+ (* (- (car scores) (cdr scores)) 3) (* (strategieDeuxTroisConsecutif ej) 1)) (* (strategiePlus12 ej) 1)) coup)
        (cons (+ (+ (* (- (cdr scores) (car scores)) 3) (* (strategieDeuxTroisConsecutif ej) 1)) (* (strategiePlus12 ej) 1)) coup)
        )
    )
  )

(define (evaluation-IA-findepartie noeud joueur)
  (let* (
         (ej (noeudIA-etatdejeu noeud))
         (scores (etatdejeu-scores ej))
         (coup (noeudIA-coup noeud))
         )
    (if (= (etatdejeu-joueur ej) 1) 
        (begin
          (let* (
                 (difference (- (car scores) (cdr scores)))
                 )
            (if (> difference 0)
                (cons (* difference 100) coup)
                (begin
                  (if (= difference 0)
                      (cons 0 coup)
                      (cons (* difference 100) coup)
            )
          )
          )))
            (begin
          (let* (
                 (difference (- (cdr scores) (car scores)))
                 )
            (if (> difference 0)
                (cons (* difference 100) coup)
                (begin
                  (if (= difference 0)
                      (cons 0 coup)
                      (cons (* difference 100) coup)
            ) 
          )
          )))
        )
    )
  )

  


(define (selectionerCoups list joueur)
  (if (pair? list)
      (begin 
        (let* (
              (element (car list))
              (coup (cdr element))
              )
          (if (= joueur (car element))
              (cons element (selectionerCoups (cdr list) joueur))
              (selectionerCoups (cdr list) joueur)
              )
          )
        )
      list
      )
  ) 

(define (eliminerCoupsVoisins list casePrecedente)
  (if (pair? list) 
      (begin
        (let* (
               (element (car list))
               (coup (cdr element))
               (caseDuCoup (cdr coup))
               )
          (if (or (= (+ casePrecedente 1) caseDuCoup) (= (- casePrecedente 1) caseDuCoup))
              (eliminerCoupsVoisins (cdr list) casePrecedente)
              (cons element (eliminerCoupsVoisins (cdr list) casePrecedente)) 
              )
          )
        )
     list
      )
  )

(define (dernier-coup liste)
  (if (pair? (cdr liste))
      (dernier-coup (cdr liste))
      (car liste)
  )
  )

(define (strategieDebutDeJeu ej profondeur)
  (let* (
         (joueur (etatdejeu-joueur ej))
         (listeDesCoupsJouee (etatdejeu-coups-joues ej))
         (listeDesCoupsDebut (selectionerCoups listeDesCoupsJouee joueur))
         (taille (tailleListe listeDesCoupsDebut))
         ) 
    
    (display listeDesCoupsJouee)
    (if (= taille 0)
        (begin 
          (let (
              (node (make-noeudIA ej 0 (cons 0 0) #t))
              )
            (alphabetaMilieu node profondeur (cons -99999 0) (cons 99999 0) joueur)
            )
          ) 
        (begin
          (let* (
                 (coupPrecedent (dernier-coup listeDesCoupsJouee))
                 (case (cdr coupPrecedent))
                 (node (make-noeudIA ej 0 (cons 0 0) #t)) 
                 (coupAJouerAB (alphabetaDebut node profondeur (cons -99999 0) (cons 99999 0) joueur) )
                 )
            (max-list (eliminerCoupsVoisins coupAJouerAB case))
            )
          )
        )
    )
  )


(define (alphabetaDebut noeud profondeur alpha beta joueur)
  (if (= profondeur 0)
      (evaluation-IA-stratege (noeudIA-etatdejeu noeud) joueur)
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
                      (do ( (coup (car listeDeCoups)  (set! listeDeCoups (cdr listeDeCoups))  ))  
                        ( (or (not (pair? listeDeCoups)) (<= (car beta) (car alpha) ) ) )  
                        (let* (
                               (coup (car listeDeCoups))
                               (ejCopy (copy-struct etatdejeu (noeudIA-etatdejeu noeud)))
                               (nouveauEj (joue-coup ejCopy coup))
                               (nouveauNoeud (make-noeudIA nouveauEj 0 coup #f))
                               )
                          (set! listEval (cons (alphabetaDebut nouveauNoeud (- profondeur 1) alpha beta joueur) listEval))
                          (set! alpha (max-list listEval))
                          )
                        )
                      (if (noeudIA-root? noeud) 
                          listEval
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
                               (nouveauNoeud (make-noeudIA nouveauEj 0 coup #f))
                               )
                          (set! listEval (cons (alphabetaDebut nouveauNoeud (- profondeur 1) alpha beta joueur) listEval))
                          (set! beta (min-list listEval))
                          )
                        )
                      (cons (car beta) (noeudIA-coup noeud))
                      )
                    )
                )
               (evaluation-IA-findepartie noeud joueur)
              )
          )
        )
      )
  )


(define (alphabetaMilieu noeud profondeur alpha beta joueur)
  (if (= profondeur 0)
      (begin
        (evaluation-IA-stratege (noeudIA-etatdejeu noeud) joueur)
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
                            (max-list listEval)
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
              (evaluation-IA-findepartie noeud joueur)
              )
          )
        )
      )
  )
    