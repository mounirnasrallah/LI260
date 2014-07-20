(define-struct noeudIA (etatdejeu evaluation coup root?))

(define (max-list l)
  (if (null? (cdr l))
      (car l)
      (let ((max-reste (max-list (cdr l)))
            (max-element (car l))
            )
        (if (> (car max-element) (car max-reste))
            max-element 
            max-reste
            ) 
        )
      )
  ) 


(define (min-list l)
  (if (null? (cdr l))
      (car l)
      (let ((min-reste (min-list (cdr l)))
            (min-element (car l))
            )
        (if (< (car min-element) (car min-reste))
            min-element 
            min-reste
            ) 
        )
      )
  ) 


(define (evaluation-IA ej coup)
  (let* (
         (scores (etatdejeu-scores ej))
        ; (coup (etatdejeu-coup ej))
         )
    (if (= (etatdejeu-joueur ej) 2) 
        (cons (- (car scores) (cdr scores)) coup)
        (cons (- (cdr scores) (car scores)) coup)
        )
    )
  )