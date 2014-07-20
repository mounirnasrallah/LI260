
(define (saisie-coup1 etat-jeu)
;  (display "ligne ?")
;  (let ((l (read-line)))
    (display "Colonne ?")
    (let* ((c (read-line))
           (coup (cons 1 (string->number c))))
      (display coup)
      (set-etatdejeu-coup! etat-jeu coup) 
      coup))

