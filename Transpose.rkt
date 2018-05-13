;; Integrantes: Andres Sanchez Gomez 
;;              Diego Velasquez Castro 

;; La funcion transpuesta recibe una matriz como parametro

;; Ejemplo de ejecucion: (define matrix '(( 1 2 3 4)( 5 6 7 8)( 9 10 11 12)(13 14 15 16)))
;; (transpuesta matrix)
;; La salida es: ((1 5 9 13) (2 6 10 14) (3 7 11 15) (4 8 12 16))

;;----------Funcion transpuesta -----------;;
(define (transpuesta mat)
  (cond((null? (car mat))
          '())
         (else
          (append (list(map car mat))
                  (transpuesta (map cdr mat))))))


