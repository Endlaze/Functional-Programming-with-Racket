;; Integrantes: Andres Sanchez Gomez  
;;              Diego Velasquez Castro 

;; Natural recibe una lista de numeros positivos y se corre de la siguiente manera: (natural '(5 1 2 3 8 1 4 2))
;; el resulltado que muestra en pantalla es ((5) (1 2 3 8) (1 4) (2))

;;Mezclar recibe dos listas de numeros positivos, ambas deben estar ordenadas. Se corre de la siguiente manera: (mezclar '(1 2 5 6 7) '(3 4 8 9 10))
;; el resultado que se muestra en pantalla es el siguiente: (1 2 3 4 5 6 7 8 9 10)


;; Merge-sort recibe una lista de numeros positivos en cualquier orden. Se corre de la siguiente manera: (merge-sort '(7 6 5 2 1 10 9 8 4 3))
;; el resultado que se muestra en pantalla es el siguiente: (1 2 3 4 5 6 7 8 9 10)



;;;------------------------ Funcion natural ------------------------------ ;;

(define (natural lista)
  ;; -- -- -- -- Funcion natural auxiliar -- -- -- -- ;;
  (define (natural-aux lista temp nueva last)

    (cond ((null? lista)
           (append nueva (list temp)))
          ((<= last (car lista))
           (natural-aux (cdr lista)
                        (append  temp (list (car lista)))
                        nueva
                        (car lista)))
          (else
           (natural-aux lista
                        (list)
                        (append nueva (list temp)) 0))))

  ;; -- -- -- --- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ;;

  (natural-aux lista '() '() 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Funcion mezclar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (mezclar lista1 lista2)

  ;;-- -- -- -- Funcion mezclar auxiliar -- -- -- --;;
  (define (mezclar-aux lista1 lista2 newlist)
    (cond ((and (null? lista1) (null? lista2))
           (newlist))
          ((null? lista1)
           (append newlist lista2))
          ((null? lista2)
           (append newlist lista1))
          ((equal? (car lista1) (car lista2))
           (mezclar-aux (cdr lista1)
                        (cdr lista2)
                        (append newlist (list (car lista1)) (list (car lista1)))))
          ((< (car lista1) (car lista2))
           (mezclar-aux (cdr lista1)
                        lista2
                        (append newlist (list (car lista1)))))
          (else
           (mezclar-aux lista1
                        (cdr lista2)
                        (append newlist (list (car lista2)))))
          )
    )
  ;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ;;
  (mezclar-aux lista1 lista2 '()))


;;;;;;;;;;;;;;;;;;;;Funcion merge-sort ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-sort lista)
  ;; -- -- -- -- Funcion merge-sort auxiliar -- -- -- -- ;;
  (define (merge-aux listaux)
    (cond ((= (length listaux) 1)
           (car listaux))
          (else
           (merge-aux (append (list (mezclar (car listaux) (cadr listaux))) (cdr(cdr listaux)))))))

  ;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ;;
  (merge-aux (natural lista))

  )
 
;;--------------------------------------------------------------------------------------------------;;
  



