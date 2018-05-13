;; Estudiantes: Andrés Sánchez Gómez     
;;              Diego Velásquez Castro  
;;
;; Programa: balancea un árbol binario
;; Input: árbol binario ordenado
;; Output: árbol balanceado
;;
;;
;; Ejemplo de como correr balancear

(define tt0 '(1 ()
                 (2 ()
                   ( 3 ()
                      (5 4 6)))))

;;>(balancear tt0)
;;(3 (1 () 2) (5 4 6))


;; árbol
(define (árbol centro izq der)
  (cond ( (and (null? izq)
               (null? der))
          centro)
        ( else
          (list centro izq  der))))


;; Atomo
(define (atom? x)
    (not (list? x)))

;; raíz

(define (raíz arb)
    (cond ( (atom? arb)
            arb)
        (else
            (car arb))))
        
;; Hijo izquierdo

(define (hijo-izq arb)
    (cond ( (atom? arb)
            '())
        (else
            (cadr arb))))
        
        
;; Hijo derecho

(define (hijo-der arb)
    (cond ( (atom? arb)
            '())
        (else
            (caddr arb))))
;; Insertar un elemento      
(define (insertar ele arb)
  (cond ( (null? arb)
          (árbol ele '() '()))
        ( (<= ele (raíz arb))
          (árbol (raíz arb)
                 (insertar ele (hijo-izq arb))
                 (hijo-der arb)))
        ( (> ele (raíz arb))
          (árbol (raíz arb)
                 (hijo-izq arb)
                 (insertar ele (hijo-der arb))))))
                           

;;Encuentra el mayor en un árbol
(define (mayor arb)
    (cond ( (null? arb)
            #f)
        ( (null? (hijo-der arb))
          (raíz arb))
      ( else
        (mayor (hijo-der arb)))))

;;Elimina el elemento de una lista
(define (eliminar ele arb)
    (cond ( (null? arb)
    '())
    ( (< ele (raíz arb))
        (árbol (raíz arb)
        (eliminar ele (hijo-izq arb))
        (hijo-der arb)))
    ( (> ele (raíz arb))
        (árbol (raíz arb)
        (hijo-izq arb)
        (eliminar ele (hijo-der arb))))
    ( (and (null? (hijo-izq arb))
        (null? (hijo-der arb)))
            '())
    ( (null? (hijo-izq arb))
        (hijo-der arb))
    ( (null? (hijo-der arb))
        (hijo-izq arb))
    ( else
        (árbol (mayor (hijo-izq arb))
        (eliminar (mayor (hijo-izq arb))
        (hijo-izq arb))
        (hijo-der arb)))))


;; Devuelve el elemento de una lista
(define (elemento index lista)
  (cond ((null? lista)
         #f)
        ((equal? index 0)
         (car lista))
        (else
         (elemento (- index 1) (cdr lista))))) 
;; Devuelve el elemento central de una lista

(define (central lista)
  (cond ((even? (length lista))
         (elemento (-(quotient (length lista) 2) 1) lista))
        (else
         (elemento (quotient (length lista) 2) lista))))

;; Funcion que devuelve una lista con los elementos que estan antes de un elemento dado
(define (sec-izq ele lista)
  (cond ((or (null? lista)
             (equal? ele (car lista)))
         '())
        (else
         (cons (car lista) (sec-izq ele (cdr lista))))))

;; Funcion que devuelve una lista con los elementos que estan despues de un elemento dado


(define (sec-der ele lista)
  (cond ((null? lista)
         '())
        ((equal? ele (car lista))
         (cdr lista))
        (else
         (sec-der ele (cdr lista)))))
     

;;; Funcion balancear
(define (balancear arb)
    (balancear-aux (en-orden-list arb)))


(define (balancear-aux lista)
  (cond ((null? lista)
         '())
        (else
         (árbol (central lista)
                (balancear-aux (sec-izq (central lista) lista))
                (balancear-aux (sec-der (central lista) lista))))))



;; Devuelve una lista en orden del árbol
(define (en-orden-list arb)
  (cond ( (null? arb)
          '())
        ( else
          (append (en-orden-list (hijo-izq arb))
               (list (raíz arb))
                (en-orden-list (hijo-der arb))))))
    
    
;;;;;;; árbol pruebas


(define tt3
'(50 (17 (9 ()
(14 12 ()))
(23 19
()))
(76 (54 ()
(72 67
()))
())))

