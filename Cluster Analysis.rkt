;; Integrantes: Diego Velasquez Catro 2017107723
;;              Andres Sanchez Gomez  2017239278

;; Ejemplo de como correr promedio (promedio '(1 2 3) '(4 5 6) '(7 8 9)) (funciona con cantidad variable de listas)
;; Ejemplo de como correr distancia (distancia '(1 2 3) '(4 5 6)) (funciona con dos listas)

;;Ejemplo de como correr cluster  y cluster-hasta

;
;(define prueba'((e1) (1 1 1 1 1 0 0 1 1 1 1 1 1 0 0 1 1 0 0 1)
; (e2) (0 1 1 0 1 0 0 1 1 1 1 1 1 0 0 1 1 0 0 1)
; (e3) (0 0 0 1 0 0 1 1 0 1 1 1 1 0 0 1 1 0 0 1)
; (e4) (0 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0 1 1 1)
; ))

;(cluster prueba)

;(cluster-hasta 2 prueba)

;;--------------------------------------------------------------;;

;; Funcion para calcular la distancia de dos listas
(define (distancia v w)
   (sqrt (apply + (map (lambda(x y) (expt (- x y) 2)) v w))))


;; Funcion para calcular el promedio
(define (promedio . args)
  (prom-aux args (length   args)))

;; Funcion auxiliar de promedio
(define (prom-aux args largo)
    (cond ((null? (car args))
                  '())
              (else
             (cons (/ (apply + (map car args)) largo) (prom-aux (map cdr args) largo)))))
         
;; Funcion para multiplicar una lista segun la cantidad de elementos que tenga el identificador de la lista
(define (mul lista)
  (list (car lista) (map (lambda (x) (* x (length (car lista)))) (cadr lista))))


;; Funcion que une y saca el promedio de dos listas con sus identificadores respectivos
(define (unir l1 l2)
    (list (append (car l1) (car l2))
          (prom-aux (list (cadr (mul l1)) (cadr (mul l2))) (+ (length (car l1)) (length (car l2))))))


;; Funcion para encontrar los elementos m[as similares
(define (most-similar l1 l2  inputt dist)
    (cond((null? l1)
          (car dist))
         ((null? l2)
            (most-similar (cddr l1) inputt inputt dist))
        (else
            (cond ((or (= (cadr dist) 0) (and (> (distancia (cadr l1) (cadr l2)) 0) (< (distancia (cadr l1) (cadr l2)) (cadr dist))))
                     (most-similar l1 (cddr l2) inputt (list (list (car l1) (car l2)) (distancia (cadr l1) (cadr l2)))))
                   (else
                       (most-similar l1 (cddr l2) inputt dist))))))


;; Busca la lista de valores de los elementos para sacar el promedio
(define (busca-elemento ele inputt)
    (cond ((equal? ele (car inputt))
                   (list (car inputt) (cadr inputt)))
          (else
              (busca-elemento ele (cddr inputt))))) 
          

;; Funcion para eliminar un elemento dado un identificador                
(define (eliminar inputt elemento)
  (cond ((null? inputt) '())
        ((equal? (car inputt) elemento)
         (cddr inputt))
        (else
         (append (list (car inputt) (cadr inputt)) (eliminar (cddr inputt) elemento)))))
     
     
;; Funcion para eliminar los elementos agrupados   
(define (elim-elem inputt elementos)
    (cond ((null? elementos) inputt)
          (else
              (elim-elem (eliminar inputt (car elementos)) (cdr elementos)))))
    

;; Funcion cluster
(define (cluster inputt)
    (let (( similar (most-similar inputt inputt inputt '(() 0 ) ))) 
         
         (append (unir (busca-elemento (car similar) inputt) (busca-elemento (cadr similar) inputt)) 
                 (elim-elem inputt similar))))

;;Funcion cluster-hasta
             
(define (cluster-hasta num inputt)
    (cond ((or (= (* num 2) (length inputt)) (< (length inputt) (* num 2)) (= num 0) (null? inputt))
              inputt)
          (else
              (cluster-hasta num (cluster inputt)))))