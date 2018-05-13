;;Andres Sanchez Gomez 
;;Diego Velasquez Castro 

;; Se define la matriz que desee para luego efectuar el pivot.
(define mat1
 '((xx x1 x2 x3 x4 x5 x6 rhs)
   (f0 -2  1 -8  0  0  0  0)
   (f1  2 -4  6  1  0  0  4)
   (f2 -1  3  4  0  1  0  3)
   (f3  0  0  2  0  0  1  1)))
;;
;;
;;Luego, se procede a efectuar el pivot con la fila y columna que
;;se quieren 
;;
;;
;;
;;
;; 
;; > (pivot 'f3 'x3 mat)
;; ((xx x1 x2 x3 x4 x5 x6  rhs)
;;  (f0 -2  1  0  0  0  4   4 )
;;  (f1  2 -4  0  1  0 -3   1 )
;;  (f2 -1  3  0  0  1 -2   1 )
;;  (f3  0  0  1  0  0 1/2 1/2))


;; Pretty print (no funciona)
(define (atom? ele)
    (or (number? ele)
        (symbol? ele)
        (char? ele)))
    
(define (construir lista index)
    (cond ( (= (length lista) index)
            lista)
          ( (< (length lista) index)
            (construir (cons #\space lista)
            index))
          ( (> (length lista) index)
            (construir (cdr lista)
                        index))))
                    
(define (cat ele index)
    (let* ( (str (cond (number? ele)
                     (number->string ele)
                     (symbol->string ele)))
            (lista (string->list str))
            )
        (list->string (construir lista index))))
    
(define (pprint ele index)
    (cond ( (atom? ele)
            (display (cat ele index)))
          ( (null? ele)
            (newline))
    ( else
        (pprint (car ele) index)
        (pprint (cdr ele) index))))

;;;;;;;;;;;;; Pivot ;;;;;;;;;;;;;;;;;;


;; Funcion para sacar un elemento de una lista dado un indice
(define (elemento index lista)
 (cond ( (null? lista)
 #f)
 ( (equal? index 0)
 (car lista))
 ( else
 (elemento (- index 1) (cdr lista)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion para obtener una fila, recibe un identificador

;; Funcion para sacar un elemento de una lista dado un indice
(define (elemento index lista)
 (cond ( (null? lista)
 #f)
 ( (equal? index 0)
 (car lista))
 ( else
 (elemento (- index 1) (cdr lista)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion para obtener una fila, recibe un identificador

(define (get-fila matrix elem)
  (cond( (equal? (caar matrix) elem)
         (car matrix))
       (else
        (get-fila (cdr matrix) elem))))

;; Funcion para sacar la columna en la que se encuentra un elemento

(define (get-columna elemento lista)
  (cond ((equal? elemento (car lista))
         0)
        (else
         (+ 1 (get-columna elemento (cdr lista))))))

;;Función para multiplicar un numero por una lista
(define (multiplicar_lista num lista)
  (map (lambda (n) (* num n )) lista))
;;Funcion para sumar dos listas
(define (sumar_lista lista1 lista2)
  (map + lista1 lista2))

;; Funcion para realizar operaciones elementales

(define (op_elem fila numcolumna matriz)
  (cond ((null? matriz)
         '())
        ((equal? (caar matriz) 'xx)
         (cons (car matriz) (op_elem fila numcolumna (cdr matriz))))
        ((equal? (caar matriz ) (car fila))
         (cons (append(list (car fila)) (multiplicar_lista -1 (cdr fila))) (op_elem fila numcolumna (cdr matriz))))
     
         (else
             (cons (append (list (caar matriz)) (sumar_lista (multiplicar_lista (elemento numcolumna (car matriz)) (cdr fila)) (cdar matriz))) (op_elem fila numcolumna (cdr matriz)) ))))

;; Funcion para realizar el pivote
(define (pivot fila columna mat)
  (pivot-aux (get-fila mat fila) (get-columna columna (car mat)) mat))

;; Auxiliar pivote

(define (pivot-aux fila columna matriz)
    (op_elem(append (list (car fila)) (multiplicar_lista (/ 1 (* -1 (elemento columna fila))) (cdr fila)  )) columna matriz))
