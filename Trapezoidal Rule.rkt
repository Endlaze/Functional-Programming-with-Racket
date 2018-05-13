;; Miebros del grupo: Andres Sanchez Gomez
;;                    Diego Velasquez Castro


;; Condiciones:
;; - El parametro fun corresponde a una funcion
;; - El parametro p1 corresponde al primer parametro
;; - El parametro p2 corresponde al segundo parametro
;; - El parametro n es la cantidad de segmentos en los que se desea dividir la figura
;; - El calculo de la funcion dada en el pdf es: (trapecio fun 0.20 3.80 1000)= 0.8750964237519292

;; fu es la funcion definida en el pdf para la prueba en la tarea
(define(fu x)
  (* (exp (* -1 x)) x))

;;----------------------------------------------------------------


;; funcion de la Regla del Trapecio

(define (trapecio fun p1 p2 n)


  ;;------------funcion para calcular el la altura---------------

  (define (calc-h a b n)
    (/ (- b a) n))
  ;;---------------------------------------------------------------

  ;; funcion auxiliar

  (define (trapecio-aux fun p1 p2 h n suma aux)
    (cond ((zero? n)
           (* (+ suma (fun p1) (fun p2)) (/ h 2)))
          (else
           (trapecio-aux fun p1 p2 h (- n 1) (+ suma (* (fun aux) 2)) (+ aux h)))))
  ;;
  
  (trapecio-aux fun p1 p2 (calc-h p1 p2 n) (- n 1) 0 (+ p1 (calc-h p1 p2 n))))
  