#lang racket/base

;; Estudiantes: Andrés Sánchez Gómez 
;;              Diego Velásquez Castro 

;;Nota: el programa se debe correr en "#lang racket"
;;      en el text-field CapturarFactorial sólo se pueden introducir números


;; Para iniciar el programa se debe invocar a la funcion "iniciar" de la siguiente manera: (iniciar)


;; Código de la tarea

(require
 framework
 racket/gui/base
 racket/class
 racket/list
 )

(provide QuizFactorialInterfaz-init FrameInterfaz)

(define (label-bitmap-proc l)
  (let ((label (first l)) (image? (second l)) (file (third l)))
    (or (and image?
             (or (and file
                      (let ((bmp (make-object bitmap% file 'unknown/mask)))
                        (and (send bmp ok?) bmp)))
                 "<Bad Image>"))
        label)))

(define (list->font l)
  (with-handlers
   ((exn:fail?
     (λ (e)
       (send/apply
        the-font-list
        find-or-create-font
        (cons (first l) (rest (rest l)))))))
   (send/apply the-font-list find-or-create-font l)))

(define QuizFactorialInterfaz #f)
(define FrameInterfaz #f)
(define Mensaje1 #f)
(define CapturarFactorial #f)
(define BotonCalcular #f)
(define IndicadorFactorial #f)
(define DisplayFactorial #f)
(define (QuizFactorialInterfaz-init
         #:CapturarFactorial-callback
         (CapturarFactorial-callback
          (lambda (text-field control-event) (void)))
         #:BotonCalcular-callback
         (BotonCalcular-callback (lambda (button control-event) (procedimiento)))
         #:DisplayFactorial-callback
         (DisplayFactorial-callback
          (lambda (text-field control-event) (void))))
  (set! FrameInterfaz
    (new
     frame%
     (parent QuizFactorialInterfaz)
     (label "Factorial")
     (width #f)
     (height #f)
     (x #f)
     (y #f)
     (style '())
     (enabled #t)
     (border 0)
     (spacing 0)
     (alignment (list 'center 'top))
     (min-width 700)
     (min-height 600)
     (stretchable-width #t)
     (stretchable-height #t)))
  (set! Mensaje1
    (new
     message%
     (parent FrameInterfaz)
     (label (label-bitmap-proc (list "Calculadora Factorial" #f #f)))
     (style '())
     (font
      (list->font (list 32 "Arial" 'default 'normal 'bold #f 'default #f)))
     (enabled #t)
     (vert-margin 20)
     (horiz-margin 10)
     (min-width 0)
     (min-height 0)
     (stretchable-width #f)
     (stretchable-height #f)
     (auto-resize #f)))
  (set! CapturarFactorial
    (new
     text-field%
     (parent FrameInterfaz)
     (label "Ingrese un número")
     (callback CapturarFactorial-callback)
     (init-value "")
     (style
      ((λ (l) (list* (first l) (second l) (third l)))
       (list 'single 'horizontal-label '())))
     (font
      (list->font (list 12 "Arial" 'default 'normal 'normal #f 'default #f)))
     (enabled #t)
     (vert-margin 10)
     (horiz-margin 10)
     (min-width 0)
     (min-height 0)
     (stretchable-width #f)
     (stretchable-height #f)))
  (set! BotonCalcular
    (new
     button%
     (parent FrameInterfaz)
     (label (label-bitmap-proc (list "Calcular" #f #f)))
     (callback BotonCalcular-callback)
     (style '())
     (font
      (list->font (list 13 "Arial" 'default 'normal 'normal #f 'default #f)))
     (enabled #t)
     (vert-margin 10)
     (horiz-margin 10)
     (min-width 0)
     (min-height 0)
     (stretchable-width #f)
     (stretchable-height #f)))
  (set! IndicadorFactorial
    (new
     message%
     (parent FrameInterfaz)
     (label (label-bitmap-proc (list "El factorial es: " #f #f)))
     (style '())
     (font
      (list->font (list 12 "Arial" 'default 'normal 'bold #f 'default #f)))
     (enabled #t)
     (vert-margin 10)
     (horiz-margin 10)
     (min-width 0)
     (min-height 0)
     (stretchable-width #f)
     (stretchable-height #f)
     (auto-resize #f)))
  (set! DisplayFactorial
    (new
     text-field%
     (parent FrameInterfaz)
     (label "")
     (callback DisplayFactorial-callback)
     (init-value "")
     (style
      ((λ (l) (list* (first l) (second l) (third l)))
       (list 'multiple 'horizontal-label '())))
     (font (list->font (list 8 #f 'default 'normal 'normal #f 'default #f)))
     (enabled #t)
     (vert-margin 20)
     (horiz-margin 20)
     (min-width 500)
     (min-height 300)
     (stretchable-width #f)
     (stretchable-height #f)))
  (send FrameInterfaz show #t))



;; Funcion factorial
(define (factorial n)
  (cond ((equal? n 0)
         1)
        (else
         (* n (factorial (- n 1))))))

;;Funcion para realizar el procedimiento de calcular el factorial
(define (procedimiento)
  (send DisplayFactorial set-value (number->string (factorial (string->number (send CapturarFactorial get-value))))))

;; Funcion para iniciar el programa
(define (iniciar)
  (QuizFactorialInterfaz-init))


