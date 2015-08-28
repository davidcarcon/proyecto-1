Hola como estas!!!!


;Ejercicio 1
(define pendiente
  (lambda(x y x^ y^)
    (/ (- y y^)(- x x^))))

;Ejercicio 2
(define norma-r3
  (lambda(a b c)
    (sqrt(+(+(expt a 2)(expt b 2))(expt c 2)))))

;Ejercicio 3
(define kh-ms
  (lambda (x)
    (/(* x 1000)3600)))

;Ejercicio 4 
;Ocupamos una funcion auxiliar.
;Recibe dos enteros, checa si hay un dividor de x en el intervalo [d, sqrt(x)]
;Esto basandonos en el criterio de primalidad de la raiz cuadrada.
(define resuelve
   (lambda (x d)
      (if (<= d (sqrt x))
          (if (= (modulo x d) 0)
              #F
          (resuelve x (+ 1 d)))
      #T)))

(define primo?
  (lambda(x)
    (if(= x 2)
       #T
     (resuelve x 2))))

;Ejercicio 5
(define ackermann
  (lambda (m n)
     (if (and (>= m 0) (>= n 0))
       (cond
          [(= m 0) (+ n 1)] 
          [(and (> m 0) (= n 0)) (ackermann (- m 1) 1)]
          [(and (> m 0) (> n 0)) (ackermann (- m 1) (ackermann m (- n 1)))])
       "error: al menos uno de los parametros no es natural")))

;Ejercicio 6
(define mcd2
  (lambda (x y)
      (if (= (modulo x y) 0)
                 y
      (mcd2 y (modulo x y)))))

(define mcd
  (lambda (a b c)
    (if (and (and (> a 0) (> b 0)) (> c 0))
       (mcd2 a (mcd2 b c))
       "error: alguno de los parámetros nos es mayor que 0")))
;Ejercicio 7
(define filtro
  (lambda(lista funcion)
    (cond
      [(empty? lista)'()]
      [(funcion(car lista))(cons(car lista)(filtro(cdr lista)funcion))]
      [else(filtro(cdr lista)funcion)])))
      