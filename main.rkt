#lang racket

;Leemos matriz de datos para el carrusel 
(define (read-carousel-data file-path)
  (with-input-from-file file-path
    (lambda ()
      (define (loop result)
        (define row (read))
        (if (eof-object? row)
            (reverse result)
            (loop (cons row result))))
      (loop '()))))

(define carousel-data (read-carousel-data "data.txt"))

; Definimos el tamano de nuestro carrusel 
(define rows (length carousel-data))
(define columns (length (car carousel-data)))

; Funcion basura (Cuenta el total de elementos dentro del carrusel)
; (define total-slots (* rows columns))

; Inicializamos la posicion del carrusel
(define current-row 0)
(define current-col 0)

; Validamos que el carrusel sea del tamano minimo (20x5)
(define (validate-carousel-size carousel-data)
  (define (validate-rows data min-rows)
    (if (null? data)
        (>= min-rows 20)
        (validate-rows (cdr data) (+ min-rows 1))))
  
  (define (validate-columns row min-cols)
    (if (null? row)
        (>= min-cols 5)
        (validate-columns (cdr row) (+ min-cols 1))))

  (and (validate-rows carousel-data 0)
       (validate-columns (car carousel-data) 0)))

; Verificamos si lo es
(define valid-carousel-size? (validate-carousel-size carousel-data))

(unless valid-carousel-size?
  (error "Error: El tamaño mínimo tiene que ser de 20x5 (20 filas y 5 columnas)."))

; Retiramos la cantidad de producto especificada 
(define (withdraw-item data row col count)
  (cond
    [(= row 0) (cons (withdraw-item-helper (car data) col count) (cdr data))]
    [else (cons (car data) (withdraw-item (cdr data) (- row 1) col count))]))

(define (withdraw-item-helper row col count)
  (cond
    [(= col 0)
     (if (> (cadr (car row)) 0)
         (cons (list (caar row) (max 0 (- (cadr (car row)) count)) (caddr (car row))) (cdr row))
         (error "Error: No hay suficientes articulos"))]
    [else (cons (car row) (withdraw-item-helper (cdr row) (- col 1) count))]))

; Agregamos la cantidad de producto especificada
(define (add-item data row col count)
  (cond
    [(= row 0) (cons (add-item-helper (car data) col count) (cdr data))]
    [else (cons (car data) (add-item (cdr data) (- row 1) col count))]))

(define (add-item-helper row col count)
  (cond
    [(= col 0) (cons (list (caar row) (+ (cadr (car row)) count) (caddr (car row))) (cdr row))]
    [else (cons (car row) (add-item-helper (cdr row) (- col 1) count))]))

; Guardamos los nuevos valores dentro del archivo 
(define (write-carousel-data file-path data)
  (with-output-to-file file-path
    (lambda ()
      (define (write-rows rows)
        (unless (null? rows)
          (write (car rows))
          (newline)
          (write-rows (cdr rows))))
      (write-rows data))
    #:exists 'replace))

; Nos movemos por el carrusel y dependiendo de su posicion 
; Verificamos si podemos seguir moviendonos en esa direccion
(define (move command)
  (cond
    [(string=? command "up")
     (if (> current-row 0)
         (set! current-row (- current-row 1))
         (display "Error: No puedes subir mas, ya estas en la cima.\n"))]
    [(string=? command "down")
     (if (< current-row (- rows 1))
         (set! current-row (+ current-row 1))
         (display "Error: No puedes bajar mas, ya tocaste fondo.\n"))]
    [(string=? command "left")
     (if (> current-col 0)
         (set! current-col (- current-col 1))
         (display "Error: No puedes ir a la izquierda, ya chocaste.\n"))]
    [(string=? command "right")
     (if (< current-col (- columns 1))
         (set! current-col (+ current-col 1))
         (display "Error: No puedes ir a la derecha, ya chocaste.\n"))]
    [else (display "Error: Movimiento invalido\n")]))

; Despliegamos la informacion de la posicion en donde estamos
(define (display-product-info)
  (display (format "Producto: ~a\nCantidad: ~a\nPrecio: ~a\n"
                   (car (list-ref (list-ref carousel-data current-row) current-col))
                   (cadr (list-ref (list-ref carousel-data current-row) current-col))
                   (caddr (list-ref (list-ref carousel-data current-row) current-col)))))

; Para procesar nuestros comandos
(define (process-commands commands)
; Calcula movimientos para llegar a un objeto
  (define (move-to-item item)
    (define-values (row col) (find-item item carousel-data 0))
    (display (format "Numero de movimientos: ~a\n" (+ (abs (- current-row row)) (abs (- current-col col)))))
    (set! current-row row)
    (set! current-col col))

; Sistema de filtro para encontrar la posicion del producto en el carrusel
(define (find-item item data current-row)
  (cond
    [(null? data) (values -1 -1)]
    [(>= (find-item-in-row item (car data) 0) 0) (values current-row (find-item-in-row item (car data) 0))]
    [else (find-item item (cdr data) (+ current-row 1))]))

  (define (find-item-in-row item row current-col)
  (if (null? row) -1
      (if (string=? item (caar row))
          current-col
          (find-item-in-row item (cdr row) (+ current-col 1)))))
  ; Para seguir procesando nuestros comandos y soltar errores en caso de que sea necesario 
  (cond
    [(and (pair? commands) (string=? (car commands) "mover"))
     (begin
       (for-each move (cdr commands))
       (display-product-info))]
    [(and (pair? commands) (string=? (car commands) "agregar"))
     (if (= (length commands) 3)
         (begin
           (move-to-item (cadr commands))
           (set! carousel-data (add-item carousel-data current-row current-col (string->number (caddr commands)))))
         (set! carousel-data (add-item carousel-data current-row current-col (string->number (cadr commands)))))
     (write-carousel-data "data.txt" carousel-data)]
    [(and (pair? commands) (string=? (car commands) "remover"))
     (if (= (length commands) 3)
         (begin
           (move-to-item (cadr commands))
           (let ((current-items (cadr (list-ref (list-ref carousel-data current-row) current-col))))
             (if (>= current-items (string->number (caddr commands)))
                 (set! carousel-data (withdraw-item carousel-data current-row current-col (string->number (caddr commands))))
                 (begin
                   (set! carousel-data (withdraw-item carousel-data current-row current-col current-items))
                   (display (format "No hay suficientes articulos para completar la solicitud. Has removido ~a articulo(s).\n" current-items))))))
         (let ((current-items (cadr (list-ref (list-ref carousel-data current-row) current-col))))
           (if (>= current-items (string->number (cadr commands)))
               (set! carousel-data (withdraw-item carousel-data current-row current-col (string->number (cadr commands))))
               (begin
                 (set! carousel-data (withdraw-item carousel-data current-row current-col current-items))
                 (display (format "No hay suficientes articulos para completar la solicitud. Has removido ~a articulo(s).\n" current-items))))))
     (write-carousel-data "data.txt" carousel-data)]
    [else (display "Error: Comando invalido")]))
; Para despliegar el valor total de nuestro inventario
(define (display-total-value)
  (display (format "Valor total de inventario: $~a\n" (total-value carousel-data))))
; Calcula el valor total de nuestro inventario
(define (total-value data)
  (apply + (flatten (map (lambda (row) (map (lambda (item) (* (cadr item) (caddr item))) row)) data))))
; Despliega articulos con una cantidad menor o igual a 3
(define (display-low-stock-items)
  (display "Articulos con bajo nivel de inventario:\n")
  (display-low-stock carousel-data 0))
; Obtiene posicion de articulos con cantidad menor o igual a 3
(define (display-low-stock data row)
  (unless (null? data)
    (display-low-stock-in-row (car data) row 0)
    (display-low-stock (cdr data) (+ row 1))))
(define (display-low-stock-in-row row current-row col)
  (unless (null? row)
    (let* ([current-cell (car row)]
           [product-name (car current-cell)]
           [product-count (cadr current-cell)])
      (when (and (<= product-count 5) (> product-count 0))
        (display (format "~a (Fila ~a, Columna ~a) - Quedan: ~a\n" product-name (+ current-row 1) (+ col 1) product-count))))
    (display-low-stock-in-row (cdr row) current-row (+ col 1))))
; Lee los comandos de nuestro segundo input
(define (read-commands file-path)
  (with-input-from-file file-path
    (lambda ()
      (define (loop result)
        (define cmd (read))
        (if (eof-object? cmd)
            (reverse result)
            (loop (cons (map (lambda (x)
              (cond
                [(symbol? x) (symbol->string x)]
                [(number? x) (number->string x)]
                [else x])) cmd) result))))
      (loop '()))))
; Ejecuta comandos de nuestro segundo input
(define commands (read-commands "comandos.txt"))



(for-each process-commands commands)
; Despliega el valor total de nuestro inventario
(display-total-value)
; Despliega articulos con bajo stock de nuestro inventario
(display-low-stock-items)