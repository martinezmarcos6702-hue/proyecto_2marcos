(defun promedio (lista)
  (if (null lista) 0
      (/ (reduce #'+ lista) (length lista))))

(defun varianza (lista)
  (let ((p (promedio lista)))
    (if (or (null lista) (<= (length lista) 1)) 0.0001
        (/ (reduce #'+ (mapcar (lambda (x) (expt (- x p) 2)) lista))
           (length lista)))))

(defun desviacion (lista)
  (sqrt (varianza lista)))

(defun gauss (x m d)
  (if (< d 0.00001) 0
      (/ (exp (/ (- (expt (- x m) 2)) (* 2 (expt d 2))))
         (* d (sqrt (* 2 3.141592))))))

(defparameter *dataset*
  '((25 500 1)
    (30 300 0)
    (22 400 1)
    (35 200 0)
    (28 350 1)))

(defun calcular-clase (e s c)
  (let* ((filas (remove-if-not (lambda (x) (= (third x) c)) *dataset*))
         (edades (mapcar #'first filas))
         (salarios (mapcar #'second filas))
         (m-e (promedio edades))
         (d-e (desviacion edades))
         (m-s (promedio salarios))
         (d-s (desviacion salarios))
         (priori (/ (length filas) (length *dataset*))))
    (* priori (gauss e m-e d-e) (gauss s m-s d-s))))

(defun principal (e s)
  (let ((p1 (calcular-clase e s 1))
        (p0 (calcular-clase e s 0)))
    (format t "Probabilidad Clase 1: ~f~%" p1)
    (format t "Probabilidad Clase 0: ~f~%" p0)
    (if (> p1 p0)
        (format t "Resultado: 1~%")
        (format t "Resultado: 0~%"))))

(principal 27 370)
