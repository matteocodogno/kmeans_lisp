;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                           MEMBRI GRUPPO PROGETTO                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                              ;
;    NOME/COG:  Matteo Codogno       (730620)                                  ;
;                                                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    INIT                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    LIST                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; divide tutti gli elementi di una lista per una COSTANTE
(defun ldiv (a k)
    (if (not (null a))
        (append (list (/ (car a) k))
            (ldiv (cdr a) k))))

; calcola l'esponenziale di ogni elemento della lista
(defun lexp (a k)
    (if (not (null a))
        (append (list (expt (car a) k))
            (lexp (cdr a) k))))

; cambia l'n-esimo elemento di una lista
(defun set-nth (list n val)
    (if (> n 0)
        (cons (car list)
            (set-nth (cdr list) (1- n) val))
        (cons val (cdr list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                   VECTOR                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;prodotto interno
(defun innerprod (a b)
    (if (null a)
        0
        (+ (* (car a) (car b))
              (innerprod (cdr a) (cdr b)))))

;norma
(defun norm (a)
    (if (null a)
        0
        (sqrt (innerprod a a))))

;somma
(defun vsum (a b)
    (if (not (null a))
        (append (list (+ (car a) (car b)))
            (vsum (cdr a) (cdr b)))))

;differenza
(defun vsub (a b)
    (if (not (null a))
        (append (list (- (car a) (car b)))
            (vsub (cdr a) (cdr b)))))

;distanza
(defun distance (a b)
    (sqrt (apply '+ (lexp (vsub a b) 2))))
;(apply '+ v3) --> somma di una lista

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  KMEANS                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;init - metodo forgy
(defun forgy (a k)
    (if (not (zerop k))
        (progn
            (setf e (nth (random (list-length a)) a))
            (append (list e)
                (forgy (remove e a) (- k 1))))))

;calcolo distanze di un punto dai centroidi
(defun distances (v l)
    (if (not (null l))
        (append (list (distance v (car l)))
            (distances v (cdr l)))))

;scelta del centroide più vicino
(defun choose_centroid (d c)
    (nth (position (reduce #'min d) d) c))

;suddivisione delle osservazioni attorno ai centroidi a loro più vicini
(defun partition (o c)
    (if (not (null o))
        (append (append (list (choose_centroid (distances (car o) c) c)) (list (car o)))
            (partition (cdr o) c))))

;crea il cluster con tutti gli elementi vicini al centroide 'c'
(defun splitting (c l)
    (if (not (null l))
        (append (if (equal c (car l))
            (list (second l)))
            (splitting c (cdr (remove (second l) l))))))

;converte la lista centroide+vettore in cluster
(defun split_item (c l)
    (if (not (null c))
        (append (list (splitting (car c) l))
            (split_item (cdr c) l))))

;calcolo centroide c->cluster
(defun compute_centroid (n c)
    (if (not(null c))
        (if (= (list-length c) 1)
            (ldiv (car c) n)
            (compute_centroid (+ n 1) (remove (second c) (set-nth c 0 (vsum (car c) (second c))))))))

;calcolo del centroide per ogni cluster
(defun centroid (cl)
    (if (not (null cl))
        (append (list (compute_centroid 0 (car cl)))
            (centroid (cdr cl)))))

;centroidi ottimali per i cluster
(defun compute_cluster (ob ce)
    (setf ncl (split_item ce (partition ob ce)))
    (setf nce (centroid ncl))
    (if (not (equal nce ce))
        (compute_cluster ob nce)
        (print ncl)))

;km
(defun km (ob k)
    (if (> (list-length ob) k)
            (compute_cluster ob (forgy ob k))))
