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
(defun set-nth (l n val)
    (if (> n 0)
        (cons (car l)
            (set-nth (cdr l) (1- n) val))
        (cons val (cdr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                   VECTOR                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;prodotto interno
(defun innerprod (a b)
    (if (or (null a) (null b))
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
    (if (and (not (null a)) (not (null b)))
        (append (list (+ (car a) (car b)))
            (vsum (cdr a) (cdr b)))
        (or a b)))

;differenza
(defun vsub (a b)
    (if (and (not (null a)) (not (null b)))
        (append (list (- (car a) (car b)))
            (vsub (cdr a) (cdr b)))
        (or a b)))

;distanza
(defun distance (a b)
    (sqrt (apply '+ (lexp (vsub a b) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  KMEANS                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;init - metodo forgy
; a - lista vettori
; k - numero centroidi
(defun forgy (a k)
    (if (not (zerop k))
        (progn
            (let ( (e (nth (random (list-length a)) a)) )
                (append (list e)
                    (forgy (remove e a) (- k 1)))))))

;calcolo distanze di un punto dai centroidi
; v - vettore / observation
; l - lista centroidi
(defun distances (v l)
    (if (not (null l))
        (append (list (distance v (car l)))
            (distances v (cdr l)))))

;scelta del centroide più vicino
; d - lista distanze
; c - lista centroidi
(defun choose_centroid (d c)
    (nth (position (reduce #'min d) d) c))

;suddivisione delle osservazioni attorno ai centroidi a loro più vicini
; o - lista vettori / observation
; c - lista centroidi
(defun partition (o c)
    (if (not (null o))
        (append 
            (list (cons 
                (choose_centroid (distances (car o) c) c)
                (list (car o))))
            (partition (cdr o) c))))

;crea il cluster (la lista) con tutti gli elementi vicini al centroide 'c'
; c - centroide
; l - lista generata dal partition
(defun splitting (c l)
    (if (not (null l))
        (append (if (equal c (car (car l)))
            (list (second (car l))))
            (splitting c 
                (cdr l)))))

;converte la lista centroide+vettore in cluster
; c - lista centroidi
; l - lista generata dal partition
(defun split_item (c l)
    (if (not (null c))
        (append (list (splitting (car c) l))
            (split_item (cdr c) l))))

;calcolo centroide c->cluster
(defun compute_centroid (n c)
    (if (not (null c))
        (if (= (list-length c) 1)
            (ldiv (car c) n)
            (compute_centroid (+ n 1) 
                (remove 
                    ; rimuove v[1]
                    (second c)
                    ; mette la somma dei 2 vettori in v[0]
                    (set-nth c 0
                        ; somma v[0] con v[1]
                        (vsum (car c) (second c))))))))

;calcolo del centroide per ogni cluster
; cl - cluster
(defun centroid (cl)
    (if (not (null cl))
        (append (list (compute_centroid 1 (car cl)))
            (centroid (cdr cl)))))

;centroidi ottimali per i cluster
; ob - observations
; ce - lista centroidi
(defun compute_cluster (ob ce)
    (let ( (ncl (split_item ce (partition ob ce))) )
        (let ( (nce (centroid ncl)) )
            ; se il nuovo centroide è uguale al vecchio allora ho finito
            (if (not (equal nce ce))
                ; altrimenti computo ancora
                (compute_cluster ob nce)
                ncl))))

;km
; ob - observations
; k - numero cluster
(defun km (ob k)
    (if (> (list-length ob) k)
            (compute_cluster ob (forgy ob k))
            (error "Cluster number is greater than observations")))
