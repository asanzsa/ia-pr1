;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname PracticaLab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;------------------------------------------
;
; Práctica 1 de Inteligencia Artificial
;
; Autores:    Álvaro Sanz Sanz
;             Mario Alejandro Abajo Gamarra
;             
;------------------------------------------

; Descripción del problema: 
; ------------------------
; Trabajando sobre el mapa de autopistas  de Australia de  la primera figura (las indicadas en color negro), 
; se trata de elaborar un programa  Lisp-Scheme que admita como entradas  los nombres de dos ciudades 
; (de las nombradas en el primer mapa), que representarán el  comienzo y el fin  de un viaje, calcule la 
; mejor ruta entre ellas.

; Recursos: 
; ------------------------
; Se proporcionan dos mapas (figuras 1 y 2) con el esquema de rutas, así como enlaces a paginarios web con 
; la información sobre distancias entre cada dos ciudades y sobre las coordenadas geográficas de estas.

; Se pide: 
; --------
; Hacer una implementación del algoritmo A* para este problema.

; Ejemplo de ejecución del programa:
; ---------------------------------
; i.e.- (encuentra_camino "Katherine" "Darwin")
; i.e.- (encuentra_camino "Perth" "Sydney")


; DATOS
; -----
; Tabla de Conexiones entre ciudades (Mapa carreteras).
; Distancias entre ciudades expresadas en km.
; Datos sacados de http://distancecalculator.globefeed.com/
(define mapa_carreteras (make-immutable-hash '(
("Darwin" (("Katherine" 311.72)))
("Katherine" (("Darwin" 311.72) ("Tennant Creek" 703.63) ("Halls Creek" 735.36)))
("Halls Creek" (("Katherine" 735.36) ("Port Hedland" 1830.25)))
("Port Hedland" (("Halls Creek" 1830.25) ("Perth" 1517.99)))
("Perth" (("Port Hedland" 1517.99) ("Norseman" 643.76)))
("Norseman" (("Perth" 643.76) ("Eucla" 772.59)))
("Eucla" (("Norseman" 772.59) ("Port Augusta" 968.86)))
("Port Augusta" (("Alice Springs" 1207.19) ("Adelaide" 323.52) ("Eucla" 968.86)))
("Alice Springs" (("Tennant Creek" 519.22) ("Port Augusta" 1207.19)))
("Tennant Creek" (("Katherine" 703.63) ("Mount Isa" 566.02) ("Alice Springs" 519.22)))
("Mount Isa" (("Tennant Creek" 566.02) ("Toowoomba" 1694.23)))
("Toowoomba" (("Mount Isa" 1694.23) ("Brisbane" 119.23) ("Goondiwindi" 225.87)))
("Goondiwindi" (("Toowoomba" 225.87) ("Narrandera" 775.93)))
("Narrandera" (("Goondiwindi" 775.93) ("Sydney" 504.62) ("Canberra" 278.46) ("Melbourne" 424.53) ("Adelaide" 834.43)))
("Melbourne" (("Adelaide" 751.33) ("Narrandera" 424.53) ("Canberra" 536.3) ("Sydney" 820.61)))
("Adelaide" (("Port Augusta" 323.52) ("Narrandera" 834.43) ("Melbourne" 751.33)))
("Canberra" (("Sydney" 284.04) ("Narrandera" 278.46) ("Melbourne" 536.3)))
("Sydney" (("Newcastle" 117.15) ("Canberra" 284.04) ("Melbourne" 820.61) ("Narrandera" 504.62)))
("Newcastle" (("Brisbane" 706.59) ("Sydney" 117.15)))
("Brisbane" (("Bundaberg" 347.38) ("Newcastle" 706.59) ("Toowoomba" 119.23)))
("Bundaberg" (("Rockhampton" 285.98) ("Brisbane" 347.38)))
("Rockhampton" (("Mackay" 324.36) ("Bundaberg" 285.98)))
("Mackay" (("Townsville" 376.77) ("Rockhampton" 324.36) ))
("Townsville" (("Cairns" 323.71) ("Mackay" 376.77)))
("Cairns" (("Townsville" 323.71)))
("Burnie" (("Launceston" 129.77)))
("Launceston" (("Burnie" 129.77) ("Hobart"  188.21)))
("Hobart" (("Launceston"  188.21)))
)))

; Tabla de posición terrestre de cada ciudad.
; Posición terrestre expresada en grados decimales.
; Datos sacados de http://www.latlong.net/
(define lat_long (make-immutable-hash '(
("Darwin" (-12.462827 130.841777))
("Katherine" (-14.464967 132.264256))
("Halls Creek" (-18.224055 127.668204))
("Port Hedland" (-20.311627 118.575258))
("Perth" (-31.953004 115.857469))
("Norseman" (-32.198568 121.781268))
("Eucla" (-31.677126 128.889304))
("Port Augusta" (-32.492440 137.762818))
("Alice Springs" (-23.700210 133.880611))
("Tennant Creek" (-19.645850 134.191246))
("Mount Isa" (-20.724705 139.497462))
("Toowoomba" (-27.564330 151.953987))
("Goondiwindi" (-28.547206 150.307452))
("Narrandera" (-34.747901 146.550364))
("Melbourne" (-37.814107 144.963280))
("Adelaide" (-34.928621 138.599959))
("Canberra" (-35.282000 149.128684))
("Sydney" (-33.867487 151.206990))
("Newcastle" (-32.926689 151.7789205))
("Brisbane" (-27.471011 153.023449))
("Bundaberg" (-24.864963 152.348653))
("Rockhampton" (-23.377915 150.510103))
("Mackay" (-21.141210 149.185625))
("Townsville" (-19.257622 146.817879))
("Cairns" (-16.920334 145.770860))
("Burnie" (-41.052465 145.906851))
("Launceston" (-41.426181 147.112468))
("Hobart" (-42.881903 147.323815))
)))


; FUNCIÓN PRINCIPAL
; -----------------
; Encuentra el camino entre dos ciudades Origen Destino.
; Origen : Ciudad inicial.
; Destino : Ciudad final.
; i.e.- (encuentra_camino "Katherine" "Darwin")
(define (encuentra_camino Origen Destino) 
  (if (false? (esPosibleCamino Origen Destino))
       (printf "(No es posible llegar desde ~a a ~a por carretera)" Origen Destino)
       (encuentra_camino_tras_comprobacion Origen Destino)
  )
)

; Comprueba si es un camino imposible.
; Origen : Ciudad inicial.
; Destino : Ciudad final.
; i.e.- (esPosibleCamino "Melbourne" "Burnie" )
(define (esPosibleCamino Origen Destino )
  (if (or (and( equal? Origen "Burnie") (or (equal? Destino "Burnie") (equal? Destino "Launceston") (equal? Destino "Hobart")))
          (and( equal? Origen "Hobart") (or (equal? Destino "Burnie") (equal? Destino "Launceston") (equal? Destino "Hobart")))
          (and( equal? Origen "Launceston") (or (equal? Destino "Burnie") (equal? Destino "Launceston") (equal? Destino "Hobart")))
          (and (and (not(equal? Origen "Burnie")) (not(equal? Origen "Launceston")) (not(equal? Origen "Hobart")))
               (and (not(equal? Destino "Burnie")) (not(equal? Destino "Launceston")) (not(equal? Destino "Hobart"))))
      ) #t #f
  )
)

; Encuentra el camino entre dos ciudades Origen Destino tras las comprobaciones presvias.
; Origen : Ciudad inicial.
; Destino : Ciudad final.
; i.e.- (encuentra_camino_tras_comprobacion "Katherine" "Darwin")
(define (encuentra_camino_tras_comprobacion Origen Destino) 
  (let ((Abiertos (list (generar_nodo_inicial Origen Destino))) ; Nodo Raiz/Origen + h(0km + distanciaAerea).
   )
  (busqueda_A_estrella Origen Destino Abiertos)
   )
)


; FUNCIONES
; ---------
; Función del Algoritmo busqueda A*.
; Origen : Estado inicial.
; Destino : Estado final.
; Sucesores : Lista de nodos temporales, o hijos del nodo examinado.
; Abiertos : Lista de nodos pendientes de evaluar.
; Mientras la lista Abiertos no esté vacia. Hacer.
;   ACTUAL = Escoger y eliminar el mejor nodo de la lista ABIERTOS. (El de menor distancia)
;   Evaluar ACTUAL:
;        Si es meta: imprimir RUTA y terminar.
;        Sino: Obtener los nodos de ACTUAL, calculando el valor del nodo con la función Heuristica (SUCESORES).
;   Añadir los SUCESORES a la lista de ABIERTOS.
; Presentar FALLO y terminar.
; i.e- (busqueda_A_estrella "Darwin" "Eucla" null '("Darwin" 0))
(define (busqueda_A_estrella Origen Destino Abiertos)
  (let ((actual (busca_menor Abiertos)))
    (if (empty? Abiertos)
        '("no hay camino") ; "camino: "
        (if (equal? (nombre_nodo actual) Destino)
            (printf "~a~n" (list-ref actual 3))
            (busqueda_A_estrella 
                  actual
                  Destino 
                  ;(aplicar_heuristica Destino (explorar_nodo actual) (borrar_elemento Abiertos actual))
                 (borrar_elemento (append (generar_nodos_sucesores (explorar_nodo actual) actual Destino '()) Abiertos) actual)
             )
        )
    )
  )
)

; Expande los hijos de un nodo padre (conexiones de una Ciudad).
; i.e.- (explorar_nodo "Darwin")
(define (explorar_nodo nodo)
  (hash-ref mapa_carreteras (nombre_nodo nodo))
)

; Aplica la función heurística a una lista de nodos.
; i.e.- (aplicar_heuristica "Katherine" (explorar_nodo '("Eucla" 0)) '())
(define (aplicar_heuristica Destino lista_nodos aux)
  (if (null? lista_nodos)
      aux
      (append (aplicar_heuristica Destino (cdr lista_nodos) (list (f_heuristica_nodo Destino (car lista_nodos)))) aux)
  )
)

; Calcula la heurística para un nodo dado.
; i.e.- (f_heuristica_nodo "Darwin" '("Eucla" 110))
(define (f_heuristica_nodo Destino nodo)
  (list (car nodo) (+ (car (cdr nodo)) (distanciaAerea Destino (car nodo))) (car (cdr nodo)))
)

; Genera el primer nodo o nodo inicial.
; i.e.- (generar_nodo_inicial "Darwin" "Katherine")
(define (generar_nodo_inicial origen meta)
  (list origen
        0
        (distanciaAerea origen meta)
        (list origen)
  )
)

; Genera los nodos sucesores de una ciudad.
(define (generar_nodos_sucesores nodos padre meta n)
  (if(null? nodos)
     n
     (generar_nodos_sucesores 
           (cdr nodos) 
           padre
           meta 
           (append (list (generar_nodo padre (car nodos) meta)) n))
  )
)

; Genera un nodo sucesor a partir del nodo padre y la meta.
; i.e.- (generar_nodo '("Darwin" 100 32 (list (list "una ciudad"))) '("Katherine" 110) "Tennant Creek")
; i.e.- (generar_nodo (list "Darwin" 100 32 (list (list "una ciudad"))) (list "Katherine" 110) "Tennant Creek")
; i.e.- (generar_nodo (list "Darwin" 100 32 (list "Darwin")) (list "Katherine" 110) "Tennant Creek")
(define (generar_nodo padre hijo meta)
  (list (nombre_nodo hijo)
         (+ (distancia_acumulada_nodo padre) (distancia_acumulada_nodo hijo))
         (distanciaAerea (nombre_nodo hijo) meta)
         (append (list-ref padre 3) '("-") (list (nombre_nodo hijo)))
  )
)

; Obtiene el nombre de un nodo.
; i.e.- (nombre_nodo '("b" 2))
(define (nombre_nodo nodo)
  (list-ref nodo 0)
)

; Obtiene la distancia heuristica de un nodo.
; i.e.- (distancia_heuristica_nodo '("b" 2))
(define (distancia_heuristica_nodo nodo)
  (+ (list-ref nodo 1) (list-ref nodo 2))
)

; Obtiene la distancia acumulada de un nodo.
; i.e.- (distancia_heuristica_nodo '("b" 2))
(define (distancia_acumulada_nodo nodo)
  (list-ref nodo 1)
)

; Busca el menor elemento de una lista.
; i.e.- (busca_menor '(("b" 2) ("c" 3) ("d" 4)))
(define (busca_menor tl)
  (encuentra_menor tl (car tl))
)

; Encuentra el menor elemento de una lista.
; i.e.- (encuentra_menor '(("b" 2 3) ("c" 3 2) ("d" 4 1)) '("d" 4 1))
(define (encuentra_menor tl b)
  (cond ((null? tl) b)
        ((< (distancia_heuristica_nodo (car tl)) (distancia_heuristica_nodo b))
            (encuentra_menor (cdr tl) (car tl)))
        (else (encuentra_menor (cdr tl) b)))
)

; Inserta un elemento a una lista.
; i.e.- (insertar_nodo '("a" 3) '(("b" 2) ("c" 3) ("d" 4)))
; i.e.- (insertar_nodo "a" '("b" "c" "d"))
(define (insertar_nodo nodo arbol) 
  (append arbol (list nodo))
)

; Borra un elemento de una lista.
; i.e.- (borrar_elemento '(("b" 1 1) ("c" 3 2) ("d" 4 3)) '("b" 1 1))
(define (borrar_elemento tl2 a)
      (cond ((null? tl2) (quote ()))
            ((equal? a (car tl2)) (cdr tl2))
            (else (cons (car tl2) (borrar_elemento (cdr tl2) a))))
)

; Encuentra la distancia aerea entre 2 ciudades dadas.
; i.e.- (distanciaAerea "Darwin" "Katherine")
(define (distanciaAerea Cit1 Cit2)
   ( let ( 
          (lat1 (covertirARadianes (list-ref (hash-ref lat_long Cit1) 0)))
          (lat2 (covertirARadianes (list-ref (hash-ref lat_long Cit2) 0)))
          (lng1 (covertirARadianes (list-ref (hash-ref lat_long Cit1) 1)))
          (lng2 (covertirARadianes (list-ref (hash-ref lat_long Cit2) 1)))
          )
      (convertirAKilometros (leyEsfericaDelCoseno (formulaDeHaversine (delta lat1 lat2) (delta lng1 lng2) (abs lat1) (abs lat2))))
   )
)

; Calcula la diferencia entre dos posiciones expresadas en radianes.
; i.e.- (delta 200 100)
(define (delta l1 l2)
  (if (= (sgn l1) (sgn l2))
      (abs (- (abs l2) (abs l1)))
      (+ (abs l2) (abs l1))
  )
)
  
; Convierte un angulo expresado en grados decimales a radianes.
; i.e.- (covertirARadianes 10)
(define (covertirARadianes d)
  (/ (* pi d) 180)
)

; Formula de Haversine para calcular la distancia entre dos coordenadas de la tierra.
; a = sin^2 (Δlat / 2) + [cos (lat1) x cos (lat2) x sin^2 (Δlong / 2)]
; i.e.- (formulaDeHaversine 10 10 1 1)
(define (formulaDeHaversine dlat dlong lat1 lat2)
  (+ (expt (sin (/ dlat 2)) 2) (* (cos lat1) (cos lat2) (expt (sin (/ dlong 2)) 2)))
)
  
; Formula de la Ley esférica del coseno.
; c = 2 x arctan (√ a / √ (1-a))
; i.e.- (leyEsfericaDelCoseno 10)
(define (leyEsfericaDelCoseno a)
  (* 2 (atan (/ (sqrt a) (sqrt (- 1 a)))))
)

; Convierte una distancia a Kilometros.
; d = R x c  ; R 6371 -> Radio de la tierra.
; i.e.- (convertirAKilometros 1)
(define (convertirAKilometros d)
  (* 6371 d)
)


; TEST
; ----
; i.e.- (encuentra_camino "Katherine" "Darwin")
; TEST 1:(Katherine - Darwin)
(printf "TEST 1:")
(define test1 (encuentra_camino "Katherine" "Darwin") )

; i.e.- (encuentra_camino "Darwin" "Eucla")
; TEST 2:(Darwin - Katherine - Tennant Creek - Alice Springs - Port Augusta - Eucla)
(printf "TEST 2:")
(define test2 (encuentra_camino "Darwin" "Eucla") )

; i.e.- (encuentra_camino "Katherine" "Darwin")
; TEST 3:Burnie - Launceston - Hobart)
(printf "TEST 3:")
(define test3 (encuentra_camino "Burnie" "Hobart") )

; i.e.- (encuentra_camino "Darwin" "Eucla")
; TEST 4:(Perth - Norseman - Eucla - Port Augusta - Adelaide - Narrandera - Goondiwindi - Toowoomba - Brisbane - Bundaberg - Rockhampton - Mackay - Townsville - Cairns)
(printf "TEST 4:")
(define test4 (encuentra_camino "Perth" "Cairns") )

; i.e.- (encuentra_camino "Port Hedland" "Canberra")
; TEST 5:(Port Hedland - Perth - Norseman - Eucla - Port Augusta - Adelaide - Narrandera - Canberra)
(printf "TEST 5:")
(define test5 (encuentra_camino "Port Hedland" "Canberra") )

; i.e.- (encuentra_camino "Norseman" "Burnie")
; TEST 6:(No es posible llegar desde Norseman a Burnie por carretera)
(printf "TEST 6:")
(define test6 (encuentra_camino "Norseman" "Burnie") )