;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname PracticaLab1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;-----------------------------------------
; Práctica 1 de Inteligencia Artificial
;
; Autores:    Álvaro Sanz Sanz
;             *
;-----------------------------------------

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
; TODO


; Definimos todas las ciudades del mapa de Australia.
(define ciudades (list "Darwin" "Katherine" "Halls Creek" "Port Hedland" "PERTH" 
                       "Norseman" "Eucla" "Port Augusta" "Alice Springs" "Tennant Creek" 
                       "Mount Isa" "Toowoomba" "Goondiwindi" "Narrandera" "MELBOURNE" 
                       "ADELAIDE" "CANBERRA" "SYDNEY" "Newcastle" "BRISBANE" "Bundaberg" 
                       "Rockhampton" "Mackay" "Townsville" "Cairns" "Burnie" "Launceston" 
                       "HOBART"))

; Definimos las conexiones entre ciudades (Mapa carreteras).



; Definimos la posición terrestre expresada en grados decimales 
; de cada ciudad Australiana con interes para el problema.
; Datos sacados de http://www.latlong.net/
; alternativa (define ih (make-immutable-hash '(("hello"  5))))
(define lat_long (make-hash))
(hash-set! lat_long "Darwin" (list -12.462827 130.841777))
(hash-set! lat_long "Katherine" (list -14.464967 132.264256))
(hash-set! lat_long "Halls Creek" (list -18.224055 127.668204))
(hash-set! lat_long "Port Hedland" (list -20.311627 118.575258))
(hash-set! lat_long "PERTH" (list -31.953004 115.857469))
(hash-set! lat_long "Norseman" (list -32.198568 121.781268))
(hash-set! lat_long "Eucla" (list -31.677126 128.889304))
(hash-set! lat_long "Port Augusta" (list -32.492440 137.762818))
(hash-set! lat_long "Alice Springs" (list -23.700210 133.880611))
(hash-set! lat_long "Tennant Creek" (list -19.645850 134.191246))
(hash-set! lat_long "Mount Isa" (list -20.724705 139.497462))
(hash-set! lat_long "Toowoomba" (list -27.564330 151.953987))
(hash-set! lat_long "Goondiwindi" (list -28.547206 150.307452))
(hash-set! lat_long "Narrandera" (list -34.747901 146.550364))
(hash-set! lat_long "MELBOURNE" (list -37.814107 144.963280))
(hash-set! lat_long "ADELAIDE" (list -34.928621 138.599959))
(hash-set! lat_long "CANBERRA" (list -35.282000 149.128684))
(hash-set! lat_long "SYDNEY" (list -33.867487 151.206990))
(hash-set! lat_long "Newcastle" (list -32.926689 151.7789205))
(hash-set! lat_long "BRISBANE" (list -27.471011 153.023449))
(hash-set! lat_long "Bundaberg" (list -24.864963 152.348653))
(hash-set! lat_long "Rockhampton" (list -23.377915 150.510103))
(hash-set! lat_long "Mackay" (list -21.141210 149.185625))
(hash-set! lat_long "Townsville" (list -19.257622 146.817879))
(hash-set! lat_long "Cairns" (list -16.920334 145.770860))
(hash-set! lat_long "Burnie" (list -41.052465 145.906851))
(hash-set! lat_long "Launceston" (list -41.426181 147.112468))
(hash-set! lat_long "HOBART" (list -42.881903 147.323815))
(hash-set! lat_long "Los Angeles" (list 35.6850 139.7514))
(hash-set! lat_long "Tokyo" (list 34.052234 -118.243685))
(hash-set! lat_long "Madrid" (list 40.4167754 -3.7037902))
(hash-set! lat_long "Barcelona" (list 41.3850639 2.1734035))
(hash-set! lat_long "Zaragoza" (list 41.6487908 -0.8895811))
(hash-set! lat_long "Londres" (list 51.508515 -0.1254872))
(hash-set! lat_long "Edinburgo" (list 55.953252 -3.188267))


; Distancia Aerea entre 2 ciudades dadas.
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
(define (delta l1 l2)
  (if (= (sgn l1) (sgn l2))
      (abs (- (abs l2) (abs l1)))
      (+ (abs l2) (abs l1))
  )
)

; Convierte una angulo expresado en grados decimales a radianes.
(define (covertirARadianes d)
    (/ (* pi d) 180)
 )

; Formula de Haversine para calcular la distancia entre dos coordenadas de la tierra.
;a = sin^2 (Δlat / 2) + [cos (lat1) x cos (lat2) x sin^2 (Δlong / 2)]
(define (formulaDeHaversine dlat dlong lat1 lat2)
   (+ (expt (sin (/ dlat 2)) 2) (* (cos lat1) (cos lat2) (expt (sin (/ dlong 2)) 2)))
)
  
; Ley esférica del coseno ; c = 2 x arctan (√ a / √ (1-a))
(define (leyEsfericaDelCoseno a)
    (* 2 (atan (/ (sqrt a) (sqrt (- 1 a)))))
)

; Convierte a kilometros una distancia; d = R x c  ; R 6371 -> Radio de la tierra.
(define (convertirAKilometros d)
  (* 6371 d)
)



