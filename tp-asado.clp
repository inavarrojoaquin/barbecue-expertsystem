;;;=====================================================================================================================
;;;   Programa que calcula las cantidades de ingredientes que se deben comprar a la hora de hacer un asado.
;;;
;;;     Se incluye:
;;;     -Tipos de carne
;;;		-Bebidas
;;;		-Ensaladas
;;;		-Pan
;;;		-Carbon
;;;		-Hielo
;;;     
;;;
;;;			Ejemplo CLIPS Version 6.3 
;;;
;;;     Para ejecutar, simplemente seleccione load, reset y run.
;;;     
;;;*** Explicacion de los pesos asignados a cada "corte" de carne ***
;;;*** Se les asigna este valor en la funcion opciones-de-carnes(), al momento de elegir un corte ***
;;;*** En base a este "peso" determinamos un porcentaje de cada "corte" por persona.
;;; Costilla	- peso=5
;;; Vacio 		- peso=4
;;; Pollo 		- peso=3
;;; Achuras 	- peso=2
;;; Chorizo 	- peso=1
;;; Morcilla	- peso=1
;;;			
;;; 1 -> 5/(5)
;;; 2 -> 5/(5+4) + 4/(5+4) = 100%
;;; 3 -> 5/(5+4+3) + 4/(5+4+3) + 3/(5+4+3) = 100%
;;; 4 -> 5/(5+4+3+2) + 4/(5+4+3+2) + 3/(5+4+3+2) + 2/(5+4+3+2) = 100%
;;; 5 -> 5/(5+4+3+2+1) + 4/(5+4+3+2+1) + 3/(5+4+3+2+1) + 2/(5+4+3+2+1) + 1/(5+4+3+2+1) = 100%
;;; 6 -> 5/(5+4+3+2+1+1) + 4/(5+4+3+2+1+1) + 3/(5+4+3+2+1+1) + 2/(5+4+3+2+1+1) + 1/(5+4+3+2+1+1) + 1/(5+4+3+2+1+1) = 100%
;;;========================================================================================================================

;;****************
;;* DEFGLOBAL *
;;****************

(defglobal
	?*cant-hombres* = -1
	?*cant-mujeres* = -1
	?*resultado* = (create$ Usted debe comprar:)
	?*nro-ensalada* = (create$)
	?*flag-nro-ensalada* = 0
	?*vector-ensalada* = (create$)
	?*nro-bebida* = (create$)
	?*flag-nro-bebida* = 0
	?*nro-corte-carne* = (create$)
	?*flag-nro-corte-carne* = 0
	?*suma-pesos* = 0
	?*vector-nom-peso* = (create$)
	?*total-carne* = 0)
	
;;**********************
;;* DEFTEMPLATES *
;;**********************

(deftemplate cant-hombres
	(slot cantidad 
		(type INTEGER)))
	
(deftemplate cant-mujeres
	(slot cantidad 
		(type INTEGER)))

(deftemplate corte-carne
	(slot nombre
		(type SYMBOL))
	(slot peso
		(type NUMBER)
		(range 0 10)
		(default 0))
	(slot cantidad
		(type NUMBER)
		(default 0)))
		
(deftemplate bebida
	(multislot nombre
		(type SYMBOL))
	(slot cantidad
		(type NUMBER)
		(default 0)))
		
(deftemplate ensalada
	(multislot nombre
		(type SYMBOL))
	(slot ingrediente
		(type SYMBOL)
		(default no))
	(slot cantidad 
		(type NUMBER)
		(default 0)))
		
(deftemplate pan
	(slot cantidad
		(type NUMBER)
		(default 0)))

;;****************
;;* DEFFUNCTIONS *
;;****************

;;Concatena informacion
(deffunction generar-resultado ($?informacion)
	(bind ?*resultado* ?*resultado* $?informacion))

;;Controla que la respuesta sea valida (si,s,no,n)
(deffunction si-o-no (?pregunta)
  (bind ?x bogus)
  (while (and (neq ?x si) (neq ?x s) (neq ?x no) (neq ?x n))
     (format t "%s(Si o No): " ?pregunta)
     (bind ?x (lowcase (sym-cat (read)))))
  (if (or (eq ?x si) (eq ?x s)) then TRUE else FALSE))

;;Brinda las opciones de cortes de carne.
(deffunction opciones-de-carnes ()
	(bind ?x bogus)
	(printout t "   Elija los cortes de carne..." crlf)
	(printout t "      Opciones: " crlf)
			(printout t "         Costilla.          (Marque 1)" crlf)
			(printout t "         Vacio.             (Marque 2)" crlf)
			(printout t "         Pollo.             (Marque 3)" crlf)
			(printout t "         Achuras.           (Marque 4)" crlf)
			(printout t "         Chorizo.           (Marque 5)" crlf)
			(printout t "         Morcilla.          (Marque 6)" crlf)
			(printout t "         Salir.             (Marque 7)" crlf)
			(printout t "      Opcion elegida: ")
			(bind ?respuesta (read))
			(if (eq (member$ ?respuesta (create$ ?*nro-corte-carne*)) FALSE)
				then
					(switch ?respuesta 
						(case 1
							then (printout t "      Corte de carne elegido: Costilla" crlf)
								 (assert (corte-carne (nombre costilla)(peso 5)))
								 (bind ?x 1))
						(case 2
							then (printout t "      Corte de carne elegido: Vacio" crlf)
								 (assert (corte-carne (nombre vacio)(peso 4)))
								 (bind ?x 2))
						(case 3
							then (printout t "      Corte de carne elegido: Pollo" crlf)
								 (assert (corte-carne (nombre pollo)(peso 3)))
								 (bind ?x 3))
						(case 4
							then (printout t "      Corte de carne elegido: Achuras" crlf)
								 (assert (corte-carne (nombre achuras)(peso 2)))
								 (bind ?x 4))
						(case 5
							then (printout t "      Corte de carne elegido: Chorizo" crlf)
								 (assert (corte-carne (nombre chorizo)(peso 1)))
								 (bind ?x 5))
						(case 6
							then (printout t "      Corte de carne elegido: Morcilla" crlf)
								 (assert (corte-carne (nombre morcilla)(peso 1)))
								 (bind ?x 6))
						(case 7
							then (if (= ?*flag-nro-corte-carne* 0)
									then (printout t "               Debe elegir al menos un corte de carne..." crlf)
										 (return (opciones-de-carnes))
									else (return)))
						(default (printout t "         Entrada no valida..." crlf)
								 (return (opciones-de-carnes))))
				else
					(printout t "      El numero elegido ya esta registrado; Elija otro numero" crlf)
					(return (opciones-de-carnes)))
	(if (neq ?x bogus) 
		then
			(bind ?*nro-corte-carne* (create$ ?*nro-corte-carne* ?x))
			(bind ?*flag-nro-corte-carne* (length (create$ ?*nro-corte-carne*))))
	(if (or (eq ?x 1) (eq ?x 2) (eq ?x 3) (eq ?x 4) (eq ?x 5) (eq ?x 6)) then TRUE else FALSE))
	
;;Brinda las opciones de bebidas, retorna TRUE si fue correcta la eleccion.
(deffunction opciones-bebidas ()
	(bind ?x bogus)
	(printout t "   Elija las bebidas..." crlf)
	(printout t "      Opciones: " crlf)
			(printout t "         Bebida sin alcohol   (Marque 1)" crlf)
			(printout t "         Fernet con Coca.     (Marque 2)" crlf)
			(printout t "         Vino.                (Marque 3)" crlf)
			(printout t "         Salir.               (Marque 4)" crlf)
			(printout t "      Opcion elegida: " )
			(bind ?respuesta (read))
			(if (eq (member$ ?respuesta (create$ ?*nro-bebida*)) FALSE)
				then
					(switch ?respuesta 
						(case 1
							then (printout t "      Bebida elegida: Bebida sin alcohol" crlf)
								 (assert (bebida (nombre gaseosa)))
								 (bind ?x 1))
						(case 2
							then (printout t "      Bebida elegida: Fernet con Coca" crlf)
								 (assert (bebida (nombre fernet-con-coca)))
								 (bind ?x 2))
						(case 3
							then (printout t "      Bebida elegida: Vino" crlf)
								 (assert (bebida (nombre vino)))
								 (bind ?x 3))
						(case 4
							then (return FALSE))
						(default (printout t "      Entrada no valida..." crlf)
								 (return (opciones-bebidas))))
				else
					(printout t "            El numero de bebida elegido ya esta registrado; Elija otro numero" crlf)
					(opciones-bebidas))
	(if (neq ?x bogus) 
		then
			(bind ?*nro-bebida* (create$ ?*nro-bebida* ?x))
			(bind ?*flag-nro-bebida* (length (create$ ?*nro-bebida*))))
	(if (or (eq ?x 1) (eq ?x 2) (eq ?x 3)) then TRUE else FALSE))
					
;;Brinda las opciones de ensaladas, retorna TRUE si fue correcta la eleccion.
(deffunction opciones-ensaladas ()
	(bind ?x bogus)
	(printout t "   Elija las ensaladas..." crlf)
	(printout t "      Opciones: " crlf)
			(printout t "         Ensalada de tomate y lechuga.          (Marque 1)" crlf)
			(printout t "         Ensalada de tomate y huevo.            (Marque 2)" crlf)
			(printout t "         Ensalada de papa y huevo.              (Marque 3)" crlf)
			(printout t "         Salir.                                 (Marque 4)" crlf)
			(printout t "      Opcion elegida: " )
			(bind ?respuesta (read))
			(if (eq (member$ ?respuesta (create$ ?*nro-ensalada*)) FALSE)
				then 
				(switch ?respuesta
					(case 1 
						then (printout t "      Ensalada elegida: Tomate y lechuga" crlf)
							 (assert (ensalada (nombre tomate-lechuga)))
							 (bind ?x 1))
					(case 2 
						then (printout t "      Ensalada elegida: Tomate y huevo" crlf)
							 (assert (ensalada (nombre tomate-huevo)))
							 (bind ?x 2))
					(case 3 
						then (printout t "      Ensalada elegida: Papa y huevo" crlf)
							 (assert (ensalada (nombre papa-huevo)))
							 (bind ?x 3))
					(case 4
							then (return FALSE))
					(default (printout t "      Entrada no valida..." crlf)
							 (return (opciones-ensaladas))))
				else
					(printout t "         El numero de ensalada elegido ya esta registrado; Elija otro numero" crlf)
					(return (opciones-ensaladas)))
	(if (neq ?x bogus) 
		then
			(bind ?*nro-ensalada* (create$ ?*nro-ensalada* ?x))
			(bind ?*flag-nro-ensalada* (length (create$ ?*nro-ensalada*))))
	(if (or (eq ?x 1) (eq ?x 2) (eq ?x 3)) then TRUE else FALSE))

;;Controla que el nro ingresado sea >= 0, devuelve el valor correcto sino FALSO
(deffunction test-cant-positiva (?x)
  (while (< ?x 0)
     (printout t "( >= 0): ")
     (bind ?x (read)))
	 (if (>= ?x 0) then ?x else FALSE))

;;genera un vector de pesos(atributo) y una suma que luego se utilizará para realizar calculos.
(deffunction generar-vector-nom-peso (?nom ?peso)
	(bind ?*suma-pesos* (+ ?*suma-pesos* ?peso))
	(bind ?*vector-nom-peso* (create$ ?*vector-nom-peso* ?nom ?peso)))

;;lee el vector extrayendo el nombre de carne elegido con un valor "peso" que se utilizara para calcular luego los gramos correspondientes
;;se calcula 500g para hombre y 400g para mujer de "carne", ese es el valor total sobre el cual se calculan porcentajes de cada tipo de carne.
(deffunction suma-cant-carne (?hombre ?mujer)
	(bind ?tam (length$ ?*vector-nom-peso*))
	(bind ?i 1)
	(while (<= ?i ?tam)
		(bind ?nom (nth$ ?i ?*vector-nom-peso*))
		(bind ?i (+ ?i 1))
		(bind ?porcentaje (/ (nth$ ?i ?*vector-nom-peso*) ?*suma-pesos*))
		(bind ?cant-carne (+ (* ?hombre (* ?porcentaje 500)) (* ?mujer (* ?porcentaje 400))))
		(generar-resultado - Cantidad de ?nom necesaria: ?cant-carne gramos)
		(bind ?*total-carne* (+ ?*total-carne* ?cant-carne))
		(bind ?i (+ ?i 1))))		
		
;;Calcula 1kg de carbon por kg de carne (1kg-carbon/1kg-carne)
(deffunction suma-cant-carbon ()
	(bind ?calculo-carbon ?*total-carne*)
	(generar-resultado - Cantidad de carbon necesaria: ?calculo-carbon gramos))

;;500cc pora hombre y mujer (gaseosa)
(deffunction suma-cant-gaseosa (?hombre ?mujer)
	(bind ?calculo-bebida (+ (* ?hombre 500) (* ?mujer 500)))
	(generar-resultado - Cantidad de bebida sin alcohol necesaria: ?calculo-bebida cc))
	
;; 1 medida de fernet = 50cc
;; coca-cola = 100cc
;; hielo = 3 cubitos
;; 1 fernet = 50cc + 100cc + 3cubitos hielo
;; Si suponemos que un hombre toma 5 vasos de fernet en 4hs y la mujer 4 vasos.
;; fernet para 1 hombre = 50cc x 5 = 250cc 
;; fernet para 1 mujer = 50cc x 4 = 200cc
;; coca para 1 homre = 100 x 5 = 500cc
;; coca para 1 mujer = 100 x 4 = 400cc

;;total fernet = (250cc x hombres) + (200cc x mujeres) = cant-fernet(cc)
;;total gaseosa = (500cc x hombres) + (400cc x mujeres) = cant-gaseosa(cc)
(deffunction suma-cant-fernet-con-coca (?hombres ?mujeres)
	(bind ?cant-fernet (+ (* 250 ?hombres) (* 200 ?mujeres)))
	(bind ?cant-gaseosa (+ (* 500 ?hombres) (* 400 ?mujeres)))
	(generar-resultado - Cantidad de fernet necesario: ?cant-fernet cc y Cantidad de gaseosa Coca-Cola para el fernet: ?cant-gaseosa cc))

;;750cc de vino por hombre y 375cc de vino por mujer.
(deffunction suma-cant-vino (?hombre ?mujer)
	(bind ?calculo-cant-vino (+ (* 750 ?hombre) (* 375 ?mujer)))
	(generar-resultado - Cantidad de vinos necesaria: ?calculo-cant-vino cc))
	
;;1kg de hielo por persona
(deffunction suma-cant-hielo (?personas)
	(bind ?calculo-hielo ?personas)
	(generar-resultado - Cantidad de hielo necesario: ?calculo-hielo Kg))
	
;;genera vector de ensaladas, solo para saber la cantidad que se seleccionaron-
(deffunction generar-vector-ensalada (?dir)
	(bind ?*vector-ensalada* (create$ ?*vector-ensalada* ?dir))
	(bind ?tam-vector (length$ ?*vector-ensalada*))
	(assert (vector-ensalada ?tam-vector)))
	
;;calcula 1 tomate mediano por persona y media planta chica de lechuga
;;divide el calculo total en la cantidad de ensaladas elegidas (?tam)
(deffunction suma-cant-ensalada-tomate-lechuga (?x ?y ?tam)
	(bind ?cant-tomate (/ (+ ?x ?y) ?tam))
	(bind ?cant-lechuga (/ (/ (+ ?x ?y) 2) ?tam))
	(generar-resultado - Cantidad de tomate necesario: ?cant-tomate medianos y Cantidad de lechuga necesaria: ?cant-lechuga plantas chicas))

;;calcula 1 tomate mediano por persona y medio huevo
;;divide el calculo total en la cantidad de ensaladas elegidas (?tam)
(deffunction suma-cant-ensalada-tomate-huevo (?x ?y ?tam)
	(bind ?cant-tomate (/ (+ ?x ?y) ?tam))
	(bind ?cant-huevo (/ (/ (+ ?x ?y) 2) ?tam))
	(generar-resultado - Cantidad de tomate necesario: ?cant-tomate medianos y Cantidad de huevos necesaria: ?cant-huevo unidades))

;;calcula 1 papa mediano por persona y medio huevo
;;divide el calculo total en la cantidad de ensaladas elegidas (?tam)
(deffunction suma-cant-ensalada-papa-huevo (?x ?y ?tam)
	(bind ?cant-papa (/ (+ ?x ?y) ?tam))
	(bind ?cant-huevo (/ (/ (+ ?x ?y) 2) ?tam))
	(generar-resultado - Cantidad de papas necesaria: ?cant-papa medianas y Cantidad de huevos necesaria: ?cant-huevo unidades))

;;1 tira de pan cada 3 personas
(deffunction suma-cant-pan (?cant-personas)
	(bind ?calculo-pan (/ ?cant-personas 3))
	(generar-resultado - Cantidad de pan necesario: ?calculo-pan tiras)) 

;;toma todos los resultados generados y los muestra uno abajo del otro	
(deffunction imprimir-todo ($?resultado)
	(bind ?fin (length$ ?resultado))
	(loop-for-count (?i 1 ?fin) do
		(bind ?aux1 (nth$ ?i ?resultado))
		(if (eq ?aux1 -)
			then
			(printout t "" crlf))
		(printout t ?aux1 " ")))

;;**********************
;;* RULES *
;;**********************

;;Pregunta la cantidad de hombres y/o mujeres.
(defrule regla-iniciar
	(initial-fact)
	=>
	(printout t "   Cuantos hombres?: " )
	(bind ?dato (read))
	(bind ?cant-hombres (test-cant-positiva ?dato))
	(assert (cant-hombres (cantidad ?cant-hombres)))
	(bind ?*cant-hombres* ?cant-hombres)
	(if (> ?cant-hombres 0)
		then 
		(printout t "   Cuantas mujeres?: " )
		(bind ?dato (read))
		(bind ?cant-mujeres (test-cant-positiva ?dato))
		(assert (cant-mujeres (cantidad ?cant-mujeres)))
		(bind ?*cant-mujeres* ?cant-mujeres)))	

;;Si no hay hombres, se verifica que haya al menos 1 mujer.
(defrule regla-test-mujeres
	(cant-hombres (cantidad 0))
	=>
	(printout t "   Cuantas mujeres?: ")
	(bind ?y (read))
	(while (<= ?y 0)
     (printout t "( > 0): ")
     (bind ?y (read)))
	(assert (cant-mujeres (cantidad ?y)))
	(bind ?*cant-mujeres* ?y))
		
;;Una vez que se cargan los datos de hombres y mujeres, se lanza esta regla que hace preguntas de lo que se desea comer.
(defrule regla-preguntas
		?dir1 <- (cant-hombres (cantidad ?x))
		?dir2 <- (cant-mujeres (cantidad ?y))
		=>
		(if (eq (opciones-de-carnes) TRUE)
			then
			(while (and (< ?*flag-nro-corte-carne* 6) (eq (si-o-no "      Desea elegir otro corte?: ") TRUE))
				(opciones-de-carnes)))
		(if (eq (si-o-no "   Con bebida? ") TRUE)
			then 
			(if (eq (opciones-bebidas) TRUE)
				then
				(while (and (< ?*flag-nro-bebida* 3) (eq (si-o-no "      Desea elegir otra bebida?: ") TRUE))
				(opciones-bebidas))))
		(if (eq (si-o-no "   Con ensalada: ") TRUE)
			then
			(if (eq (opciones-ensaladas) TRUE)
				then 
				(while (and (< ?*flag-nro-ensalada* 3) (eq (si-o-no "      Desea elegir otra ensalada?: ") TRUE))
				(opciones-ensaladas))))
		(if (eq (si-o-no "   Con Pan?: ") TRUE)
			then 
			(suma-cant-pan (+ ?x ?y)))
		(retract ?dir1)
		(retract ?dir2)
		(assert (fin)))

(defrule regla-generar-vector-carne
	(declare (salience -30))
	?x <- (corte-carne (nombre ?nom) (peso ?p) (cantidad ?cant))
	(test (= ?cant 0))
	=>
	(generar-vector-nom-peso ?nom ?p)
	(retract ?x)
	(assert (calculo-carne listo)))
	
(defrule regla-calculo-carne
	(declare (salience -50))
	?dir <- (calculo-carne listo)
	=>
	(suma-cant-carne ?*cant-hombres* ?*cant-mujeres*)
	(suma-cant-carbon)
	(retract ?dir))
	
(defrule regla-cant-bebida
	?dir <- (bebida (nombre ?nom) (cantidad ?cant))
	(test (= ?cant 0))
	=>
	(switch ?nom
		(case gaseosa
			then (suma-cant-gaseosa ?*cant-hombres* ?*cant-mujeres*))
		(case fernet-con-coca
			then (suma-cant-fernet-con-coca ?*cant-hombres* ?*cant-mujeres*))
		(case vino
			then (suma-cant-vino ?*cant-hombres* ?*cant-mujeres*))
		(default "      Se produjo un error" crlf))
	(suma-cant-hielo (+ ?*cant-hombres* ?*cant-mujeres*))
	(retract ?dir))
	
(defrule regla-generar-vector-ensalada
	?dir <- (ensalada (nombre ?nom)(ingrediente no))
	=>
	(generar-vector-ensalada ?dir))

(defrule regla-cant-ensalada 
	(declare (salience -20))
	?dir1 <- (ensalada (nombre ?nom)(ingrediente no))
	?dir2 <- (vector-ensalada ?tam)
	=>
	(switch ?nom
		(case tomate-lechuga
			then (suma-cant-ensalada-tomate-lechuga ?*cant-hombres* ?*cant-mujeres* ?tam))
		(case tomate-huevo
			then (suma-cant-ensalada-tomate-huevo ?*cant-hombres* ?*cant-mujeres* ?tam))
		(case papa-huevo
			then (suma-cant-ensalada-papa-huevo ?*cant-hombres* ?*cant-mujeres* ?tam))
		(default "   Se produjo un error" crlf))
	(retract ?dir1)
	(retract ?dir2))
	
(defrule resultado
	(declare (salience -100))
	(fin)
	=>
	(imprimir-todo ?*resultado*)
	(printout t "" crlf)
	(printout t "                  Fin del programa" crlf))
	
	
	
	
	
