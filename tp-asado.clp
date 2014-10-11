(clear)
;;****************
;;* DEFGLOBAL *
;;****************

(defglobal
	?*resultado* = (create$ Usted debe comprar:))

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

 ;;Controla que el nro ingresado sea >= 0, devuelve el valor correcto sino FALSO
(deffunction test-cant-positiva (?x)
  (while (< ?x 0)
     (printout t "( >= 0): ")
     (bind ?x (read)))
	 (if (>= ?x 0) then ?x else FALSE))

;;Calcula 400g para los hombres y 300g para mujeres	 
(deffunction suma-cant-carne-vaca (?x ?y)
	(bind ?calculo-carne-vaca (/ (+ (* ?x 400) (* ?y 300)) 1000))
	(assert (cant-carne-vaca ?calculo-carne-vaca))
	(generar-resultado -Cantidad de carne-vaca necesaria: ?calculo-carne-vaca Kg))

;;Calcula 1kg de carbon por kg de carne (1kg-carbon/1kg-carne)	
(deffunction suma-cant-carbon (?gramos)
	(bind ?calculo-carbon (/ ?gramos 1000))
	(assert (cant-carbon ?calculo-carbon))
	(generar-resultado -Cantidad de carbon necesaria: ?calculo-carbon Kg))

;;650ml por hombre y 500 por mujer (gaseosa)
(deffunction suma-cant-bebida (?x ?y)
	(bind ?calculo-bebida (/ (+(* ?x 650)(* ?y 500)) 1000))
	(assert (cant-bebida ?calculo-bebida))
	(generar-resultado -Cantidad de gaseosa necesaria: ?calculo-bebida litros))

;;0.5kg de hielo cada litro de bebida
(deffunction suma-cant-hielo (?cant-bebida)
	(bind ?calculo-hielo (* ?cant-bebida 0.5))
	(assert (cant-hielo ?calculo-hielo))
	(generar-resultado -Cantidad de hielo necesario: ?calculo-hielo Kg))
	
;;1 tomate por persona	
(deffunction suma-cant-tomates (?cant-personas)
	(assert (cant-tomate ?cant-personas))
	(generar-resultado -Cantidad de tomate necesario: ?cant-personas unidades))

;;1 tira de pan cada 2 personas
(deffunction suma-cant-pan (?cant-personas)
	(bind ?calculo-pan (/ ?cant-personas 2))
	(assert (cant-pan ?calculo-pan))
	(generar-resultado -Cantidad de pan necesario: ?calculo-pan tiras)) 

		
;;**********************
;;* DEFTEMPLATES *
;;**********************

(deftemplate cant-hombres
	(slot cantidad 
		(type INTEGER)))
	
(deftemplate cant-mujeres
	(slot cantidad 
		(type INTEGER)))
		
(deftemplate tipo-ingesta
	(slot ingesta 
		(type SYMBOL)
		(allowed-symbols poco medio mucho)))

(deftemplate ingredientes
	(slot carne-vaca (type NUMBER))
	(slot carne-pollo (type NUMBER))
	(slot achuras (type NUMBER))
	(slot chorizo (type NUMBER))
	(slot moricilla (type NUMBER))
	(slot butifarra (type NUMBER))
	(slot carbon (type NUMBER))
	(slot gaseosa (type NUMBER))
	(slot tomate (type NUMBER))
	(slot lechuga (type NUMBER))
	(slot papa (type NUMBER))
	(slot huevos (type NUMBER))
	(slot pan (type NUMBER)))
	
;;**********************
;;* RULES *
;;**********************

;;Si no hay hombres, se verifica que haya al menos 1 mujer.
(defrule regla-test-mujeres
	(cant-hombres (cantidad 0))
	=>
	(printout t "Cuantas mujeres?: ")
	(bind ?y (read))
	(while (<= ?y 0)
     (printout t "( > 0): ")
     (bind ?y (read)))
	(assert (cant-mujeres (cantidad ?y))))

;;Pregunta la cantidad de hombres y/o mujeres.
(defrule regla-iniciar
	(initial-fact)
	=>
	(printout t "Cuantos hombres?: " )
	(bind ?dato (read))
	(bind ?cant-hombres (test-cant-positiva ?dato))
	(assert (cant-hombres (cantidad ?cant-hombres)))
	(if (> ?cant-hombres 0)
		then 
		(printout t "Cuantas mujeres?: " )
		(bind ?dato (read))
		(bind ?cant-mujeres (test-cant-positiva ?dato))
		(assert (cant-mujeres (cantidad ?cant-mujeres)))))	

;;Calcula la cant de carne de vaca dependiendo de la cant de personas.		
(defrule regla-preguntas
		(cant-hombres (cantidad ?x))
		(cant-mujeres (cantidad ?y))
		=>
		(suma-cant-carne-vaca ?x ?y)
		(if (eq (si-o-no "Con bebida? ") TRUE)
			then 
			(suma-cant-bebida ?x ?y))
		(if (eq (si-o-no "Con Verdura? ") TRUE)
			then 
			(suma-cant-tomates (+ ?x ?y)))
		(if (eq (si-o-no "Con Pan? ") TRUE)
			then 
			(suma-cant-pan (+ ?x ?y)))
		(assert (fin)))	

(defrule regla-cant-carbon 
	(cant-carne-vaca ?cant)
	=>
	(suma-cant-carbon ?cant))
	
(defrule regla-cant-hielo
	(cant-bebida ?cant)
	=>
	(suma-cant-hielo ?cant))
		
(defrule resultado
	(declare (salience -100))
	(fin)
	=>
	(printout t ?*resultado* crlf)
	(printout t "Fin del programa" crlf))	
	
	
	
	
