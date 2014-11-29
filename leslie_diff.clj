(declare diffadd diffsub diffmult diffdiv diffpow diffexp diffln simplifyadd simplifysub simplifymult simplifydiv simplifypow)
(defn diff [e x]
	(cond (number? e) 0
		  (symbol? e) (if (= e x) 1 0)
		  true (let [op (first e)
					 u (first (rest e))
					 v (if (= (count e) 3) (first (rest (rest e))) nil)]
					 (cond (= op '+)(diffadd u v x)
					 	   (= op '-)(diffsub u v x)
					 	   (= op '*)(diffmult u v x)
					 	   (= op '/)(diffdiv u v x)
					 	   (= op 'POW)(diffpow u v x)
					 	   (= op 'EXP)(diffexp u x)
					 	   (= op 'LN)(diffln u x)))))

(defn diffadd [u v x]
	(simplifyadd (diff u x) (diff v x)))

(defn diffsub [u v x]
	(simplifysub (diff u x) (diff v x)))

(defn diffmult [u v x]
	(simplifyadd (simplifymult u (diff v x)) (simplifymult v (diff u x))))

(defn diffdiv [u v x]
	(simplifydiv (simplifysub (simplifymult v (diff u x)) (simplifymult u (diff v x))) (simplifymult v v)))
 
(defn diffpow [u v x]
	(simplifyadd (simplifymult (simplifymult v (simplifypow u (simplifysub v 1))) (diff u x)) (simplifymult (simplifymult (simplifypow u v) (list 'LN u)) (diff v x))))

(defn diffexp [u x]
	(simplifymult (list 'EXP u) (diff u x)))

(defn diffln [u x]
	(simplifymult (simplifypow u (- 0 1)) (diff u x)))

(defn simplifyadd [x y]
	(cond (number? x) 
					(cond (number? y) (+ x y)
								(= x 0) y
								true (list '+ x y))
				(number? y) (if (= y 0) x)
				(symbol? x) 
					(cond (symbol? y) (if (= y x) (list '* 2 x) (list '+ x y))
								(list? y)
									(let [op (first y)
										one (first (rest y))
										two (if (= (count y) 3) (first (rest (rest y))) nil)]
										(if (= op '+) (simplifyadd (simplifyadd x one) two))
									)
								true (list '+ x y))
				true (list '+ x y)
	)
)

(defn simplifysub [x y]
	(cond (number? x)
					(cond (number? y) (- x y)
								(= x 0) (list '- y)
								true (list '- x y))
				(symbol? x) 
					(cond (symbol? y) (if (= y x) 0 (list '- x y))
								(number? y) (if (= y 0) x (list '- x y))
								true (list '- x y)
					)
			  true (list '- x y)
	)
)

(defn simplifymult [x y]
	(cond (= y 0) 0
				(= x 0) 0
				(= y 1) x
				(= x 1) y
				(symbol? x) 
					(cond (symbol? y) 
									(if (= x y) (list 'POW x 2) (list '* x y))
								(list? y)
									(let [op (first y)
									  base (first (rest y))
									  expo (first (rest (rest y)))]
									  (cond (= op 'POW)
									 					(if (= x base) (list 'POW x (simplifyadd expo 1)) (list '* x y))
								 					(= op '*)
								 						(list '* (list '* x base) expo)
							 						(= op '/)
							 							(list '/ (simplifymult x base) expo)
							 						true (list '* x y)
								 		)
								 	)
								 true (list '* x y)
					)
				(symbol? y) 
					(if (list? x)
						(let [op (first x)
						  base (first (rest x))
						  expo (first (rest (rest x)))]
						  (cond (= op 'POW)
						 		(if (= y base) (list 'POW y (simplifyadd expo 1)) (list '* x y))
					 		)
					 	)
						(list '* x y) ;else
				)
				(list? x)
					(cond (list? y)
									(let [opx (first x)
										basex (first (rest x))
										expox (first (rest (rest x)))
										opy (first y)
										basey (first (rest y))
										expoy (first (rest (rest y)))]
										(cond (= opx 'POW) 
											(cond (= opy 'POW)

														(= opy '*)

														true (list '* x y)
											)
													(= opx '*)
										)
										; LET THIS SERVE AS A REMINDER OF WHAT TO NEVER DO AGAIN
										; (if (= opx 'POW) ;never nest if statements omg
										; 	(if (= opy 'POW)
										; 		(if (= basex basey)
										; 			(list 'POW basex (simplifyadd expox expoy)) (list '* x y)) (list '* x y))

									 ;  (list '* x y))
									) ;let
								(symbol? y)
									(let [op (first x)
									  base (first (rest x))
									  expo (first (rest (rest x)))]
									  (cond (= op 'POW)
									 					(if (= y base) (list 'POW y (simplifyadd expo 1)) (list '* x y))
									 				(= op '*)
									 					(if (= y base) (list '* ) (list '* x y))
								 		)
					 				)
								true (list '* x y)) ;default for list? y
				true (list '* x y) ;default for cond
	)
)

(defn simplifydiv [x y]
	(cond (symbol? x)
				(cond (symbol? y) (if (= x y) 1)
							(list? y)
								(let [op (first y)
											base (first (rest y))
											pow (first (rest (rest y)))]
									(cond (= op 'POW)
													(if (= base x) (simplifydiv 1 (simplifypow base (simplifysub pow 1))) (list '/ x y)) ;check power simplification (ie (/ x (POW x 2)) )
												(= op '*)
													(if (= pow x) (simplifydiv 1 base) (list '/ x y)) ;check mult simplification
									)
								)
				)
				(number? x)
					(cond (= x 0) 0
								(number? y) (/ x y)
								true (list '/ x y)
					)
				
				(list? x)
					(if (list? y)
						(let [opx (first x)
							basex (first (rest x))
							expox (first (rest (rest x)))
							opy (first y)
							basey (first (rest y))
							expoy (first (rest (rest y)))]
							(if (= opx opy)
								(cond (= opx 'POW) ;check for POW lists, simplify
												(if (= basex basey) 
													(cond (> expox expoy) 
																	(simplifypow basex (simplifysub expox expoy))
																(> expoy expox)
																	(simplifydiv 1 (simplifypow basey (simplifysub expoy expox)))
																true (list '/ x y)
													)
													(list '/ x y)
												)
											(= opx '*) ;check for * lists, simplify
												(if (= expoy expox) (simplifydiv basex basey) (list '/ x y))
											true (list '/ x y)
								)
								(list '/ x y) ;else
							)
						)
						(list '/ x y) ;else for if list? y
					)
						
	)
)

(defn simplifypow [x y]
	(cond (= y 1) x
				(= y (- 0 1)) (simplifydiv 1 x)
				(= x 1) 1
				(= x 0) 0
				(= y 0) 1
				true (list 'POW x y) ;default return
	)
)