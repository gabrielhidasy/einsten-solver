(make-package 'tpman2)
(in-package tpman2)
(common-lisp:use-package 'common-lisp)
(export 'solver)

;;Partes do solver
(defun presolve (constraints)
  "This function normalize the rules, making all rights lefts and
other adjustments"
  (let ((adjusted-constraints '()))
    (dolist (constraint constraints)
      ;;Transforma regras do tipo (= coisa1 ([+/-] coisa2 1)) na mais tradicional
      ;;(= ([+/-] coisa 1) coisa)
      (when (and (equal (car constraint) '=) (symbolp (cadr constraint)) (listp (caddr constraint)))
	(setf constraint (car (list `(= ,(caddr constraint) ,(cadr constraint))))))
      (cond
	;;Transforma regra do tipo (= casa coisa) em (= coisa casa)
	((and (equal (car constraint) '=) (numberp (cadr constraint)) (symbolp (caddr constraint)))
	 (setf constraint (car (list `(= ,(caddr constraint) ,(cadr constraint))))))
	;;Transforma toda regra > uma regra <
	((equal (car constraint) '>)
	 (setf constraint (list '< (caddr constraint) (cadr constraint))))
	;;Transforma toda regra (= (- coisa1 n) coisa2) em uma
	;;(= (+ coisa2 n) coisa1)
	((and (equal (car constraint) '=) (listp (cadr constraint)) (equal (caadr constraint) '-))
	 ;;(print constraint)
	 (let ((n (car (cddadr constraint))))
	   (setf constraint (car (list `(= (+ ,(caddr constraint) ,n) ,(cadadr constraint)))))))
	;;(print constraint))
	;;Resolver a |coisa1 + 1| = coisa2 -> (= (+ coisa1 1) coisa2)
	;;(print constraint)
	;(= (ABS (+ PEDRO 1)) EDUARDO) -> (= (+ PEDRO 1) EDUARDO)
	((and (equal (car constraint) '=) (listp (cadr constraint)) (equal (caadr constraint) 'ABS) (equal (car (cadadr constraint)) '+))
	 (setf constraint `(= ,(cadadr constraint) ,(caddr constraint))))
	 ;(print (cadr constraint)))
	;;TODO
	;;Resolver a Coisa + n1 = n2 -> é um Coisa = n2-n1 (que vc pode resolver sozinho)
	;;(= (+ florianopolis 1) 3)
	((and (equal (car constraint) '=) (listp (cadr constraint)) (equal (caadr constraint) '+) (symbolp (cadadr constraint)) (numberp (caddr constraint)))
	 (setf constraint `(= ,(cadadr constraint) ,(- (caddr constraint) (car (cddadr constraint)))))
	 )
	((and (equal (car constraint) '=) (listp (cadr constraint)) (equal (caadr constraint) '-) (symbolp (cadadr constraint)) (numberp (caddr constraint)))
	 (setf constraint `(= ,(cadadr constraint) ,(+ (caddr constraint) (car (cddadr constraint)))))
	 )
	(T T))
      (setf adjusted-constraints (append adjusted-constraints (list constraint))))
    adjusted-constraints))

(defun smart-apply0 (constraints)
  "This function should be run one time only, it applys and removes (=
thing n) rules, or rules, and so on"
  (let ((adjusted-constraints '()))
    (dolist (constraint constraints)
      (cond
	;;Elimina regras do tipo (= thing n)
	;;Ao fixar a regra no campo ela se torna desnecessaria para
	;;todas as iteracoes, como essa roda antes de tudo e não erra
	;;nao preciso reverter nunca
	((and (equal (car constraint) '=) (symbolp (cadr constraint)) (numberp (caddr constraint)))
	 (let ((domain-setted (cdr (assoc (cadr constraint) domain-hash)))
	       (domain-element (cdr (assoc (cadr constraint) domain-item-hash)))
	       (house-setted (1- (caddr constraint))))
	   (dotimes (x nhouses)
	     (setf (aref field x  domain-setted domain-element) 0))
	   (dotimes (x nhouses)
	     (setf (aref field house-setted domain-setted x) 0))
	   (setf (aref field house-setted domain-setted domain-element) 1)))
	
	;;Elimina regras do tipo (OR (= coisa n) (= coisa n2))
	;;Assim como as regras anteriores elas são fixas no campo
	((and (equal (car constraint) 'OR) (numberp (car (cddadr constraint))))
	 (let ((domain-setted (cdr (assoc (cadadr constraint) domain-hash)))
	       (domain-element (cdr (assoc (cadadr constraint) domain-item-hash)))
	       (house-setted-1 (1- (car (cddadr constraint))))
	       (house-setted-2 (1- (cadr (cdaddr constraint)))))
	   (dotimes (x nhouses)
	     (setf (aref field x domain-setted domain-element) 0))
	   (setf (aref field house-setted-1 domain-setted domain-element) 1)
	   (setf (aref field house-setted-2 domain-setted domain-element) 1)))
	;;Se A < B, A não pode estar na ultima casa e B não pode estar
	;;na primeira
	((and (equal (car constraint) '<))
	 (let ((attr-1-domain (cdr (assoc (cadr constraint) domain-hash)))
	       (attr-2-domain (cdr (assoc (caddr constraint) domain-hash)))
	       (attr-1-item (cdr (assoc (cadr constraint) domain-item-hash)))
	       (attr-2-item (cdr (assoc (caddr constraint) domain-item-hash))))
	   (setf (aref field (1- nhouses) attr-1-domain attr-1-item) 0)
	   (setf (aref field 0 attr-2-domain attr-2-item) 0))
	 ;;Essa regra ainda não pode ser eliminada
	 (setf adjusted-constraints (append adjusted-constraints (list constraint))))
	
	;;Se (A + n) = B, A não pode estar nas ultimas n casas e B não pode estar
	;;nas n primeiras
	((and (equal (car constraint) '=) (listp (cadr constraint)) (equal '+ (caadr constraint)))
	 ;;TODO Ve nos casos p1 p2 e p3, talvez vc precise tratar mais aqui
	 (let ((attr-1-domain (cdr (assoc (cadadr constraint) domain-hash)))
	       (attr-2-domain (cdr (assoc (caddr constraint) domain-hash)))
	       (attr-1-item (cdr (assoc (cadadr constraint) domain-item-hash)))
	       (attr-2-item (cdr (assoc (caddr constraint) domain-item-hash)))
	       (n (car (cddadr constraint))))
	   (dotimes (x n)
	     (setf (aref field x attr-2-domain attr-2-item) 0)
	     (setf (aref field (- nhouses (1+ x)) attr-1-domain attr-1-item) 0)))

	 ;;Essa regra ainda não pode ser eliminada
	 (setf adjusted-constraints (append adjusted-constraints (list constraint))))
	(T
	 ;(print constraint)
	 (setf adjusted-constraints (append adjusted-constraints (list constraint))))))
    adjusted-constraints))

(defun fix-field ()
  "when an item can only be in houses a, house a will have only this item"					
  (dotimes (x nhouses)
  ;This loop parses all columns and when one column has only one '1'
  ;the line in it has only one 1 too
    (dotimes (y ndomains)
      (let ((counter-1 0) (pos-1 -1) (counter-2 0) (pos-2 -1))
	(dotimes (z nhouses)
	  (when (= 1 (aref field x y z))
	    (setf counter-2 (1+ counter-2))
	    (setf pos-2 z)
	    )
	  (when (= 1 (aref field z y x))
	    (setf counter-1 (1+ counter-1))
	    (setf pos-1 z)
	    ))
      (when (= 1 counter-1)
	(dotimes (z nhouses)
	  (setf (aref field pos-1 y z) 0))
	(setf (aref field pos-1 y x) 1))
      (when (= 1 counter-2)
	  (dotimes (z nhouses)
	    (setf (aref field z y pos-2) 0))
	  (setf (aref field x y pos-2) 1))
      ))))

;;Muito a trabalhar aqui
(defun smart-apply1 (constraints on-brutus)
  (setf smartcals (1+ smartcals))
  (let ((adjusted-constraints '()))
    (dolist (constraint constraints)
      
      (let ((nsols (generate-field-aux)))
	(when (= nsols 0) ;There are no solutions this path
	    (return))
	;;makes a lot of diference in the extra ones:
	;;if the rules determine a solution before parsing all but not on brutus
	(when (and (not on-brutus) (= (generate-field-aux) 1)) 
	  (return)))
      (cond
	;;Tenta resolver as regras do tipo (= thing thing)
	((and (equal (car constraint) '=) (symbolp (cadr constraint)) (symbolp (caddr constraint)))
	 ;;Se um dos lados esta definido define o outro e nem poe essa regra mais
	 ;(print constraint)
	 (let* ((att-1 (cadr constraint))
		(att-1-domain (cdr (assoc att-1 domain-hash)))
		(att-1-item (cdr (assoc att-1 domain-item-hash)))
		(att-2 (caddr constraint))
		(att-2-domain (cdr (assoc att-2 domain-hash)))
		(att-2-item (cdr (assoc att-2 domain-item-hash))))
	   ;;Itera no atributo 1, onde ele for 0 o 2 é 0 tbm
	   ;;Se ele só for 1 em um precisa readicionar a regra
	   (let ((flag 0))
	     (let ((count 0))
	       (dotimes (x nhouses)
		 (if (= 0 (aref field x att-1-domain att-1-item))
		     (setf (aref field x att-2-domain att-2-item) 0)
		     (setf count (1+ count))
		     )
		 )
	       (when (= count 1)
		 (setf flag 1)))
	     ;;E vice versa
	     (let ((count 0))
	       (dotimes (x nhouses)
		 (if (= 0 (aref field x att-2-domain att-2-item))
		     (setf (aref field x att-1-domain att-1-item) 0)
		     (setf count (1+ count)))
		 )
	       (when (= count 1)
		 (setf flag 1)))
	     (when (= flag 0)
	       (setf adjusted-constraints (append adjusted-constraints (list constraint)))))))
	
	;;Resolver as regras do tipo (= (+ coisa n) coisa 2) -> se uma está definida não precisa
	;;Definir a outra
	((and (equal (car constraint) '=) (listp (cadr constraint)) (equal '+ (caadr constraint)))
	 (let* ((att-1 (cadadr constraint))
		(att-1-domain (cdr (assoc att-1 domain-hash)))
		(att-1-item (cdr (assoc att-1 domain-item-hash)))
		(att-2 (caddr constraint))
		(att-2-domain (cdr (assoc att-2 domain-hash)))
		(att-2-item (cdr (assoc att-2 domain-item-hash)))
		(n (car (cddadr constraint))))
	   ;;Como na outra, se um dos atributos só é 1 em uma casa podemos eliminar a regra
	   (let ((flag 0))
	     (let ((count 0))
	       ;;Se o atributo 1 é 0 na posicao k, o atributo 2 e 0 na k+n
	       ;;Nem preciso olhar a ultima casa
	       (dotimes (x (- nhouses n))
		 (when (= 0 (aref field x att-1-domain att-1-item))
		   (setf (aref field (+ x n) att-2-domain att-2-item) 0)
		   (setf count (1+ count))))
	       (if (= count (- nhouses 2)) (setf flag 1) t))
	     (let ((count 0))
	       ;;Se o atributo 2 e 0 na posicao k, o atributo 1 e 0 na n-k
	       ;;Nao preciso olhar a primeira casa
	       (dotimes (x nhouses)
		 (when (and (>= x n) (= 0 (aref field x att-2-domain att-2-item)))
		   (setf (aref field (- x n) att-1-domain att-1-item) 0)
		   (setf count (1+ count))))
	       (if (= count (- nhouses 2)) (setf flag 1) t))
	     (when (= flag 0)
	       (setf adjusted-constraints (append adjusted-constraints (list constraint)))))))

	
	;;Resolve as regras do tipo (< coisa1 coisa2)
	((and (equal (car constraint) '<) (symbolp (cadr constraint)) (symbolp (caddr constraint)))
	 (let* ((att-1 (cadr constraint))
		(att-1-domain (cdr (assoc att-1 domain-hash)))
		(att-1-item (cdr (assoc att-1 domain-item-hash)))
		(att-2 (caddr constraint))
		(att-2-domain (cdr (assoc att-2 domain-hash)))
		(att-2-item (cdr (assoc att-2 domain-item-hash))))
	   ;;Como na outra, se um dos atributos só é 1 em uma casa podemos eliminar a regra
	   (let ((count 0))
	     ;;Achar a primeira ocorrencia do primeiro atributo
	     (dotimes (x (1- nhouses))
	       (if (= 0  (aref field x att-1-domain att-1-item))
		   (setf count (1+ count))
		   (return)))
	     (dotimes (x (1+ count)) ;;Era aqui, ele nao ia até o fim
	       (setf (aref field x att-2-domain att-2-item) 0)))
	   (let ((marker 0))
	     ;;Achar a ultima ocorrencia do segundo atributo
	     (dotimes (x nhouses)
	       (if (= 1  (aref field x att-2-domain att-2-item))
		   (setf marker x)))
	     (dotimes (x nhouses)
	       (when (>= x marker) ;;E aqui tbm, pq ele comecava uma depois
		 (setf (aref field x att-1-domain att-1-item) 0))))
	   ;;Decide if can remove rule
	   (let ((flag 0) (count-1 0) (count-2 0))
	     (dotimes (x nhouses)
	       (when (= 1 (aref field x att-1-domain att-1-item))
		 (setf count-1 (1+ count-1)))
	       (when (= 1 (aref field x att-2-domain att-2-item))
		 (setf count-2 (1+ count-2))))
	     (when (or (= count-1 1) (= count-2 1))
	       (setf flag 1))
	     (when (or (= flag 0))
	       (setf adjusted-constraints (append adjusted-constraints (list constraint)))))))
	       

	;;Resolver as regras ABS (so existem ABS -, as + sao removidas no presolve)
	;;mais especificamente (= (ABS (- coisa1 coisa2)) n) 
	((and (equal (car constraint) '=) (equal (caadr constraint) 'ABS))
	 ;(print constraint)
	 (let* ((att-1 (cadr (cadadr constraint)))
		(att-1-domain (cdr (assoc att-1 domain-hash)))
		(att-1-item (cdr (assoc att-1 domain-item-hash)))
		(att-2 (caddr (cadadr constraint)))
		(att-2-domain (cdr (assoc att-2 domain-hash)))
		(att-2-item (cdr (assoc att-2 domain-item-hash)))
		(n (caddr constraint)))
	   ;;(print n)
	   ;;Se em x e x+2n att-1 é 0, att-2 de x+n = 0
	   ;;E vice versa
	   (let ((2n (* 2 n)))
	     (dotimes (x (- nhouses 2n))
	       (when (and (= 0 (aref field x att-1-domain att-1-item)) (= 0 (aref field (+ x 2n) att-1-domain att-1-item)))
		 (setf (aref field (+ x n) att-2-domain att-2-item) 0))
	       (when (and (= 0 (aref field x att-2-domain att-2-item)) (= 0 (aref field (+ x 2n) att-2-domain att-2-item)))
		 (setf (aref field (+ x n) att-1-domain att-1-item) 0))))
	   
	   ;;Se x1 esta definido x2 so pode ser definido em x1+n ou x1-n
	   (let ((count 0) (pos -1))
	     (dotimes (x nhouses)
	       (when (= (aref field x att-1-domain att-1-item) 1)
		 (setf count (+ 1 count))
		 (setf pos x)))
	     (when (= count 1)
	       (dotimes (x nhouses)
		 (unless (or (= (+ x n) pos) (= (- x n) pos))
		   (setf (aref field x att-2-domain att-2-item) 0)))))
	   ;;Se x2 esta definido x1 so pode ser definido em x2+n ou x2-n
	   (let ((count 0) (pos -1))
	     (dotimes (x nhouses)
	       (when (= (aref field x att-2-domain att-2-item) 1)
		 (setf count (+ 1 count))
		 (setf pos x)))
	     (when (= count 1)
	       (dotimes (x nhouses)
		 (unless (or (= (+ x n) pos) (= (- x n) pos))
		   (setf (aref field x att-1-domain att-1-item) 0)))))
	   
	   ;;Two special cases here, if an item is 0 on house n then the other cant be in 0,
	   ;;And if an item is 0 in (nhouses - (n+1)), the other can't be 1 in nhouses-1
	   (when (= 0 (aref field n att-1-domain att-1-item))
	     (setf (aref field 0 att-2-domain att-2-item) 0))
	   
	   (when (= 0 (aref field n att-2-domain att-2-item))
	     (setf (aref field 0 att-1-domain att-1-item) 0))
	   
	   (when (= 0 (aref field (- nhouses (+ n 1)) att-1-domain att-1-item))
	     (setf (aref field (1- nhouses) att-2-domain att-2-item) 0))
	   
	   (when (= 0 (aref field (- nhouses (+ n 1)) att-2-domain att-2-item))
	     (setf (aref field (1- nhouses) att-1-domain att-1-item) 0))

	   (setf adjusted-constraints (append adjusted-constraints (list constraint)))
	   ;(print att-1)
	   ;(print att-2)
	   ;(print constraint)))
	   ))
	;;Tratar as regras do tipo (OR (= Coisa1 Coisa2) (= Coisa1 Coisa3))
	((and (equal (car constraint) 'OR))
	 ;;(print constraint)
	 (let* ((att-0 (cadadr constraint))
		(att-0-domain (cdr (assoc att-0 domain-hash)))
		(att-0-item (cdr (assoc att-0 domain-item-hash)))
		(att-1 (car (cddadr constraint)))
		(att-1-domain (cdr (assoc att-1 domain-hash)))
		(att-1-item (cdr (assoc att-1 domain-item-hash)))
		(att-2 (cadr (cdaddr constraint)))
		(att-2-domain (cdr (assoc att-2 domain-hash)))
		(att-2-item (cdr (assoc att-2 domain-item-hash))))
	   ;(print att-0)
	   ;(print att-1)
	   ;(print att-2)
	   (dotimes (x nhouses)
	     (unless (or (= (aref field x att-1-domain att-1-item) 1) (= (aref field x att-2-domain att-2-item) 1))
	       (setf (aref field x att-0-domain att-0-item) 0)))
	   (setf adjusted-constraints (append adjusted-constraints (list constraint)))))
	 ;;Se chegou aqui é porque eu não tive regra esperta, melhor imprimir
	;;o que me escapou
	(T
	 (print "Nao sou smart")
	 (print constraint)
	 (setf adjusted-constraints (append adjusted-constraints (list constraint)))))
      (fix-field)
      )
    adjusted-constraints))

(defun choose-x-y (x y)
  (generate-field-aux)
  (let ((out 0))
    (dotimes (myx nhouses)
      (dotimes (myy ndomains)
	(when (and (not (= 1 (aref field_aux myx myy))) (or (> myx x) (and (= myx x) (> myy y))))
	  (setf x myx)
	  (setf y myy)
	  ;(pretty-print-field-aux)
	  (setf out 1)
	  (return)))
      (when (= out 1)
	(return)))
    (cons x y)))

(defun smart-apply-repeat (constraints)
  (let ((nsols (generate-field-aux)))
    (setf constraints (smart-apply1 constraints nil))
    (let ((nsols1 (generate-field-aux)))
      (if (= nsols1 1)
	  t
	  (progn
	    (if (= nsols nsols1)
		t
		(setf constraints (smart-apply-repeat constraints)))))))
  constraints)

(defun brutus (constraints x y)
  "Choose one position in array to brute-force, the position should be
after x y"
  ;;Start the brute-force approach by copying the field and the current
  ;;list of constraints
  (let ((old-field (copy-field field)) (flag nil))
    ;;Choose the first x y to tackle
    (let* ((xy (choose-x-y x y)) (x (car xy)) (y (cdr xy)))
      ;;Try each non-0 value
      (dotimes (z nhouses)
	;;x is a domain, y is a house
	(when (= 1 (aref field x y z))
	  (setf (aref field x y z) 0)
	  ;;Try this house with brutus
	  (smart-apply1 constraints t)
	  (let ((nsols (generate-field-aux)))
	    (when (= nsols 1)
	      (setf flag t)
	      (return))
	    (when (> nsols 1)
	      (setf flag (brutus constraints x y))
	      (when (equal flag t)
		(return)))
	    (when (= nsols 0)
	      (setf field (copy-field old-field))  
	  ))))
      )
    flag
    )
  )
(defun solver (path)
  (defparameter smartcals 0)
  (with-open-file (f path)
    (let* ((domains (read f))
	   (constraints (read f))
	   )
      (create-field domains constraints)
      (create-domain-alist domains)
      ;;(print domain-hash)
      (setf constraints (presolve constraints))
      ;;(print constraints)
      (setf constraints (smart-apply0 constraints))

      ;;Byte-compile some things, got me a good speedup
      (compile 'fix-field) ;Great, in extra62 from 0.6 to 0.3
      (compile 'generate-field-aux) ;Good, from 0.3 to 0.2
      ;(compile 'brutus) ;This others only help in some specific cases
      ;(compile 'choose-x-y)
      ;(compile 'smart-apply0)
      ;(compile 'smart-apply1)
      (setf constraints (smart-apply-repeat constraints))

      ;;E aqui começa o brute
      (fix-field)
      (if (= (generate-field-aux) 1)
	  t
	  (progn
	    (if (= 1 (aref field_aux 0 0))
		(brutus constraints 0 0)
		(brutus constraints 0 -1))
	    ))
      ;;(print constraints)
      )
    )
  (print smartcals)
  (generate-solution-from-field)
  )


;;;Auxiliares (ler e imprimir entrada e saida de varias formas)
(defun massoc (key alist)
  "For some reason assoc is not working with a pair even when I change the test
function to equalp (which should be good) so I will make it myself"
  (if (equal alist '())
    '()
    (if (equal (caar alist) key)
	(cdar alist)
	(massoc key (cdr alist)))))

(defun generate-solution-from-field ()
  (let ((output '()))
       (dotimes (y ndomains)
	 (dotimes (x nhouses)
	   (dotimes (z nhouses)
	     (when (= 1 (aref field x y z))
	       (setf dname (cdr (assoc y domain-name-number)))
	       (setf house-number x)
	       (setf item-number z)
	       (setf item-name (massoc (cons y item-number) domain-item-inverse-hash))
	       ;(print "-----------------")
	       ;(print (cdr (assoc y domain-name-number)))
	       ;(print house-number)
	       ;(print item-name)
					;(print "-----------------")
	       (setf output (append output (list (list item-name  (1+ house-number)))))

  )))) output ))

(defun create-domain-alist (domains)
  "Might be exchanged for a hashtable in the future"
  (defparameter domain-hash '()) ;identify from witch domain
  (defparameter domain-item-hash '()) ;identify an iten in a domain
  (defparameter domain-name-number '()) ;associate domain names and numbers
  ;(defparameter domain-inverse-hash '())
  (defparameter domain-item-inverse-hash '())
  ;(defparameter domain-name-inverse-number '())
  (let ((domain-number 0))
    (dolist (domain domains)
      (setf domain-name (caadr domain))
      (push (cons domain-number domain-name) domain-name-number)
      ;(push (cons domain-name domain-number) domain-name-inverse-number)
      (let ((item-number 0))
	(dolist (element (cddr domain))
	  (push (cons element item-number) domain-item-hash)
	  (push (cons (cons domain-number item-number) element) domain-item-inverse-hash)
	  (setf item-number (1+ item-number))
	  (push (cons element domain-number) domain-hash))
	(setf domain-number (1+ domain-number))))))

(defun create-field (domains constraints)
  (defparameter nhouses (- (length (car domains)) 2))
  (defparameter ndomains (length domains))
  (defparameter field (make-array (list nhouses ndomains nhouses)))
  (defparameter field_aux (make-array (list nhouses ndomains)))
  (defparameter min_field_aux nhouses)
  (dotimes (x nhouses)
	(dotimes (y ndomains)
	  (dotimes  (z nhouses)
	    (setf (aref field x y z) 1))
	  (setf (aref field_aux x y) 0))))

(defun copy-field (old-field)
  (let ((new-field (make-array (list nhouses ndomains nhouses))))
    (dotimes (x nhouses)
      (dotimes (y ndomains)
	(dotimes (z nhouses)
	  (setf (aref new-field x y z) (aref old-field x y z)))))
    new-field))

(defun pretty-print-field ()
  (dotimes (y ndomains)
    (dotimes (x nhouses)
      (print "n")
      (dotimes (z nhouses)
	(format t "~a " (aref field x y z))))
    (print "-----------------------")))

(defun generate-field-aux ()
  (let ((acc 1))
    (dotimes (x nhouses)
      (dotimes (y ndomains)
	(setf (aref field_aux x y) 0)
	(dotimes (z nhouses)
	  (setf (aref field_aux x y) (+ (aref field_aux x y) (aref field x y z))))
	(setf acc (* acc (aref field_aux x y)))))
    acc))

(defun pretty-print-field-aux ()
  (generate-field-aux)
  (let ((acc 1))
    (dotimes (y ndomains)
      (print ".")
      (dotimes (x nhouses)
	(setf acc (* acc (aref field_aux x y)))
	(format t "~a " (aref field_aux x y))))(print acc) acc))
