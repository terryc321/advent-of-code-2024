
(import (chicken pretty-print))
(import (srfi-1))
(import  (simple-loops))
(import (chicken format))
;; (import (scheme char))
(define fmt format)
(import (srfi-8))

(import (chicken sort))

;; tidy up the antlr s expression tree output
;; really just want each r to be (a x y)(b x y)(c x y)
;; where a button A 
;; b button B 
;; c prize 
;; 

(define (string-capitalize s)
  (list->string (map char-upcase (string->list s))))

(define (symbol-capitalize s)
  (string->symbol (list->string (map char-upcase (string->list (symbol->string s))))))

(define (sorted-symbols xs)
  (map string->symbol (sort (map symbol->string xs) string<?)))

(define (test xs)
  (letrec ((foo (lambda (xs)
		  (cond
		   ((null? xs) xs)
		   (#t (map (lambda (x) (fmt #t "~a " x)) xs))))))
    (let-values (((fix syms) (fixup xs)))
      (let ((tidy-syms (sorted-symbols syms)))
	;; declarations 
	(format #t "declare ")
	(foo tidy-syms)
	(format #t "~%~%")
	;; assignments and boolean operations 
	(map (lambda (x) (map (lambda (y) (format #t "~a" y)) x) (format #t "~%")) fix)
	#f))))




(define (fixup xs)
  (let ((symbols '()))
    (letrec ((add-symbol! (lambda (x)
			    (when (not (member x symbols))
			      (set! symbols (cons x symbols)))))
	     (fix-assigns (lambda (x)
			    (let ((sym (symbol-capitalize (second x)))
				  (val (second (fourth x))))
			      (add-symbol! sym)
			      (list sym "=" val))))
	     (fix-generic (lambda (tag x) 	     
		       (let ((sym1 (symbol-capitalize (second x)))
			     (sym2 (symbol-capitalize (fourth x)))
			     (sym3 (symbol-capitalize (sixth x))))
			 (add-symbol! sym1)
			 (add-symbol! sym2)
			 (add-symbol! sym3)			 
		       (list tag " " sym1 " " sym2 " " sym3 "}"))))
	   (fix-and (lambda (x) '(#t)))
	   (fix-xor (lambda (x) '(#t)))
	   (foo (lambda (x)
		  (cond
		   ((eq? (car x) 'assigns) (fix-assigns x))
		   ((eq? (car x) 'or) (fix-generic "{Ior" x))
		   ((eq? (car x) 'xor) (fix-generic "{Ixor" x))
		   ((eq? (car x) 'and) (fix-generic "{Iand" x))
		   (#t (error "?"))))))
  (let* ((s1 (apply append (map cdr (cdr xs))))
	 (s2 (map foo s1)))    
    (values s2 symbols)))))

    










(define example1 '(s (r (assigns x00 : (bool 1))) (r (assigns x01 : (bool 1))) (r (assigns x02 : (bool 1))) (r (assigns y00 : (bool 0))) (r (assigns y01 : (bool 1))) (r (assigns y02 : (bool 0))) (r (and x00 AND y00 -> z00)) (r (xor x01 XOR y01 -> z01)) (r (or x02 OR y02 -> z02))))

(define example2 '(s (r (assigns x00 : (bool 1))) (r (assigns x01 : (bool 0))) (r (assigns x02 : (bool 1))) (r (assigns x03 : (bool 1))) (r (assigns x04 : (bool 0))) (r (assigns y00 : (bool 1))) (r (assigns y01 : (bool 1))) (r (assigns y02 : (bool 1))) (r (assigns y03 : (bool 1))) (r (assigns y04 : (bool 1))) (r (xor ntg XOR fgs -> mjb)) (r (or y02 OR x01 -> tnw)) (r (or kwq OR kpj -> z05)) (r (or x00 OR x03 -> fst)) (r (xor tgd XOR rvg -> z01)) (r (or vdt OR tnw -> bfw)) (r (and bfw AND frj -> z10)) (r (or ffh OR nrd -> bqk)) (r (and y00 AND y03 -> djm)) (r (or y03 OR y00 -> psh)) (r (or bqk OR frj -> z08)) (r (or tnw OR fst -> frj)) (r (and gnj AND tgd -> z11)) (r (xor bfw XOR mjb -> z00)) (r (or x03 OR x00 -> vdt)) (r (and gnj AND wpb -> z02)) (r (and x04 AND y00 -> kjc)) (r (or djm OR pbm -> qhw)) (r (and nrd AND vdt -> hwm)) (r (and kjc AND fst -> rvg)) (r (or y04 OR y02 -> fgs)) (r (and y01 AND x02 -> pbm)) (r (or ntg OR kjc -> kwq)) (r (xor psh XOR fgs -> tgd)) (r (xor qhw XOR tgd -> z09)) (r (or pbm OR djm -> kpj)) (r (xor x03 XOR y03 -> ffh)) (r (xor x00 XOR y04 -> ntg)) (r (or bfw OR bqk -> z06)) (r (xor nrd XOR fgs -> wpb)) (r (xor frj XOR qhw -> z04)) (r (or bqk OR frj -> z07)) (r (or y03 OR x01 -> nrd)) (r (and hwm AND bqk -> z03)) (r (xor tgd XOR rvg -> z12)) (r (or tnw OR pbm -> gnj))))

(define input '(s (r (assigns x00 : (bool 1))) (r (assigns x01 : (bool 1))) (r (assigns x02 : (bool 0))) (r (assigns x03 : (bool 0))) (r (assigns x04 : (bool 0))) (r (assigns x05 : (bool 1))) (r (assigns x06 : (bool 0))) (r (assigns x07 : (bool 1))) (r (assigns x08 : (bool 1))) (r (assigns x09 : (bool 0))) (r (assigns x10 : (bool 1))) (r (assigns x11 : (bool 0))) (r (assigns x12 : (bool 0))) (r (assigns x13 : (bool 0))) (r (assigns x14 : (bool 1))) (r (assigns x15 : (bool 0))) (r (assigns x16 : (bool 1))) (r (assigns x17 : (bool 1))) (r (assigns x18 : (bool 0))) (r (assigns x19 : (bool 1))) (r (assigns x20 : (bool 0))) (r (assigns x21 : (bool 0))) (r (assigns x22 : (bool 1))) (r (assigns x23 : (bool 1))) (r (assigns x24 : (bool 1))) (r (assigns x25 : (bool 1))) (r (assigns x26 : (bool 0))) (r (assigns x27 : (bool 1))) (r (assigns x28 : (bool 1))) (r (assigns x29 : (bool 0))) (r (assigns x30 : (bool 1))) (r (assigns x31 : (bool 0))) (r (assigns x32 : (bool 0))) (r (assigns x33 : (bool 1))) (r (assigns x34 : (bool 1))) (r (assigns x35 : (bool 0))) (r (assigns x36 : (bool 1))) (r (assigns x37 : (bool 1))) (r (assigns x38 : (bool 1))) (r (assigns x39 : (bool 1))) (r (assigns x40 : (bool 1))) (r (assigns x41 : (bool 1))) (r (assigns x42 : (bool 1))) (r (assigns x43 : (bool 1))) (r (assigns x44 : (bool 1))) (r (assigns y00 : (bool 1))) (r (assigns y01 : (bool 0))) (r (assigns y02 : (bool 1))) (r (assigns y03 : (bool 1))) (r (assigns y04 : (bool 0))) (r (assigns y05 : (bool 0))) (r (assigns y06 : (bool 1))) (r (assigns y07 : (bool 1))) (r (assigns y08 : (bool 0))) (r (assigns y09 : (bool 1))) (r (assigns y10 : (bool 1))) (r (assigns y11 : (bool 1))) (r (assigns y12 : (bool 1))) (r (assigns y13 : (bool 0))) (r (assigns y14 : (bool 1))) (r (assigns y15 : (bool 1))) (r (assigns y16 : (bool 1))) (r (assigns y17 : (bool 0))) (r (assigns y18 : (bool 1))) (r (assigns y19 : (bool 0))) (r (assigns y20 : (bool 0))) (r (assigns y21 : (bool 1))) (r (assigns y22 : (bool 0))) (r (assigns y23 : (bool 1))) (r (assigns y24 : (bool 0))) (r (assigns y25 : (bool 1))) (r (assigns y26 : (bool 0))) (r (assigns y27 : (bool 1))) (r (assigns y28 : (bool 0))) (r (assigns y29 : (bool 0))) (r (assigns y30 : (bool 1))) (r (assigns y31 : (bool 1))) (r (assigns y32 : (bool 0))) (r (assigns y33 : (bool 1))) (r (assigns y34 : (bool 0))) (r (assigns y35 : (bool 0))) (r (assigns y36 : (bool 1))) (r (assigns y37 : (bool 0))) (r (assigns y38 : (bool 1))) (r (assigns y39 : (bool 0))) (r (assigns y40 : (bool 0))) (r (assigns y41 : (bool 0))) (r (assigns y42 : (bool 1))) (r (assigns y43 : (bool 0))) (r (assigns y44 : (bool 1))) (r (and y08 AND x08 -> pkh)) (r (and grg AND twt -> bbk)) (r (or vvt OR wwt -> vgs)) (r (xor x10 XOR y10 -> pmq)) (r (xor pmq XOR hkf -> z10)) (r (or vmw OR bfb -> hkf)) (r (or twp OR qbq -> kmj)) (r (and qns AND mwj -> qhk)) (r (xor dqm XOR cqp -> z02)) (r (and snr AND crb -> htp)) (r (xor jwv XOR dgj -> z09)) (r (or sjf OR rwf -> wkq)) (r (xor y02 XOR x02 -> dqm)) (r (and msw AND qqp -> rss)) (r (xor fgv XOR bhw -> z26)) (r (and y03 AND x03 -> ftb)) (r (xor kmj XOR fnh -> z24)) (r (xor jhv XOR bkq -> wtt)) (r (xor x27 XOR y27 -> knf)) (r (xor y40 XOR x40 -> jhf)) (r (or gmq OR nrs -> kkq)) (r (xor y03 XOR x03 -> ndn)) (r (xor x25 XOR y25 -> hfj)) (r (and x33 AND y33 -> rvf)) (r (and wkh AND pmn -> vrq)) (r (or mgg OR hbf -> fhw)) (r (or nrr OR rtk -> wqr)) (r (and x00 AND y00 -> mtk)) (r (xor crb XOR snr -> z30)) (r (xor y37 XOR x37 -> qns)) (r (or kgw OR wms -> snr)) (r (xor y09 XOR x09 -> jwv)) (r (and mtk AND kvc -> jjp)) (r (xor x01 XOR y01 -> kvc)) (r (and jgg AND bsn -> fpm)) (r (or kvt OR ftb -> drd)) (r (and x15 AND y15 -> tfs)) (r (xor x34 XOR y34 -> jgv)) (r (xor y44 XOR x44 -> vjg)) (r (xor x21 XOR y21 -> fqr)) (r (xor x36 XOR y36 -> rwh)) (r (and y30 AND x30 -> kgr)) (r (or sqg OR hfb -> rfq)) (r (xor x05 XOR y05 -> kbj)) (r (or rjq OR skn -> jhv)) (r (and y17 AND x17 -> bmp)) (r (and x28 AND y28 -> jwb)) (r (and x44 AND y44 -> vrj)) (r (xor wwn XOR gwm -> z23)) (r (and y39 AND x39 -> gsr)) (r (xor wkq XOR tfv -> z15)) (r (and x31 AND y31 -> sst)) (r (xor x08 XOR y08 -> twt)) (r (and wnf AND rsk -> pjp)) (r (and mmr AND qdc -> rwf)) (r (xor y22 XOR x22 -> jsr)) (r (or pnj OR bcc -> hhn)) (r (or fpm OR wtv -> kth)) (r (xor kkq XOR jsr -> z22)) (r (xor srn XOR kth -> z33)) (r (or vrq OR mrt -> tkc)) (r (and wwn AND gwm -> qbq)) (r (xor jhf XOR mdn -> z40)) (r (xor y13 XOR x13 -> tqf)) (r (xor kvc XOR mtk -> z01)) (r (xor stg XOR kdv -> z35)) (r (xor grg XOR twt -> z08)) (r (and kkq AND jsr -> fjh)) (r (and tmd AND shm -> rjm)) (r (and nvq AND chk -> gnp)) (r (or twb OR ptq -> qqp)) (r (xor x32 XOR y32 -> bsn)) (r (xor y39 XOR x39 -> bkq)) (r (or mph OR vbp -> stg)) (r (and x02 AND y02 -> rks)) (r (and dtj AND hfj -> frh)) (r (xor y43 XOR x43 -> qmr)) (r (xor chk XOR nvq -> z28)) (r (and x16 AND y16 -> wnf)) (r (xor y06 XOR x06 -> ghf)) (r (or rhr OR ccq -> qjq)) (r (and x38 AND y38 -> rjq)) (r (and tsf AND dtb -> twb)) (r (xor x15 XOR y15 -> tfv)) (r (and vds AND gdk -> skn)) (r (xor hhn XOR rwh -> z36)) (r (and x40 AND y40 -> bjf)) (r (or wtd OR cks -> nvq)) (r (or jpk OR qhk -> vds)) (r (xor x14 XOR y14 -> qdc)) (r (and y19 AND x19 -> ptq)) (r (and fnh AND kmj -> gqj)) (r (or pjp OR vtj -> smg)) (r (xor x11 XOR y11 -> tmd)) (r (or bbk OR pkh -> dgj)) (r (and bkq AND jhv -> z39)) (r (and y07 AND x07 -> krp)) (r (and hkf AND pmq -> crj)) (r (and y22 AND x22 -> cdk)) (r (or knb OR vkk -> mmr)) (r (xor vds XOR gdk -> z38)) (r (and jhf AND mdn -> qkq)) (r (or vrj OR gkc -> z45)) (r (and x41 AND y41 -> mrt)) (r (xor jgv XOR dkh -> z34)) (r (xor y16 XOR x16 -> vtj)) (r (or rcf OR gqj -> dtj)) (r (or rvf OR bqr -> dkh)) (r (and x25 AND y25 -> swn)) (r (xor dbv XOR vhs -> z31)) (r (xor y12 XOR x12 -> fgq)) (r (and y20 AND x20 -> kcf)) (r (or rss OR kcf -> mjj)) (r (xor hfj XOR dtj -> z25)) (r (and x14 AND y14 -> sjf)) (r (xor fpd XOR smg -> z17)) (r (and x21 AND y21 -> z21)) (r (and x43 AND y43 -> prt)) (r (xor x31 XOR y31 -> vhs)) (r (or grv OR tfs -> rsk)) (r (xor y33 XOR x33 -> srn)) (r (xor msw XOR qqp -> z20)) (r (or gsr OR wtt -> mdn)) (r (xor x30 XOR y30 -> crb)) (r (and y24 AND x24 -> rcf)) (r (or frh OR swn -> bhw)) (r (and htm AND ndn -> kvt)) (r (and x06 AND y06 -> hbf)) (r (xor x35 XOR y35 -> kdv)) (r (and srn AND kth -> bqr)) (r (and dgj AND jwv -> bfb)) (r (xor bsn XOR jgg -> z32)) (r (or rjm OR skj -> tff)) (r (and gcf AND fhw -> mpv)) (r (or vfb OR jjp -> cqp)) (r (xor x00 XOR y00 -> z00)) (r (or crj OR fgc -> shm)) (r (xor tff XOR fgq -> z12)) (r (xor mwj XOR qns -> z37)) (r (and vjg AND kmg -> gkc)) (r (xor x19 XOR y19 -> dtb)) (r (xor hvs XOR tsn -> z18)) (r (xor dtb XOR tsf -> z19)) (r (and y05 AND x05 -> srp)) (r (and kkh AND bsb -> kgw)) (r (or rks OR trv -> htm)) (r (and frn AND ghf -> mgg)) (r (xor qjq XOR kbj -> frn)) (r (xor knf XOR vgs -> z27)) (r (and y09 AND x09 -> vmw)) (r (and x36 AND y36 -> dbc)) (r (and knf AND vgs -> wtd)) (r (xor y23 XOR x23 -> wwn)) (r (xor wnf XOR rsk -> z16)) (r (xor y29 XOR x29 -> kkh)) (r (xor vjg XOR kmg -> z44)) (r (or htp OR kgr -> dbv)) (r (xor htm XOR ndn -> z03)) (r (xor rfq XOR tqf -> z13)) (r (and mjj AND fqr -> nrs)) (r (xor gcf XOR fhw -> z07)) (r (and qmr AND wqr -> frs)) (r (and x32 AND y32 -> wtv)) (r (xor x07 XOR y07 -> gcf)) (r (or gmf OR pvh -> tsf)) (r (xor mjj XOR fqr -> gmq)) (r (or qqs OR dbc -> mwj)) (r (or krp OR mpv -> grg)) (r (and x34 AND y34 -> vbp)) (r (and tff AND fgq -> hfb)) (r (and drd AND wkb -> rhr)) (r (and tqf AND rfq -> vkk)) (r (and x13 AND y13 -> knb)) (r (and bhw AND fgv -> vvt)) (r (and hhn AND rwh -> qqs)) (r (and kbj AND qjq -> jcf)) (r (xor y41 XOR x41 -> wkh)) (r (and y01 AND x01 -> vfb)) (r (and jgv AND dkh -> mph)) (r (or gnp OR jwb -> bsb)) (r (or cdk OR fjh -> gwm)) (r (or prt OR frs -> kmg)) (r (xor x17 XOR y17 -> fpd)) (r (and y42 AND x42 -> rtk)) (r (and fpd AND smg -> tqm)) (r (and dbv AND vhs -> gdw)) (r (and y29 AND x29 -> wms)) (r (xor kkh XOR bsb -> z29)) (r (and cqp AND dqm -> trv)) (r (and hvs AND tsn -> gmf)) (r (and wkq AND tfv -> grv)) (r (and x04 AND y04 -> ccq)) (r (xor x20 XOR y20 -> msw)) (r (xor y26 XOR x26 -> fgv)) (r (xor mmr XOR qdc -> z14)) (r (xor ghf XOR frn -> z06)) (r (and y26 AND x26 -> wwt)) (r (or sst OR gdw -> jgg)) (r (xor pmn XOR wkh -> z41)) (r (xor y04 XOR x04 -> wkb)) (r (xor y28 XOR x28 -> chk)) (r (xor wqr XOR qmr -> z43)) (r (and kdv AND stg -> bcc)) (r (and y18 AND x18 -> pvh)) (r (xor mvk XOR tkc -> z42)) (r (and x27 AND y27 -> cks)) (r (or bjf OR qkq -> pmn)) (r (and tkc AND mvk -> nrr)) (r (xor y38 XOR x38 -> gdk)) (r (and y37 AND x37 -> jpk)) (r (and x23 AND y23 -> twp)) (r (or tqm OR bmp -> tsn)) (r (xor wkb XOR drd -> z04)) (r (xor x42 XOR y42 -> mvk)) (r (and y35 AND x35 -> pnj)) (r (xor shm XOR tmd -> z11)) (r (xor y24 XOR x24 -> fnh)) (r (and x11 AND y11 -> skj)) (r (and x10 AND y10 -> fgc)) (r (or srp OR jcf -> z05)) (r (and y12 AND x12 -> sqg)) (r (xor y18 XOR x18 -> hvs))))



(define (run)
  (with-output-to-file "input.oz" (lambda () (test input)))
  (with-output-to-file "example1.oz" (lambda () (test example1)))
  (with-output-to-file "example2.oz" (lambda () (test example2))))








