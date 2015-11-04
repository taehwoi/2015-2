#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw4-2.rkt")

(sgoutput
 (lambda ()
   (equal?
    (react (a (a (a S K) I) (v "x")))
    "x")))
(sgoutput
 (lambda ()
   (equal?
    (react (a (a K (v "x")) (a I (v "x"))))
    "x")))
(sgoutput
 (lambda ()
   (equal?
    (react (a (a (a (v "x") (v "y")) (v "z")) (v "w")))
    "(((x y) z) w)")))
(equal?
 (react S)
 "S")
(equal?
 (react K)
 "K")
(equal?
 (react I)
 "I")
(equal?
 (react (v "1"))
 "1")
(equal? 
 (react (a I (v "x"))) 
 "x") 
(equal? 
 (react (a (a K (v "x")) (v "y"))) 
 "x") 
(equal? 
 (react (a (a (a S (v "x")) (v "y")) (v "z"))) 
 "((x z) (y z))") 
(equal? 
 (react (a (a (a S K) S) K)) 
 "K") 
(equal? 
 (react (a (a (a (a S (a K (a S I))) K) (v "x")) (v "y"))) 
 "(y x)")
(equal? 
 (react (a (a (a (v "H") (v "e")) (a (a (a (a (a K (a I S)) (v "x")) (a I I)) (a (a K I) (v "x"))) (v "l"))) (a (a (v "o") (a (a K (a (v ",") (v "w"))) (v "x"))) (a (a (a I (v "o")) (a I (v "r"))) (a (a (a (a S (a K (a S K))) S) (a (a K (a (a K S) (v "3"))) (v "5"))) (a (v "l") (v "d")))))) 
) 
 "(((H e) (l l)) ((o (, w)) ((o r) (l d))))")

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a K S) (a S (v "jangho"))) S) (a (a (a (v "eec") S) I) (a (v "security") (a (v "sysprog") (a I (v "jangho")))))))
   "((S S) (((eec S) I) (security (sysprog jangho))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a I (a (a (a I S) K) S)))
   "((S K) S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a I (a (a (v "CSEseminar") K) I)) (a (a (a (v "eec") I) (a K (v "pl"))) (a (v "physlab") K))))
   "(((CSEseminar K) I) (((eec I) (K pl)) (physlab K)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (a (a S (v "physlab")) (v "jangho"))) (a (a (a I I) (a (a (v "physlab") I) K)) (a K (v "sysprog")))))
   "((S ((S physlab) jangho)) (((physlab I) K) (K sysprog)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a K S) S) (a (a (a K (v "revreserver")) S) K)) (a S (a I S))))
   "((S (revreserver K)) (S S))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a (a I I) S) I) (a K K)) (a (a (a I (v "doo0")) K) (a I (a S I)))))
   "(((doo0 K) (S I)) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a S K) S) (v "jangho")) (a (a (a S K) I) (a (a S (v "lastone")) I))))
   "(jangho ((S lastone) I))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a S (a (v "doo0") K)) (a (a (v "swpp") I) (v "sysprog"))) (a (a (a K S) I) (a (a (v "swpp") S) (a I (a I I))))))
   "(((doo0 K) (S ((swpp S) I))) (((swpp I) sysprog) (S ((swpp S) I))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a (a K S) S) S) (a I (a I (a S K)))) (a (a S I) I)))
   "((S ((S I) I)) ((S K) ((S I) I)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a I (a K S)) (a (a S I) I)) S) S))
   "((S S) S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a K (a (a (a (a S K) K) I) (a (a S I) (a I (a (v "revreserver") K))))))
   "(K ((S I) (revreserver K)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (a S K)) (a (a (a I S) (a K K)) I)))
   "(S K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a S S) (a K (a (a K (v "Medivh")) I))) (a I (a (v "myrmidon") (v "doo0")))))
   "((S (myrmidon doo0)) Medivh)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a I I) I) (a (a (a (v "physlab") I) (a I (a (v "swpp") S))) I)))
   "(((physlab I) (swpp S)) I)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a S (v "revreserver")))
   "(S revreserver)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a K (v "swpp")) (a (a K (v "doo0")) (a S (v "pl")))) (a (a (a (a K S) (a (v "os") (v "hoonga"))) (a S S)) (a (a (a I S) S) (a (v "pp") K)))))
   "(swpp ((S (S S)) ((S S) (pp K))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a K K) (v "eec")) (a (v "os") K)) (a (a (a (a I K) (a I S)) K) (a I (a (a K K) I)))))
   "(os K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a I (a S I)) S) (a (a (a S S) (v "eec")) (a (a (v "revreserver") (v "eec")) S))))
   "((((revreserver eec) S) (S ((S ((revreserver eec) S)) (eec ((revreserver eec) S))))) ((eec ((revreserver eec) S)) (S ((S ((revreserver eec) S)) (eec ((revreserver eec) S))))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (v "physlab") I) (a I (a K I))) (a (a S K) (a (a K S) I))))
   "(((physlab I) (K I)) ((S K) S))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a K K) (a I S)) (a (a (v "swpp") S) (a (a I I) (a (v "sysprog") I)))) (a (a (v "pp") (a S (a K S))) I)))
   "((swpp S) (sysprog I))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a S I) (v "hoonga")) S))
   "(S (hoonga S))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a S (a (a S (a (a (v "sysprog") (v "pl")) (a I K))) (a S (a I K)))))
   "(S ((S ((sysprog pl) K)) (S K)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (v "physlab")) (a (a I K) (a I (a (v "swpp") (a S S))))))
   "((S physlab) (K (swpp (S S))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a I K) K) (a S S)) (a S (a S S))))
   "(K (S (S S)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (v "myrmidon") (a (a (a (v "lastone") K) (v "myrmidon")) (a (v "pl") (a (a I K) S)))))
   "(myrmidon (((lastone K) myrmidon) (pl (K S))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (v "Medivh") K) K) (a K (a (a I (a I (v "eec"))) (a S S)))))
   "(((Medivh K) K) (K (eec (S S))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a S (a I I)) I) (a (a I I) (a S S))))
   "((S S) (S S))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (v "doo0") (a (a S (v "jangho")) I)) (a (a S S) (a I K))))
   "((doo0 ((S jangho) I)) ((S S) K))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a S I) (a S (v "doo0"))) (a (a (a I (a (v "sysprog") K)) (a (v "security") S)) (a K S))))
   "((((sysprog K) (security S)) (K S)) ((S doo0) (((sysprog K) (security S)) (K S))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a I (a (v "hoonga") (a I K))))
   "(hoonga K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a K (a (a I I) (a (v "myrmidon") K))))
   "(K (myrmidon K))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a K K) (a (v "swpp") (a S (a K S)))) K))
   "(K K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (v "myrmidon") (a (a I I) (a S S))) (a (a K (a K (a I S))) (a (a I (a (v "jangho") (v "hoonga"))) (v "Medivh")))))
   "((myrmidon (S S)) (K S))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a K K))
   "(K K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (v "hoonga") (a S (v "revreserver"))) (a (a S S) (a S (v "physlab")))) (a I S)))
   "(((hoonga (S revreserver)) ((S S) (S physlab))) S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a I (a K K)) (a I (a K (a (v "doo0") (v "hoonga"))))) (a S K)))
   "(K (S K))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (a (a S I) K)) (a K I)))
   "((S I) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a (a S K) (a I (v "doo0"))) (a (a (v "eec") S) S)) (v "revreserver")) (a (a (a (v "doo0") K) I) (a (a (a S S) I) K))))
   "((((eec S) S) revreserver) (((doo0 K) I) ((S K) K)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (v "eec") (a (a (v "physlab") S) (a (v "os") K))) (a (a K K) (v "swpp"))) (a (a (a I I) (v "lastone")) (a (a (a (v "doo0") (v "physlab")) I) (a (v "sysprog") S)))))
   "(((eec ((physlab S) (os K))) K) (lastone (((doo0 physlab) I) (sysprog S))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (a I (a (a I (v "pl")) S))) (a (a (v "eec") (a I (v "eec"))) (v "hoonga"))))
   "((S (pl S)) ((eec eec) hoonga))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a I (a I (a K (v "Medivh")))) (a (a (a S K) S) I)) (a I (v "os"))))
   "(Medivh os)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a S K) (a (v "pp") K)) (a S (a (a I K) K))))
   "(S (K K))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a I (a (v "eec") I)))
   "(eec I)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a S S) K) (a S (a K S))) (a S (a (a (v "CSEseminar") K) S))))
   "(S ((S ((CSEseminar K) S)) (S (K S))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a K (v "hoonga")) (v "myrmidon")) (a (a (a I S) (a I I)) (a (a S (v "swpp")) (a S K)))) (a (a K (v "physlab")) (a (a I (v "swpp")) (a (v "pl") (v "sysprog"))))))
   "((hoonga ((S I) ((S swpp) (S K)))) physlab)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (v "swpp") (a (a S (a I (v "doo0"))) (a S (a S S)))))
   "(swpp ((S doo0) (S (S S))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a I S) (a I S)) (a I (a (a K S) (a S (v "myrmidon"))))))
   "((S S) S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a I K) (a K S)) (a (a (v "lastone") (v "physlab")) (a (a S I) I))) I))
   "S"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a I (a (v "sysprog") (a (v "pp") K))) I))
   "((sysprog (pp K)) I)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (a I S)) (a (a I (a K (a S I))) (a (v "hoonga") K))))
   "((S S) (S I))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a S K) (a (a S (v "Medivh")) S)) (v "swpp")) (a (v "sysprog") I)))
   "(swpp (sysprog I))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (a (v "sysprog") (a (v "sysprog") S))) (a I (a (a I (v "revreserver")) (v "CSEseminar")))))
   "((S (sysprog (sysprog S))) (revreserver CSEseminar))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (a K K)) (a S (a (a K K) I))))
   "(K K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a K K) I) (a (a I I) (a S (a K I)))))
   "(K (S (K I)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K K) (a (a K (v "pp")) (a (a (v "swpp") I) (v "eec")))))
   "K"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (v "sysprog") I) I))
   "((sysprog I) I)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a S S) (a S K)) (a (a (a (v "pp") (v "pl")) (v "lastone")) (a S (a I I)))))
   "((S (((pp pl) lastone) (S I))) ((S K) (((pp pl) lastone) (S I))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a S I) (a (a (v "jangho") I) (v "doo0"))) (a I K)) (a I (a (a S K) (v "pl")))))
   "(((jangho I) doo0) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S I) (a I (a (a (a (v "physlab") (v "physlab")) (a S K)) (a K (a K S))))))
   "((S I) (((physlab physlab) (S K)) (K (K S))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (v "pl") K) (v "security")))
   "((pl K) security)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a I (a (a (a (a (v "jangho") (v "jangho")) (v "lastone")) (v "CSEseminar")) (a I (a (a I K) I)))))
   "((((jangho jangho) lastone) CSEseminar) (K I))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (a (a (a I S) (a (v "swpp") (v "lastone"))) K)) (a (a S (a I (a I S))) (a I I))))
   "((S (swpp lastone)) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a I (a I (v "swpp"))) S))
   "(swpp S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a K (a I K)) K) (v "revreserver")) (a (a S I) (a (a I (a I (v "doo0"))) K))))
   "revreserver"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (a (v "pl") S)) (a (v "swpp") (a (a S I) (v "revreserver")))))
   "(pl S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a I K))
   "K"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a K (a (a S K) I)) K) (a (a S (a I (v "revreserver"))) I)))
   "((S revreserver) I)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (v "jangho")) (a (v "jangho") I)))
   "((S jangho) (jangho I))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a (v "swpp") I) K) K) (a (a I (a K S)) (a (a S (a S S)) (a S (a K K))))))
   "((((swpp I) K) K) S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a I (v "pl")) S) (a (v "security") K)))
   "((pl S) (security K))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a K (a (v "CSEseminar") (v "hoonga"))) (a (a I K) (a (v "swpp") K))) (a I K)))
   "((CSEseminar hoonga) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a I (a (a K I) (a (a (v "swpp") K) S))) S))
   "S"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (a (a K (a K I)) (a (v "hoonga") (v "swpp")))) K))
   "((S (K I)) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a K (a (a (a (a S (v "os")) S) K) I)))
   "(K (((os K) (S K)) I))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (v "jangho") (a (v "sysprog") (a S I))))
   "(jangho (sysprog (S I)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a I (a I K)) (a K S)) (v "pl")))
   "(K S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a I (a S I)) (a K S)))
   "((S I) (K S))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a I (a (a (v "lastone") S) I)) (a I K)))
   "(((lastone S) I) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a K (a (v "lastone") (v "swpp"))) (v "physlab")) (a (a (v "hoonga") (a S S)) K)))
   "((lastone swpp) ((hoonga (S S)) K))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (a (a (v "myrmidon") I) (a K (a I (v "CSEseminar"))))) I))
   "((myrmidon I) (K CSEseminar))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a I S) (v "myrmidon")) (a (a (v "hoonga") (a S (v "revreserver"))) (a (v "pp") I))) (a (a (v "os") S) (a (v "Medivh") K))))
   "((myrmidon ((os S) (Medivh K))) (((hoonga (S revreserver)) (pp I)) ((os S) (Medivh K))))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a (a S I) S) (a S I)) (a (v "lastone") (a (v "pp") S))) (a (v "hoonga") (a (v "jangho") (v "CSEseminar")))))
   "(((lastone (pp S)) ((S (S I)) (lastone (pp S)))) (hoonga (jangho CSEseminar)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S K) (a S (a K K))))
   "((S K) (S (K K)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a I (a (a S (v "security")) (v "sysprog"))) (a S (v "revreserver"))) (a K (a I I))))
   "(((security (S revreserver)) (sysprog (S revreserver))) (K I))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a K (a K (v "lastone"))) (v "sysprog")) (a K I)))
   "lastone"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (a K (v "pp"))) K))
   "((S (K pp)) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (v "sysprog") (a K S)))
   "(sysprog (K S))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (v "lastone") (a S S)) S))
   "((lastone (S S)) S)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a I (a (v "lastone") K)) S) (a S (v "pp"))) (a (a I (a S S)) (a S K))))
   "((((lastone K) S) (S pp)) ((S S) (S K)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a (v "pl") S) K) K) (a (a (a (v "pp") (a S K)) I) (a (v "pl") K))))
   "((((pl S) K) K) (((pp (S K)) I) (pl K)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (a I (a I I))) (a (a (v "doo0") I) S)))
   "I"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (v "eec") (a I (a S I))) I) (a (a (a (a K K) I) (a (v "pl") (a S I))) (a (a (a I (v "doo0")) I) (v "swpp")))))
   "(((eec (S I)) I) (pl (S I)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (a (a S I) I) S) (a S (a K S))))
   "((S S) (S (K S)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a S (v "pl")) (a (a K K) (a I (a I (v "myrmidon"))))))
   "((S pl) K)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a K (a (a (v "myrmidon") (a (a (v "eec") (v "jangho")) S)) (v "hoonga"))))
   "(K ((myrmidon ((eec jangho) S)) hoonga))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (v "Medivh") (a K S)) (a (a (a (a I I) K) (a I I)) (v "lastone"))))
   "((Medivh (K S)) I)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (a (a K (a S (v "CSEseminar"))) (a (a S (v "eec")) (v "doo0")))) (a (v "jangho") S)))
   "(S CSEseminar)"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a I (a (a I S) S)) (a (a (v "physlab") K) (a (a K (a K S)) I))))
   "((S S) ((physlab K) (K S)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a (v "doo0") (a I I)) (a (a (a I I) (v "revreserver")) (a (v "CSEseminar") I))))
   "((doo0 I) (revreserver (CSEseminar I)))"
)))

(sgoutput
 (lambda ()
  (equal?
   (react (a (a K (v "os")) (a I S)))
   "os"
)))
