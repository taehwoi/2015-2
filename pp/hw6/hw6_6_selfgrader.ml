open CommonGrade
open Hw6_6

let m1 = AREA ("a", STATION "a")

let m2 = AREA ("a", AREA("a", STATION "a"))

let m3 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))

let m4 = AREA ("a", STATION "b")

let m5 = AREA("a", CONNECT(STATION "a",
			   AREA("b", STATION "c")))
let m6 =(STATION "c")
let m7 = (AREA ("e", STATION "e"))
let m8 = (AREA ("e", STATION "c"))
let m9 = (AREA ("a", CONNECT (AREA ("b", STATION "a"), AREA("c", STATION "c"))))
let m10 = (AREA ("a", CONNECT (AREA ("b", STATION "a"), AREA("c", STATION "b"))))
let m11 = (AREA ("a", CONNECT (AREA ("b", STATION "a"), AREA("c", STATION "a"))))
let m12 = (CONNECT (AREA ("a", STATION "a"), AREA ("b", STATION "b")))
let m13 = (AREA ("a", AREA ("b", CONNECT (STATION "b", STATION "a"))))
let m14 = (CONNECT (AREA ("b", 
                           CONNECT (STATION "b", 
                            AREA ("d", STATION "b"))), 
                     AREA ("e", STATION "e")))
let m15 = (CONNECT (AREA ("b", 
                           CONNECT (STATION "b", 
                            AREA ("d", STATION "d"))), 
                     AREA ("e", STATION "b")))

let _ = output (fun () -> (checkMetro m1 = true))
let _ = output (fun () -> (checkMetro m2 = true))
let _ = output (fun () -> (checkMetro m3 = true))
let _ = output (fun () -> (checkMetro m4 = false))
let _ = output (fun () -> (checkMetro m5 = false))

let _ = output (fun () -> (checkMetro m6 = false))
let _ = output (fun () -> (checkMetro m7 = true))
let _ = output (fun () -> (checkMetro m8 = false))
let _ = output (fun () -> (checkMetro m9 = true))
let _ = output (fun () -> (checkMetro m10 = false))
let _ = output (fun () -> (checkMetro m11 = true))
let _ = output (fun () -> (checkMetro m12 = true))
let _ = output (fun () -> (checkMetro m13 = true))
let _ = output (fun () -> (checkMetro m14 = true))
let _ = output (fun () -> (checkMetro m15 = false))
 
