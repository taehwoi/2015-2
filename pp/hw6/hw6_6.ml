exception TODO

type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
 and name = string

let rec checkMetro (m:metro): bool =
  let rec helper (m:metro) (al: name list): bool=
    match m with
      | STATION n -> List.mem n al
      | AREA (n,m) -> helper m (n::al)
      | CONNECT (m0,m1) -> helper m0 al && helper m1 al
  in helper m []
