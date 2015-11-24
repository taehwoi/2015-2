open CommonGrade
open Hw7_2

let _ = output (fun () -> 
  "x" = 
    (SkiLiquid.pprint 
       (SkiLiquid.react 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.M (SkiLiquid.S,SkiLiquid.K),
                           SkiLiquid.I),
              SkiLiquid.V "x"))))
)

let _ = output (fun () -> 
  "x" = 
    (SkiLiquid.pprint
       (SkiLiquid.react 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.K,(SkiLiquid.V "x")),
              SkiLiquid.M (SkiLiquid.I,(SkiLiquid.V "x"))))))
)

let _ = output (fun () ->
  "(((x y) z) w)" =
    (SkiLiquid.pprint
       (SkiLiquid.M 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.V "x",SkiLiquid.V "y"),
              SkiLiquid.V "z"),
           SkiLiquid.V "w")))
)

module Ski = SkiLiquid

let _ = output (fun () ->
  "S" =
    (Ski.pprint 
    (Ski.S))
)
let _ = output (fun () ->
  "S" =
    (Ski.pprint 
    (Ski.react
    (Ski.S)))
)

let _ = output (fun () ->
  "(Hey Jude)" =
    (Ski.pprint 
    (Ski.react
      (Ski.M 
        ((Ski.M 
          ((Ski.M 
            ((Ski.M (Ski.S ,(Ski.M (Ski.K, (Ski.M (Ski.S,Ski.I)))))), 
            Ski.K)),
          (Ski.V "Jude"))), 
        (Ski.V "Hey"))))
))

let _ = output (fun () ->
  "((S K) S)" =
  (Ski.pprint 
    (Ski.react
      (Ski.M 
        ((Ski.I ,
          (Ski.M 
            ((Ski.M 
              ((Ski.M (Ski.I, Ski.S)) ,
            Ski.K), 
        Ski.S )) 
)))))))
