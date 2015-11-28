type design = TURTLE | WAVE | DRAGON    (* three design patterns *)
type orientation = NW | NE | SE | SW
type box = BOX of orientation * design | GLUED of box * box * box * box

module type FRAME =
sig
  val box: box
  val rotate: box -> box                (* roatate box M to 3 to W to E *)
  val pp: box -> int * int -> unit      (* pretty printer *)
  val size: int
end

module BasicFrame (Design: sig val design: design end): FRAME = 
struct 
  exception NON_BASIC_BOX
  let box = BOX (NW, Design.design)     (* a box is defined *)

  let rec rotate (b:box) : box = 
    match b with
    | BOX (NW, d)-> BOX (NE, d)
    | BOX (NE, d)-> BOX (SE, d)
    | BOX (SE, d)-> BOX (SW, d)
    | BOX (SW, d)-> BOX (NW, d)
    | _ -> raise NON_BASIC_BOX

  let pp b center = 
    match b with
    | BOX (NW,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (NE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SW,x) -> ()                  (* dummy, fill it if you want *)
    | _ -> raise NON_BASIC_BOX

  let size = 1
end

module Rotate (Box:FRAME) : FRAME =
struct 
  (*TODO*)
  let rec rotate (b:box) : box = 
    match b with
    | BOX (NW, d)-> BOX (NE, d)
    | BOX (NE, d)-> BOX (SE, d)
    | BOX (SE, d)-> BOX (SW, d)
    | BOX (SW, d)-> BOX (NW, d)
    (*FIXME*)
    | GLUED (b0, b1, b2, b3) -> GLUED (rotate b3, rotate b0, rotate b1, rotate b2)


  let box = rotate (Box.box)

  let size = 1 (*FIXME*)

  let pp b center = 
    match b with
    | BOX (NW,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (NE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SW,x) -> ()                  (* dummy, fill it if you want *)
    | _ -> ()
end

module Glue (Nw:FRAME) (Ne:FRAME) (Se:FRAME) (Sw:FRAME) : FRAME =
struct
  exception DIFFERENT_SIZED_BOXES

  let box = 
    GLUED (Nw.box, Ne.box, Se.box, Sw.box)

  let rec rotate (b:box) : box = 
    match b with
    | BOX (NW, d)-> BOX (NE, d)
    | BOX (NE, d)-> BOX (SE, d)
    | BOX (SE, d)-> BOX (SW, d)
    | BOX (SW, d)-> BOX (NW, d)
    (*FIXME*)
    | GLUED (b0, b1, b2, b3) -> GLUED (rotate b3, rotate b0, rotate b1, rotate b2)

  let size = 1 (*FIXME*)
  let pp b center = 
    match b with
    | BOX (NW,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (NE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SW,x) -> ()                  (* dummy, fill it if you want *)
    | _ -> ()
end
