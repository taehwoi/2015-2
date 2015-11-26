module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module type ArgTy = 
sig
  type t
  val is_eq : t -> t -> bool
end

module QueueMake (Arg: ArgTy) 
  : Queue with type element = Arg.t =
struct
  exception EMPTY_Q
  type element = Arg.t
  type queue = 
    Q of element list * element list
  let emptyq = Q ([], [])

  let enq: queue * element -> queue =
    fun x -> 
      match x with
      (Q (l0, l1), e) -> 
          Q (e::l0, l1)

  let rec deq: queue -> element * queue =
    fun q -> 
      match q with
      | Q ([], []) -> raise EMPTY_Q
      | Q (l0, []) -> deq (Q ([], List.rev l0))
      | Q (l0, h::t) -> (h, Q (l0, t))
end

(*if ( (List.fold_left (||) false (List.map (fun x -> Arg.is_eq e x) l0))*)
(*|| (List.fold_left (||) false (List.map (fun x -> Arg.is_eq e x) l1)))*)
(*then Q (l0, l1)*)
(*else *)
