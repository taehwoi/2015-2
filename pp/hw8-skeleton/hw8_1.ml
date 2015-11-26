module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module StringQ : Queue with type element = string = 
struct 
  exception EMPTY_Q
  type element = string
  type queue = 
    | Q of string list * string list

  let emptyq = Q ([], [])

  let enq: queue * element -> queue =
    fun x -> 
      match x with
      | (Q (l0, l1), e) -> Q (e::l0, l1)

  let  deq: queue -> element * queue =
    fun q -> 
      match q with
      | Q (l0, l1) -> 
        (match l1 with 
        | [] -> raise EMPTY_Q
        | h::t -> 
            if (List.length t = 1) 
            then (h, Q ([], List.rev l0))
            else (h, Q (l0, t)))
end

(*module StringQQ : Queue with type element = StringQ.queue = *)
(*struct *)
(*end*)
