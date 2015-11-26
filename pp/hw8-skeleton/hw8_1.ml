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
    Q of element list * element list

  let emptyq = Q ([], [])

  let enq: queue * element -> queue =
    fun x -> 
      match x with
      (Q (l0, l1), e) -> Q (e::l0, l1)

  let rec deq: queue -> element * queue =
    fun q -> 
      match q with
      | Q ([], []) -> raise EMPTY_Q
      | Q (l0, []) -> deq (Q ([], List.rev l0))
      | Q (l0, h::t) -> (h, Q (l0, t))

end

module StringQQ : Queue with type element = StringQ.queue = 
struct 
  exception EMPTY_Q
  type element = StringQ.queue

  type queue = 
     Q of element list * element list

  let emptyq = Q ([], [])

  let enq: queue * element -> queue =
    fun x -> 
      match x with
      (Q (l0, l1), e) -> Q (e::l0, l1)

  let rec deq: queue -> element * queue =
    fun q -> 
      match q with
      | Q ([], []) -> raise EMPTY_Q
      | Q (l0, []) -> deq (Q ([], List.rev l0))
      | Q (l0, h::t) -> (h, Q (l0, t))
end
