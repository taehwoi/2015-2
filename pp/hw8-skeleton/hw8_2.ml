module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module StringSetQ : Queue with type element = string = 
struct 
  exception EMPTY_Q
  type element = string
  type queue = 
    Q of element list * element list

  let emptyq = Q ([], [])

  let enq: queue * element -> queue =
    fun x -> 
      match x with
      (Q (l0, l1), e) -> 
        if (List.mem e l0 || List.mem e l1) 
        then Q (l0, l1)
        else Q (e::l0, l1)

  let rec deq: queue -> element * queue =
    fun q -> 
      match q with
      | Q ([], []) -> raise EMPTY_Q
      | Q (l0, []) -> deq (Q ([], List.rev l0))
      | Q (l0, h::t) -> (h, Q (l0, t))
end

module StringSetQQ : Queue with type element = StringSetQ.queue = 
struct 
  exception EMPTY_Q
  type element = StringSetQ.queue

  type queue = 
     Q of element list * StringSetQ.queue list

  let emptyq = Q ([], [])

  let rec queue2list (q:StringSetQ.queue) : string list =
    try let (e,r) = StringSetQ.deq q in
        e::(queue2list r)
    with StringSetQ.EMPTY_Q -> []

  let enq: queue * element -> queue =
    fun x -> 
      match x with
      (Q (l0, l1), e) ->
        if (List.mem (queue2list e) (List.map queue2list l0)
            || List.mem (queue2list e) (List.map queue2list l1))
        then Q (l0, l1)
        else Q (e::l0, l1)

  let rec deq: queue -> element * queue =
    fun q -> 
      match q with
      | Q ([], []) -> raise EMPTY_Q
      | Q (l0, []) -> deq (Q ([], List.rev l0))
      | Q (l0, h::t) -> (h, Q (l0, t))

end
