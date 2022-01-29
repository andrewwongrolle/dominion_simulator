val apply_n : int -> ('a -> 'a) -> ('a -> 'a)
val shuffle : 'a list -> 'a list
val compress : 'a list -> ('a -> 'a -> int) -> ('a * int) list
