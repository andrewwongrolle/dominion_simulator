(* utils.ml
 * utility functions 
 * *)

(* returns a function that applies a function "f" n times *)
let rec apply_n (n : int) (f : 'a -> 'a) : 'a -> 'a = 
    fun x -> if n > 0 then (apply_n (n-1) f) (f x) else x

(* Sort-based shuffle 
 * Generates random key for each element then sorts based on that key *)
let shuffle (l : 'a list) : 'a list =
    (* when two random keys are tied, choose one as larger randomly *)
    (* this avoids biased shuffle *)
    let compare_rand_tie x y = (
        match compare x y with
        | 0 -> if Random.bool() then 1 else -1 
        | _ -> compare x y ) 
    in
    let l_keys = List.map (fun x -> (Random.bits(), x )) l in
    let sorted = List.sort compare_rand_tie l_keys in
    List.map snd sorted
(* "compresses" l into a list of tuples of elements and their count in l
 * [a;b;a;c;c;c;d] -> [(a,2);(b,1);(c,3);(d,1)] *)
let compress (l : 'a list) (compare : 'a -> 'a -> int) : ('a * int) list =
    (* inserts elt x into a table with the element count *) 
    let rec insert_into (table : ('a * int) list) (x : 'a) =
        match table with
        | [] -> [(x, 1)]
        | (y, n) :: t -> 
            if (compare x y) = 0 then (y, n + 1) :: t
            else (y, n) :: (insert_into t x)
    in
    List.fold_left insert_into [] l
