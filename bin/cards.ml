(* cards.ml
 * A simulator for the board game Dominion written in OCaml
 * Defines types representing cards, players, game state
 * Many objects were harmed in the making of this simulator
 * *)

open Classes

let _add_money (n : int) : (state -> unit) = fun s -> s#active_player#add_money n
let _add_vp (n : int) : (card list -> int) = fun _ -> n
let no_vp = fun _ -> 0
let no_fx = fun _ -> ()

let copper = new card "copper" 0 (_add_money 1) no_vp 
let silver = new card "silver" 3 (_add_money 2) no_vp 
let gold = new card "gold" 6 (_add_money 3) no_vp 

let estate = new card "estate" 2 no_fx (_add_vp 1) 
let duchy = new card "duchy" 5 no_fx (_add_vp 3) 
let province = new card "province" 8 no_fx (_add_vp 6) 

let smithy = new card "smithy" 4 
    (fun s -> let _ = s#active_player#use_action in s#active_player#draw_n 3) 
    no_vp
