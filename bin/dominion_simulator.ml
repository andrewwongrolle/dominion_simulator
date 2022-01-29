(* dominion_simulator.ml
 * A simulator for the board game Dominion written in OCaml
 * Defines types representing cards, players, game state
 * Many objects were harmed in the making of this simulator
 * *)

open Classes
open Cards
open Lib

let _ = Random.self_init()

let first_supply : (card * int) list = [
    (copper, 60);
    (silver, 40);
    (gold, 30);
    (province, 8);
    (duchy, 8);
    (estate, 8);
    (smithy, 10)
]
let deck = [copper; copper; copper; copper; copper; copper; copper; estate; estate; estate]
let bm_buy (s : state) : card option = 
    let money = s#active_player#money in
    if money >= 8 && s#in_supply province > 0 then Some province 
    else if money >= 6 && s#in_supply gold > 0 then Some gold 
    else if money >= 3 && s#in_supply silver > 0 then Some silver 
    else None 
let bm_treasure (s : state) : card option =
    let c = List.find_opt copper#equals s#active_player#hand in
    let s' = List.find_opt silver#equals s#active_player#hand in
    let g = List.find_opt gold#equals s#active_player#hand in
    match (c, s', g) with
    | (Some _, _, _) -> c
    | (_, Some _, _) -> s'
    | (_, _, Some _) -> g
    | _ -> None 
let bm_action : state -> card option = fun _ -> None
let bm_gain : state -> card option = fun _ -> None
let bm_trash : state -> card option = fun _ -> None
let bm_discard : state -> card option = fun _ -> None

let smithy_buy (s : state) : card option = 
    let money = s#active_player#money in
    let smithies = s#active_player#num_card smithy in
    if money >= 8 && s#in_supply province > 0 then Some province 
    else if money >= 6 && s#in_supply gold > 0 then Some gold 
    else if money >= 4 && s#in_supply smithy > 0 && smithies < 2 then Some smithy 
    else if money >= 3 && s#in_supply silver > 0 then Some silver 
    else None 

let smithy_action (s : state) : card option = 
    let p = s#active_player in 
    if p#get_actions > 0 && List.exists smithy#equals p#hand then 
        Some smithy
    else bm_action s

let bm_1 = new player "Big Money 1" (Utils.shuffle deck) bm_action bm_treasure bm_buy bm_gain bm_trash bm_discard
let bm_2 = new player "Big Money 2" (Utils.shuffle deck) bm_action bm_treasure bm_buy bm_gain bm_trash bm_discard
let smithy_bm = new player "Smithy Money" (Utils.shuffle deck) smithy_action bm_treasure smithy_buy bm_gain bm_trash bm_discard

let province_or_pileout supply =
    let provinces = snd (List.find (fun x -> province#equals (fst x)) supply) in
    let empty_piles = List.length (List.filter (fun x -> (snd x) = 0) supply) in
    (provinces = 0 || empty_piles >= 3)

let max_turns = 100

let rec play (s : state) : unit = 
    let p = s#active_player in
    if s#game_over || p#turns > max_turns then () else
    match (p#action_rule s) with
    | Some a ->
        let () = p#play_card s a in 
        play s
    | None -> 
        match (p#treasure_rule s) with
        | Some t ->
            let () = p#play_card s t in 
            play s
        | None ->
            match (p#buy_rule s) with
            | Some b ->
                let () = p#buy_card s b in
                play s
            | None ->
                let () = p#cleanup in
                let () = s#pass_turn in
                play s

let () = bm_1#draw_hand
let () = bm_2#draw_hand
let () = smithy_bm#draw_hand
let first_state = new state [bm_1;smithy_bm] first_supply province_or_pileout
let () = play first_state 
