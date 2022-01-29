(* dominion_simulator.ml
 * A simulator for the board game Dominion written in OCaml
 * Defines types representing cards, players, game state
 * Many objects were harmed in the making of this simulator
 * *)

open Lib

let log s = print_endline s

class player (n : string) (d : card list) r1 r2 r3 r4 r5 r6 =
    object (self)
        method name = n
        val mutable turns = 0
        method turns = turns
        val mutable hand : card list = []
        method hand = hand
        val mutable discard : card list = []
        val mutable deck : card list = d 
        val mutable in_play : card list = []
        val mutable actions = 1;
        method add_actions n = actions <- actions + n 
        method use_action = actions <- actions - 1
        method get_actions = actions
        val mutable money = 0;
        method add_money n = money <- money + n 
        method money = money
        val mutable buys = 1;
        method add_buy n = buys <- buys + n 
        method buys = buys
        method action_rule : state -> card option = r1
        method treasure_rule: state -> card option = r2
        method buy_rule : state -> card option = r3
        method gain_rule : state -> card option = r4
        method trash_rule : state -> card option = r5
        method discard_rule : state -> card option = r6
        method reshuffle = 
            let () = deck <- List.append deck (Utils.shuffle discard) in
            let () = discard <- [] in
            log (self#name ^ " reshuffles.")
        method draw = 
            match deck with 
            | top_card :: rest -> (
                let () = deck <- rest in 
                let () = hand <- top_card :: hand in
                log (self#name ^ " draws a " ^ top_card#name) )
            | [] -> (
                if List.length discard = 0 then () 
                else 
                    let () = (self#reshuffle) in 
                    self#draw )
        method draw_n n = 
            if n < 1 then raise (Invalid_argument "drawing < 1 card")
            else if n = 1 then self#draw 
            else let _ = self#draw in self#draw_n (n - 1)
        method draw_hand = 
            match deck with
            | c1 :: c2 :: c3 :: c4 :: c5 :: rest -> 
                let () = deck <- rest in
                let () = hand <- [c1;c2;c3;c4;c5] in 
                log (self#name ^ " draws " ^
                    (String.concat ", " [c1#name;c2#name;c3#name;c4#name;c5#name]) )
            | _ -> 
                let () = self#draw in 
                let () = self#draw in 
                let () = self#draw in 
                let () = self#draw in 
                self#draw
        method private add_to_play (c : card) = 
            let () = in_play <- c :: in_play in
            log (self#name ^ " plays a " ^ c#name)
        (* removes a card c from hand *)
        method private remove_from_hand (c : card) =
            let (same, diff) = List.partition (fun x -> c#equals x) hand in
            hand <- (List.tl same) @ diff
        (* puts card from hand into play and carries out its effects *)
        method play_card (s : state) (c : card) = 
            let () = self#remove_from_hand c in
            let () = self#add_to_play c in
            c#play s
        method gain_card (s : state) (c : card) = 
            let () = s#remove_from_supply c in
            discard <- c :: discard
        method buy_card (s : state) (c : card) = 
            if c#cost > money then raise (Invalid_argument "not enough money")
            else 
                let () = self#gain_card s c in
                let () = money <- money - c#cost in
                log (self#name ^ " buys a " ^ c#name)
        method cleanup = 
            (* put all cards in play and in hand into discard *)
            let () = discard <- List.concat [discard; in_play; hand] in
            let () = in_play <- [] in
            let () = hand <- [] in
            (* reset actions, money, buys, turns, draw a new hand *)
            let () = actions <- 1 in
            let () = money <- 0 in
            let () = buys <- 1 in
            let () = turns <- turns + 1 in
            self#draw_hand
        (* find the number of cards "c" player owns *)
        method num_card (c : card) = 
            let all_cards = deck @ hand @ discard @ in_play in
            List.length (List.find_all (fun x -> c#equals x) all_cards)
        (* String of player name, deck, victory points *)
        method stringify = 
            let all_cards = deck @ hand @ discard @ in_play in
            let names = List.map (fun x -> x#name) all_cards in
            let points = List.fold_left (fun a x -> a + (x#vp all_cards) ) 0 all_cards in
            let f a b = a ^ fst b ^ ": " ^ string_of_int (snd b) ^ "\n" in
            "Player: " ^ self#name ^ "\n" ^
            "Turns: " ^ string_of_int self#turns ^ "\n" ^
            "Points: " ^ string_of_int points ^ "\n" ^
            "Deck:\n" ^ 
            (List.fold_left f "" (Utils.compress names compare))
    end 
and state (ps : player list) (kingdom : (card * int) list) (end_cond : ((card * int) list) -> bool) =
    object
        val mutable supply = kingdom 
        method in_supply (c : card) : int = 
            match (List.find_opt (fun x -> c#equals (fst x)) supply) with
            | None -> 0
            | Some p -> snd p
        method remove_from_supply (c : card) = 
            match List.partition (fun x -> c#equals (fst x)) supply with
            | ([(c', n)], l) -> supply <- (c', n - 1) :: l
            | _ -> raise (Invalid_argument ("card " ^ c#name ^ " not in supply"))
        val mutable game_over = false
        method game_over = game_over 
        val mutable trash : card list = []
        val mutable active_player = List.hd ps
        method active_player = active_player 
        val mutable inactive_players = List.tl ps
        method inactive_players = inactive_players 
        method pass_turn = 
            (* if game over conditions are met, end the game *)
            if end_cond supply then 
                let () = log "Game Over." in 
                let () = log active_player#stringify in
                let () = List.fold_left (fun _ p -> log p#stringify) () inactive_players in
                game_over <- true
            else 
            let (h, t) = (List.hd inactive_players, List.tl inactive_players) in
            let () = inactive_players <- List.append t [active_player] in
            let () = active_player <- h in
            log (active_player#name ^ " begins their turn " ^ string_of_int active_player#turns)
    end
and card (n : string) (c : int) (on_play : state -> unit) (worth : card list -> int) = 
    object(self) 
        method name = n
        method print = print_endline self#name
        method cost = c
        method equals (c : card) : bool = (self#name = c#name)
        method play = on_play 
        method vp = worth
    end
