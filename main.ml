(***********************************************************************)
(*                                                                     *)
(*         Crosswords (algorithm for generating crosswords)            *)
(*                                                                     *)
(*     Alexandre Loheac, Dina Capelle, Paul Watfeh, Maxime Le Du       *)
(*						Ecole Nationale de l'Aviation Civile  	       *)
(*                                                                     *)
(*  Copyright 2017 Ecole Nationale de l'Aviation Civile.               *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Library General Public License v3.                         *)
(*                                                                     *)
(***********************************************************************)


let () = 
	let grid = Grid.load_grid (open_in "grid.txt") in 
	let () = Grid.print_grid grid in
	
	let dict = Dict.load_dict (open_in "french.txt") in
	(*for i = 0 to dict.max_len-1 do 
		Printf.printf "%d\n" (List.length dict.dict.(i)) done;*)();
	
	let () = Display.open_window grid.width grid.height in 
	let () = Display.draw_grid () in
	let () = Display.display_grid grid in
	(*let () = Display.display_string 2 2 Display.Up "abc" in*)
	let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
        Grid.algo (Grid.find_words grid dict.dict) grid;;
	
