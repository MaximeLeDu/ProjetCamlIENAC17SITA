(***********************************************************************)
(*                                                                     *)
(*         Crosswords (algorithm for generating crosswords)            *)
(*                                                                     *)
(*     Alexandre Loheac, Dina Capelle, Paul Watfeh, Maxime Le Du       *)
(*				  Ecole Nationale de l'Aviation Civile  			   *)
(*                                                                     *)
(*  Copyright 2017 Ecole Nationale de l'Aviation Civile.               *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Library General Public License v3.                         *)
(*                                                                     *)
(***********************************************************************)

let size = 15;;
let size_char_w = 5 and size_char_h = 12;;
let w,h = ref 0, ref 0;;
let c,r = ref 0, ref 0;;

let open_window col row = 
	w := col*size; c := col;
	h := row*size; r := row;
	Printf.printf "Opening Window %dx%d\n" !w !h;
	let s = Printf.sprintf " %dx%d" !w !h in
	Graphics.open_graph s;;
	
let draw_grid () = 
	Printf.printf "Drawing grid %dx%d...\n" !c !r;
	for j = 0 to !r-1 do
		for i = 0 to !c-1 do
			let x = i*size and y = j*size in begin
				Graphics.moveto x 0;
				Graphics.lineto x !h;
				Graphics.moveto 0 y;
				Graphics.lineto !w y;
			end
		done
	done;;

let display_char ?(col = (0,0,0)) i j c = 
	let r, g, b = col in Graphics.set_color (Graphics.rgb r g b);
	let x, y = (i)*size+(size-size_char_w)/2, !h-(j+1)*size+(size-size_char_h)/2 in begin
		Graphics.moveto x y;
		Graphics.draw_char c;
	end;;

let display_square ?(col = (0,0,0)) i j =
	let r, g, b = col in Graphics.set_color (Graphics.rgb r g b);
	let x, y = (i)*size, !h-(j+1)*size in
		Graphics.fill_rect x y size size;;

let display_grid ?(col = (0,0,0)) g = 
	for j = 0 to g.Grid.height-1 do
		for i = 0 to g.Grid.width-1 do
			let c = g.Grid.grid.(j).(i).data in
				if c = Grid.Shaded then display_square ~col:col i j
				else if c <> Grid.Empty then let Grid.Char(ch) = c in display_char ~col:col i j ch
		done
	done;;
