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


let empty_char = '_' and
	shaded_char = '#';;
	
type data_type = | Empty | Shaded | Char of char;;
type style_type = | Fixed | Changeable;;

type direction = 
	| Up
	| Down
	| Left
	| Right;;

type box = {
	mutable data:	data_type;
    mutable style: 	style_type;
    mutable word :	(direction * int * int * int) list};;
    
type grid = {
    grid: 	box array array;
    width:	int;
    height:	int};;

let is_in_bounds i j grid = j >= grid.height || i >= grid.width;;

let load_grid fd = 
	let rec aux l = 
		try aux ((String.trim (input_line fd))::l)
		with End_of_file -> List.rev l in
	let raw = aux [] in
	
	let l = List.map (fun a -> String.length a) raw in
	let w = List.fold_left max 0 l
	and h = List.length raw in
	let g = {grid = Array.make h [||];
			 width = w;
			 height = h} in
	for j = 0 to h-1 do g.grid.(j) <- Array.init w (fun _ -> {data = Empty;
													style = Changeable;
													word =	[]}) done;
	
	let rec browse j r = match r,j with
		| [],_ -> g
		| s::tl,_ -> for i = 0 to w-1 do 
						let c = String.get s i in
						if c <> empty_char then g.grid.(j).(i).style <- Fixed;
						if c = empty_char then g.grid.(j).(i).data <- Empty
						else if c = shaded_char then g.grid.(j).(i).data <- Shaded
						else g.grid.(j).(i).data <- Char(c) done;
						browse (j+1) tl in
	browse 0 raw;;

let print_grid g = 
	for j = 0 to g.height-1 do
		for i = 0 to g.width-1 do match g.grid.(j).(i).data with
			| Empty -> Printf.printf " "
			| Shaded -> Printf.printf "%c" (Char.chr 219)
			| Char(c) -> Printf.printf "%c" c 
		done;
		Printf.printf "\n" done;;

type word = {
    i: int;
    j:int;
    dir:direction;
    size:int;
    domain:string list};;


let word_init x y d t dict =
  {i   = x;
   j   = y;
   dir = d;
   size = t;
   domain = dict.(t)};;

let update_word w l i =
  let rec update_dom dom = match dom with
  |[]     -> []
  |hd::tl -> if hd.[i] == l then hd::(update_dom tl) else update_dom tl
  in
  {i   = w.i;
   j   = w.j;
   dir = w.dir;
   size = w.size;
   domain = update_dom w.domain};;


let find_words g dict =
  let debut_hor = ref 0 in
  let debut_ver = ref 0 in
  let hor = ref [] in
  let ver = ref [] in
  let h = g.height -1 in
  let w = g.width -1 in
  for j = 0 to h do
    for i = 0 to w do
      if ( g.grid.(j).(i).data == Shaded || i == w) then begin
        if( i - !debut_hor > 1) then begin
          hor :=(word_init !debut_hor j Left (i - !debut_hor) dict)::(!hor);
          for k=(!debut_hor) to i-1 do
            g.grid.(j).(k).word <- (Left,!debut_hor,j,k- !debut_hor )::g.grid.(j).(k).word;
          done;
        end;
        debut_hor := (!debut_hor +1) mod h;
      end;
    done;
  done;
  for i=0 to w do
    for j=0 to h do
        
      if ( g.grid.(j).(i).data == Shaded || j == h) then begin
        if( j - !debut_ver > 1) then begin
          ver := (word_init !debut_ver i Down (j - !debut_ver) dict)::(!ver);
          for k=(!debut_ver) to j-1 do
            g.grid.(k).(i).word <- (Left,i,!debut_ver,k- !debut_hor )::g.grid.(k).(i).word;
          done;
        end;
        debut_ver := (!debut_ver +1) mod w;
      end;
    done;
  done;
  !hor@ !ver;
;;

let reduce_domain word letter g=
  let (_,i,j,k) = letter in
  let Char(l) = g.grid.(j).(i).data in
  let rec red_dom_rec domain = match domain with
  |[]     -> []
  |hd::tl -> if(hd.[k]==l) then hd::(red_dom_rec tl) else red_dom_rec tl
  in
  {i=word.i;
   j=word.j;
   dir=word.dir;
   size=word.size;
   domain=red_dom_rec word.domain};;

let update_grid l tl g =
  let rec update_grid_rec t = match t with
  |[] -> []
  |hd1::tl1 ->
      let rec upd_rec l1 = 
        match l1 with
        |[] -> hd1::update_grid_rec tl1 
        |hd2::tl2 ->
            let (d,i,j,_) = hd2 in
            if(i == hd1.i && j == hd1.j && d == hd1.dir) then (reduce_domain hd1 hd2 g)::(update_grid_rec tl1) else upd_rec tl2
      in upd_rec l
  in update_grid_rec tl;;

let update_word_1 hd hd1 g=
  let li = ref [] in
  if (hd.dir = Left||hd.dir = Right) then begin
    for i=hd.i to hd.i+hd.size-1 do
      g.grid.(hd.j).(i).data <- Char(hd1.[i-hd.i]);
      match g.grid.(hd.j).(i).word with
      |word1::word2::[] -> li:=word2::(!li)
      |word::[]         -> let (dir,_,_,_) = word in if (dir = Down||dir = Up) then li:=word::(!li)
      |_                -> ()
    done;
  end;
  if (hd.dir = Down||hd.dir = Up) then begin
    for j=hd.j to hd.j+hd.size-1 do
      g.grid.(j).(hd.i).data <- Char(hd1.[j-hd.j]);
      match g.grid.(j).(hd.i).word with
      |word1::word2::[] -> li:=word1::(!li)
      |word::[]         -> let (dir,_,_,_) = word in if (dir = Left||dir = Right) then li:=word::(!li)
      |_                -> ()
    done;
  end;
  !li
;;

exception MotVide;;

let rec algo s g = match s with
  |[] -> print_grid g
  |hd::tl ->
      let rec fonc dom =
        try
          match dom with
          |[] -> raise MotVide
          |hd1::tl1 -> let l = update_word_1 hd hd1 g in begin algo (update_grid l tl g) g; fonc tl1;end;
        with MotVide -> ()
      in fonc hd.domain
;;
