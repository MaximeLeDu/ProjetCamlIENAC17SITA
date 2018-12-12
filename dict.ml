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

module Int =
       struct
         type t = int
         let compare x y = Pervasives.compare x y
       end;;

module MyMap = Map.Make(Int);;

type dict = {
    mutable dict: 	string list array;
    mutable max_len:	int};;

let load_dict fd = 
	let dict = {dict = Array.make 1 [];
				 max_len = 1} in
	try
  		while true do
    		let line = String.trim (input_line fd) in
    		let n = String.length line in
    		if n >= dict.max_len then begin
    			dict.dict <- Array.append dict.dict (Array.make (n-dict.max_len+1) []);
    			dict.max_len <- Array.length dict.dict
    		end;
    		dict.dict.(n) <- line::dict.dict.(n)
  		done;
  		dict
	with End_of_file -> dict;;
(*
let load_dict_Map fd = 
	let m = ref MyMap.empty in
	try
  		while true do
    		let line = String.trim (input_line fd) in
    		let n = String.length line in
    		try let l = MyMap.find n !m in
    			m := MyMap.update n (fun _ -> (Some line)::l) !m
    		with Not_found -> m := MyMap.add n [line] !m
  		done
	with End_of_file -> !m;;*)
