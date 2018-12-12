type direction = 
	| Up
	| Down
	| Left
	| Right;;

type word = {
    i: int;
    j:int;
    dir:direction;
    lettres:char array;
    domain:string list};;


let word_init x y d t dict =
  {i   = x;
   j   = y;
   dir = d;
   lettres  = Array.make t '?';
   domain = dict.(t)};;

   
   
let find_words g fd =
     
  let debut_hor = ref 0 in
  let debut_ver = ref 0 in
  let hor = ref [] in
  let ver = ref [] in
  let h = g.grid.height -1 in
  let w = g.grid.weight -1 in
  for j = 0 to h do
    for i = 0 to w do
      if ( g.grid.data == Shaded || i == w || j == h) then
        if( i - !debut_hor > 1) then
          incr debut_hor
            
          
    done;
  done;
;;
