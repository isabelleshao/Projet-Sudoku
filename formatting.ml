open Graphics;;
open Sudoku;;
open Printf;;

(*** conversion txt en array***)
let txt_2_array entree =
  Array.map (function x -> int_of_string x) (Array.map (function x -> Char.escaped x)(Array.init 81 (fun t -> entree.[t])))
;;

(*** conversion array en array2d***)
let array_2_array2d array1d = 
  let dim = int_of_float (sqrt (float (Array.length array1d))) in
  Array.init dim (fun i -> Array.init dim (fun j -> array1d.(i * dim + j)))
;;

(***conversion array2d en array***)
let array2d_2_array array2d =
  let array1d =  Array.make 81 0 in 
  for i = 0 to 8 do
    for j = 0 to 8 do
      array1d.(9*j+i) <- array2d.(j).(i)
    done;
  done;
  array1d
;;

(***conversion array en string***)
let array_2_string f a =
  let s = Array.fold_left (fun s e -> s^(f e)) "" a in
  s
;;

let array_2_string2 a =
  let s = Array.fold_left (fun s e -> s^e^";") "" a in
  s
;;

let list_2_string2 a =
  let s = List.fold_left (fun s e -> s^e^";") "" a in
  s
;;






let color_2_string c =
  if c=cyan then "cyan"
  else if c=yellow then "yellow"
  else if c=red then "red"
  else if c=black then "black"
  else if c=magenta then "magenta"
  else if c=(rgb 220 220 220) then "(rgb 220 220 220)"
  else "white"

;;

let is_integer s =
    let is_int s =
      try ignore (int_of_string  (Char.escaped s)); true
      with _ -> false
    in
    if is_int s && s ='0' || s ='1' ||  s ='2' ||  s ='3' ||  s ='4' ||  s ='5' ||  s ='6' ||  s ='7' ||  s ='8' ||  s ='9' then true else false 
;;

let rec tuple_2_string =
    function
    | [] -> ""
    | (a, b, c) :: rest -> "(" ^string_of_int a^ "," ^string_of_int b^ "," ^string_of_int c^ ");"^tuple_2_string rest 
;;

let string_2_tuple t =
              let temp =Str.split (Str.regexp ")\\|(\\|,") t in
               (int_of_string (List.nth temp 0),int_of_string (List.nth temp 1),int_of_string (List.nth temp 2))
;;



      let rec truncate l acc  =
        if List.length acc <9 then
          truncate (List.tl l) ((string_of_int(List.hd l))::acc)
        else
          acc
    ;;
    
    
    (*** enregistrement string en txt***)
let string_2_txt grille =
  let oc = open_out ("sauvegardes/sauvegarde"^grille.name) in
  fprintf oc "%s\n" (array_2_string string_of_int (array2d_2_array  grille.modifiedGrid )) ;
  for i = 0 to 8 do
    fprintf oc "%s" (array_2_string2 (Array.map (fun a -> color_2_string a)  grille.colorGrid.(i)) )  ;
  done;

  fprintf oc "\n%s\n" (tuple_2_string  grille.backtrack)  ;
  fprintf oc "%s,%s\n" (string_of_int  grille.level.lvl) (string_of_int  grille.level.joue)   ;
    fprintf oc "%s\n" (array_2_string string_of_int (array2d_2_array  grille.reponseGrid )) ;
        fprintf oc "%s" (array_2_string string_of_int (array2d_2_array  grille.grid )) ;
  close_out oc;  
;;
