open Graphics;;
open Printf;;
open Formatting;;
open Sudoku;;
open Sat;;


(** Affichage de la liste des parties sauvegardés**) 
let display_liste_sauvegarde () = 
  
  let saut = ref 0 in 
  for i = 0 to  Array.length (Sys.readdir "./sauvegardes")-1  do
    if i mod 3 ==0 then saut := !saut+1; 
    moveto (140+(i mod 3)*200) (600-(!saut *30)) ;
    draw_string  (Sys.readdir "./sauvegardes").(i);
  done;
;;



let display_liste_grille () = 
  
  let saut = ref 0 in 
  for i = 0 to (* Array.length (Sys.readdir "./grids")-1*) 69  do
    if i mod 5 ==0 then saut := !saut+1; 
    moveto (140+(i mod 5)*110) (600-(!saut *30)) ;
    draw_string  (Sys.readdir "./grids").(i);
  done;
;;

let display_liste_minisat () = 
  
  let saut = ref 0 in 
  for i = 0 to 69 do
    if i mod 5 ==0 then saut := !saut+1; 
    moveto (140+(i mod 5)*110) (600-(!saut *30)) ;
    let name = "grid"^string_of_int(i) in 
    draw_string name;
  done; 
;;




(** retourne la liste des records **) 
let get_score () = 
  if (Sys.file_exists "score.txt") then
    let ic =  open_in ("score.txt") in
    let entree  =  input_line ic in
    flush stdout;
    close_in ic;
    let temp =  List.map (function x -> int_of_string x) (Str.split (Str.regexp ";") entree) in
    let sc =  List.rev( List.sort compare temp) in
    sc
  else []
;;


(*** enregistrement du score en txt***)

let score_2_txt score =

  let entree  =  get_score() in
  let oc = open_out ("score.txt") in
  let nouvList =
    if List.length entree > 9 then
      (list_2_string2( truncate entree [] ))^( string_of_int score )
    else
      list_2_string2( ( List.map (function x -> string_of_int x) entree))^( string_of_int score )
  in
    fprintf oc "%s\n" nouvList;
    close_out oc;;


(** generer les couleurs d'une grille de sudoku **)
let  get_color grille =
  let couleur = Array.make_matrix 9 9 white in
  for i = 0 to 8 do
    for j = 0 to 8 do
      if grille.(i).(j)==0 then
        couleur.(i).(j) <- (rgb 220 220 220)
      else
        ()
    done;
  done;
  couleur
;;


(** charger une grille de sudoku enregistré**)


let setSavedSudoku nom tailleSudoku= 
  

  let icSave = open_in ("sauvegardes/sauvegarde"^(nom)) in 
  let entreeSave  = array_2_array2d(txt_2_array (input_line icSave)) in
  let entreeColor  = Str.split (Str.regexp ";") (input_line icSave) in
  let entreeBacktrack  = Str.split (Str.regexp ";") (input_line icSave) in
  let entreeLvl  =  Str.split (Str.regexp ",") (input_line icSave) in
  let entreeReponses  =  array_2_array2d(txt_2_array (input_line icSave)) in
   let entree =  array_2_array2d(txt_2_array (input_line icSave)) in
  flush stdout;
  close_in icSave;

  
  let sudoku = {
      name = nom;
      width = tailleSudoku;
      height = tailleSudoku;
      columnPx = tailleSudoku/3;
      linePx = tailleSudoku/3;
      scalePx = tailleSudoku/9;
      grid =  entree;
      modifiedGrid = entreeSave ;
      reponseGrid =  entreeReponses;
      colorGrid =  array_2_array2d(
                       Array.of_list(
                           List.map (
                               fun c -> if c="cyan" then cyan
                                        else if c="yellow" then yellow
                                        else if c="magenta" then magenta
                                        else if c="(rgb 220 220 220)" then (rgb 220 220 220)
                                        else white )  entreeColor)  ); 
      cpt = 81;
      backtrack =   List.map ( fun t -> string_2_tuple t )  entreeBacktrack ;
      saved = false;
      level = {lvl=  int_of_string ( List.nth  entreeLvl 0) ;joue=  int_of_string (List.nth  entreeLvl 1)};
    } in
                 
  sudoku
;;


(** charger une nouvelle grille de sudoku **)
let setNewSudoku nom tailleSudoku lvl mode =
  if mode = "classique" then (
    let ic = open_in ("grids/grid"^(nom)) in 
    let entree  = array_2_array2d(txt_2_array (input_line ic)) in
    let ic2 = open_in ("grids/grid"^(nom)) in 
    let entree2  = array_2_array2d(txt_2_array (input_line ic2)) in
    let icSol =  open_in ("solutions/solution"^(nom)) in
    let entreeSol  = array_2_array2d(txt_2_array (input_line icSol)) in 
    flush stdout;
    close_in ic;
    close_in ic2;
    close_in icSol;
    (*print_endline entree; *)
    let sudoku = {
        name = nom;
        width = tailleSudoku;
        height = tailleSudoku;
        columnPx = tailleSudoku/3;
        linePx = tailleSudoku/3;
        scalePx = tailleSudoku/9;
        grid =  entree2;
        modifiedGrid = entree ;
        reponseGrid =  entreeSol;
        colorGrid =  (get_color entree);
        cpt = 81;
        backtrack =  []; 
        saved = false;
        level = {lvl=  lvl ;joue=  0};
      }
    in
    sudoku
 )else  if mode = "minisat" then (
            
    let entree  =  ( List.nth (grilles_de_sudoku "puzzles.sdk")  (int_of_string(nom)) ) in
    let entree2  = ( List.nth (grilles_de_sudoku "puzzles.sdk")  (int_of_string(nom)) ) in
    
    if (Sys.file_exists "puzzles_solution.sdk") <> true then
      minisat_resolution_file "puzzles.sdk" "puzzles_solution.sdk";
    
    let entreeSol  = ( List.nth (grilles_de_sudoku "puzzles_solution.sdk") (int_of_string(nom)) ) in
    (*print_endline entree; *)
    let sudoku = {
        name ="minisat"^nom^".txt";
        width = tailleSudoku;
        height = tailleSudoku;
        columnPx = tailleSudoku/3;
        linePx = tailleSudoku/3;
        scalePx = tailleSudoku/9;
        grid =  entree2;
        modifiedGrid = entree ;
        reponseGrid =  entreeSol;
        colorGrid =  (get_color entree);
        cpt = 81;
        backtrack =  []; 
        saved = false;
        level = {lvl=  lvl ;joue=  0};
      }
    in
    sudoku
  )else(
            
    let entree  =  ( List.nth (grilles_de_sudoku "puzzles_facile.sdk")  (int_of_string(nom)) ) in
    let entree2  = ( List.nth (grilles_de_sudoku "puzzles_facile.sdk")  (int_of_string(nom)) ) in
    
    if (Sys.file_exists "puzzles_solution.sdk") <> true then
     Sat.sat_resolution_file "puzzles_facile.sdk" "puzzles_facile_solution.sdk";
    
    let entreeSol  = ( List.nth (grilles_de_sudoku "puzzles_facile_solution.sdk") (int_of_string(nom)) ) in
    (*print_endline entree; *)
    let sudoku = {
        name ="sat"^nom^".txt";
        width = tailleSudoku;
        height = tailleSudoku;
        columnPx = tailleSudoku/3;
        linePx = tailleSudoku/3;
        scalePx = tailleSudoku/9;
        grid =  entree2;
        modifiedGrid = entree ;
        reponseGrid =  entreeSol;
        colorGrid =  (get_color entree);
        cpt = 81;
        backtrack =  []; 
        saved = false;
        level = {lvl=  lvl ;joue=  0};
      }
    in
    sudoku
  )

;;
