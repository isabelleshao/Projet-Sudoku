  type cellule = { i : int; j : int }
  type atome = { cellule : cellule; d : int; signe : bool }
  type clause = atome list
  type fnc = clause list
  type grille = int array array

  val grilles_de_sudoku : string -> grille list
  val affiche_grille : grille -> unit
  val definition_des_cellules : grille -> fnc
  val unicite_des_cellules : grille -> fnc
  val validite : grille -> int -> int -> int -> int -> fnc
  val validite_des_cellules_horizontale : grille -> fnc
  val validite_des_cellules_verticale : grille -> fnc
  val validite_des_cellules_bloc : grille -> fnc
  val validite_des_cellules : grille -> fnc
  val fnc_de_grille : grille -> fnc
  val liste_fnc_de_liste_grilles : grille list -> fnc list
  val un_fichier_dimacs : fnc -> int -> unit
  val tous_les_fichiers_dimacs : fnc list -> unit
  val affiche_grilles : grille list -> unit
  val minisat_fichiers_solutions : grille list -> unit
  val liste_solutions : string -> int list
  val resolution_grille : grille -> int list -> unit
  val minisat_resolution_grilles : grille list -> unit
  val fichiers_finaux : grille list -> string -> unit
  val minisat_resolution_file : string -> string -> unit
  val interp_string : int array -> string
  val fnc_bcp_v : int array -> int list list -> int list list * bool
  val fnc_bcp_f : int array -> int list list -> int list list * bool
  val fnc_assume : int array -> int list list -> int list list * bool
  val fnc_unsat : int array -> int array * int array
  val fnc_sol : int -> int list list -> string
  val lire_fnc : string -> int list list * int
  val sol_sudoku : string -> string -> unit
  val sat_fichiers_solutions : grille list -> unit
  val sat_resolution_grilles : grille list -> unit
  val sat_resolution_file : string -> string -> unit
