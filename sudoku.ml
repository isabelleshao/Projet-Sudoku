open Graphics;;

type niveau = {
    mutable lvl: int;
    mutable joue:  int;
  } 
 
  
type sudo = {
    name : string;
    width:int; height:int; columnPx : int; linePx: int; (* taille en px du sudoku,taille en px/colonne et / lignes: *)
    scalePx:int;
    grid : int array array;
    modifiedGrid : int array array;
    reponseGrid :  int array array;
    mutable  colorGrid : color array array;
    mutable cpt : int ;
    mutable backtrack : (int*int*int) list (** coord x * coord y * data **);
    mutable saved : bool;
    level : niveau
  }
;;

