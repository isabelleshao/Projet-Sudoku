  type cellule = { i : int; j : int };;
  type atome = { cellule : cellule; d : int; signe : bool };;
  type clause = atome list;;
  type fnc = clause list;;
  type grille = int array array;;


  (*| Charge les grilles de sudoku à partir du fichier 'puzzles.sdk'
    | Paramètres : nom_fichier [in], Le nom du fichier contenant les grilles
    | Renvoie : La liste des grilles*)
  let grilles_de_sudoku nom_fichier =
    let file = open_in nom_fichier in
    let rec f acc =
      try
        let s = (input_line file) in
        try
          let start = Str.search_forward (Str.regexp "[0-9]+") s 0 in
          f ((Array.init 9 (fun i -> (Array.init 9 (fun j -> int_of_string (String.sub s (start+(i*9)+j) 1)))))::acc)
        with Not_found -> f acc
      with End_of_file -> (close_in file; acc)
    in List.rev (f []);;


  (*| Affiche une grille de Sudoku dans le terminal
    | Paramètres : g [in], Une grille
    | Renvoie : Rien*)
  let affiche_grille g =
    Printf.printf "-------------\n";
    for i = 0 to 8 do
      Printf.printf "|";
      for j = 0 to 8 do
        Printf.printf "%d" g.(i).(j);
        if (((j+1) mod 3) = 0) then Printf.printf "|" else ();
      done;
      Printf.printf "\n";
      if (((i+1) mod 3) = 0) then Printf.printf "-------------\n" else ();
    done;;


  (*| Calcule la FNC de la formule décrivant la bonne définition des cellules
    | Paramètres :g [in], Une grille
    | Renvoie : La FNC de la formule*)
  let definition_des_cellules g =
    let clause i j =
      let rec f k acc =
        if k>9 then acc else f (k+1) (({ cellule = { i = i+1; j = j+1 }; d = k; signe = true })::acc)
      in f 1 []
    in
    let res = ref [] in
    for i = 0 to 8 do
      for j = 0 to 8 do
        if g.(i).(j) = 0 then
          res := (clause i j)::(!res)
        else ()
      done
    done;
    !res;;


  (*| Calcule la FNC de la formule décrivant l'unicité des cellules
    | Paramètres : g [in], Une grille
    | Renvoie : La FNC de la formule*)
  let unicite_des_cellules g =
    let res = ref [] in
    for i = 0 to 8 do
      for j = 0 to 8 do
        if g.(i).(j) = 0 then
          for k = 1 to 9 do
            for l = (k+1) to 9 do
              res := (({ cellule = { i = i+1; j = j+1 }; d = k; signe = false })::({ cellule = { i = i+1; j = j+1 }; d = l; signe = false })::[])::(!res)
            done
          done
        else ()
      done
    done;
    !res;;


  (*| Calcule la FNC de la formule décrivant la validité d'une série donnée de cellules et l'insère dans une autre FNC
    | Paramètres : g [in], Une grille
                   l1 [in], La ligne de départ
                   l2 [in], La ligne d'arrivée
                   c1 [in], La colonne de départ
                   c2 [in], La colonne d'arrivée
                   res [in/out], La liste de clauses à compléter
    | Renvoie : rien*)
  let validite g l1 l2 c1 c2 =
    let res = ref [] in
    for i = l1 to l2 do
      for j = c1 to c2 do
        if (g.(i).(j) = 0) then
          for m = (if j = c2 then i+1 else i) to l2 do
            for n = (if (not (m=i)) then c1 else j+1) to c2 do
              if (g.(m).(n) = 0) then
                for k = 1 to 9 do
                  let clause = { cellule = { i = i+1; j = j+1 }; d = k; signe = false }::{ cellule = { i = m+1; j = n+1 }; d = k; signe = false }::[] in
                  res := clause::(!res)
                done
              else
                let clause = { cellule = { i = i+1; j = j+1 }; d = g.(m).(n); signe = false }::[] in
                res := clause::(!res)
            done
          done
        else
          for m = (if j = c2 then i+1 else i) to l2 do
            for n = (if (not (m=i)) then c1 else j+1) to c2 do
              if (g.(m).(n) = 0) then
                let clause = { cellule = { i = m+1; j = n+1 }; d = g.(i).(j); signe = false }::[]
                in res := clause::(!res)
              else ()
            done
          done
      done
    done;
    !res;;


  (*| Calcule la FNC de la formule décrivant la validité des celulles ligne par ligne
    | Paramètres : g [in], Une grille
    | Renvoie : La FNC de la formule*)
  let validite_des_cellules_horizontale g =
    let rec f i acc =
      if (i > 8) then acc else f (i+1) (List.rev_append (validite g i i 0 8) acc)
    in f 0 [];;


  (*| Calcule la FNC de la formule décrivant la validité des cellules colonne par colonne
    | Paramètres : g [in], Une grille
    | Renvoie : La FNC de la formule*)
  let validite_des_cellules_verticale g =
     let rec f j acc =
      if (j > 8) then acc else f (j+1) (List.rev_append (validite g 0 8 j j) acc)
    in f 0 [];;


  (*| Calcule la FNC de la formule décrivant la validité des cellules bloc par bloc
    | Paramètres : g [in], Une grille
    | Renvoie : La FNC de la formule*)
  let validite_des_cellules_bloc g =
    let rec f k l acc =
      if (k > 2) then acc else if (l >= 2) then f (k+1) 0 (List.rev_append (validite g (k*3) ((k*3)+2) (l*3) ((l*3)+2)) acc) else f k (l+1) (List.rev_append (validite g (k*3) ((k*3)+2) (l*3) ((l*3)+2)) acc)
    in f 0 0 [];;


  (*| Calcule la FNC de la formule décrivant la validité des cellules
    | Paramètres : g [in], Une grille
    | Renvoie : La FNC de la formule*)
  let validite_des_cellules g = List.rev_append (List.rev_append (validite_des_cellules_bloc g) (validite_des_cellules_verticale g)) (validite_des_cellules_horizontale g);;


  (*| Calcule la FNC de la formule décrivant une grille de sudoku conforme et valide
    | Paramètres : g [in],  Une grille
    | Renvoie : La FNC de la formule*)
  let fnc_de_grille g = List.rev_append (List.rev_append (validite_des_cellules g) (unicite_des_cellules g)) (definition_des_cellules g);;


  (*| Calcule la liste des FNC des formules correspondant à une liste de grilles de sudoku conformes et valides
    | Paramètres : l [in] Une liste de grilles
    | Renvoie : La liste des FNC des formules*)
  let liste_fnc_de_liste_grilles l =
    let rec f acc l =
      match l with
      | [] -> acc
      | x::s -> f ((fnc_de_grille x)::acc) s
    in List.rev (f [] l);;


(*| Ecrit un fichier dimacs contenant la FNC d'une formule donnée et dont le nom désigne une grille spécifiée par son indice
    | Paramètres : l [in], Une FNC d'une formule
                   ind [in], un entier
    | Renvoie : rien*)
  let un_fichier_dimacs l ind =
    let nb_clauses = List.length l in
    let file = open_out ("dimacs/grille"^(string_of_int ind)^".dimacs")  in
    let rec dimacs_clause c =
      match c with
      | [] -> output_string file "0\n"
      | x::s -> if (not x.signe) then output_char file '-' else (); output_string file ((string_of_int ((x.cellule.i-1)*81+(x.cellule.j-1)*9+x.d))^" "); dimacs_clause s
    in let rec dimacs_fnc fnc = 
      match fnc with
      | [] -> close_out file
      | x::s -> dimacs_clause x; dimacs_fnc s
    in (output_string file ("p cnf 729 "^(string_of_int nb_clauses)^"\n"); dimacs_fnc l);;


  (*| Ecrit tous les fichiers dimacs correspondant à une liste de FNC de formules
    | Paramètres : l [in], Une liste de FNC de formules
    | Renvoie : rien*)
  let tous_les_fichiers_dimacs l =
    let rec f l nb =
      match l with
      | [] -> ()
      | x::s -> un_fichier_dimacs x nb; f s (nb+1)
    in f l 0;;


  (*| Affiche une liste de grilles de sudoku dans le terminal
    | Paramètres : l [in], Une liste de grilles
    | Renvoie : rien*)
  let rec affiche_grilles l = 
    match l with
    | [] -> ()
    | x::s -> affiche_grille x; Printf.printf "\n"; affiche_grilles s;;


  (*| Resoud une liste de grilles de sudoku et écrit les solutions dans des fichiers
    | Paramètres : l [in], Une liste de grilles
    | Renvoie : rien*)
  let minisat_fichiers_solutions l =
    let l_fnc = (liste_fnc_de_liste_grilles l) in tous_les_fichiers_dimacs l_fnc;
    for i=0 to ((List.length l_fnc)-1) do
      Sys.command ("./MiniSat_v1.14_linux dimacs/grille"^(string_of_int i)^".dimacs dimacs/grille"^(string_of_int i)^"solution")
    done;;


  (*| Récupère la solution d'une grille de sudoku qui est écrite dans un fichier
    | Paramètres : nom_fichier [in], Le nom d'un fichier
    | Renvoie : Une liste d'entiers correspondant aux valeurs manquantes*)
  let liste_solutions nom_fichier =
    let file = open_in nom_fichier in
    try
      match input_line file with
      | "SAT" -> let l = (Str.split (Str.regexp " ") (String.trim (input_line file))) in List.map (fun x -> if ((int_of_string x) mod 9)=0 then 9 else ((int_of_string x) mod 9)) (List.filter (fun x -> not (x.[0] = '-')) l)
      | _ -> (close_in file; failwith "Format incorrect") 
    with End_of_file -> (close_in file; []);;


  (*| Change les valeurs d'une grille de sudoku donnée pour y insérer les valeurs manquantes données en paramètres
    | Paramètres : g [in/out], Une grille
                   sol [in], une liste d'entiers*
    | Renvoie : rien*)
  let resolution_grille g sol =
    let rec f g sol i j =
      if (g.(i).(j)=0) then
        begin
          match sol with
          | [] -> ()
          | x::s -> g.(i).(j) <- x; f g s i j
        end
      else if (i = 8 && j = 8) then () else if (j = 8) then f g sol (i+1) 0 else f g sol i (j+1)
    in f g sol 0 0;;


  (*| Change les valeurs d'une liste de grilles de sudoku données pour y insérer les valeurs manquantes
    | Paramètres : l [in/out], Une liste de grilles
    | Renvoie : rien*)
  let minisat_resolution_grilles l =
    minisat_fichiers_solutions l;
    let rec f l i =
      match l with
      | [] -> ()
      | x::s -> let sol = (liste_solutions ("dimacs/grille"^(string_of_int i)^"solution")) in resolution_grille x sol; (f s (i+1))
    in f l 0;;


  (*| Crée le nouveau fichier solution avec les grilles de sudoku résolues
    | Paramètres : l [in], Une liste de grilles
                   nom_fichier [in], Le nom d'un fichier
    | Renvoie : rien*)
  let fichiers_finaux l nom_fichier =
    let file = open_out nom_fichier in
    let rec f l i =
      match l with
      | [] -> close_out file
      | x::s -> for i = 0 to 8 do
                  for j = 0 to 8 do
                    output_string file (string_of_int x.(i).(j))
                  done
                done;
                output_string file "\n";
                f s (i+1)
    in f l 0;;

  
  (*| Résoud les grilles de sudoku contenues dans un fichier et les place dans un nouveau fichier
    | Paramètres : src [in], Le nom du fichier contenant les grilles à résoudre
                   dest [in], Le nom du fichier dans lequel seront placées les solutions
    | Renvoie : rien*)
  let minisat_resolution_file src dest =
    let l = grilles_de_sudoku src in
      (minisat_resolution_grilles l ;
      fichiers_finaux l dest);;


(*********************************************
                                              
     Fonctions pour le nouveau SAT-SOLVER     
                                              
*********************************************)


  (*| Crée une chaine de caractères correspondant à une interprétation des variables booléennes d'une formule
    | Paramètres : interp [in], Un tableau d'entiers correspondant à une interprétation
    | Renvoie : Une chaine de caractères*)
  let interp_string interp =
    let result = ref "" in
    for i=1 to ((Array.length interp) -1) do
      match interp.(i) with
      | 1 -> result := (!result)^(string_of_int i)^" "
      | 0 | (-1) -> result := (!result)^(string_of_int (i*(-1)))^" "
      | _ -> failwith "Valeur d'interprétation incorrecte"
    done;
    (!result);;


  (*| Enlève les clauses contenant une valeur vraie dans l'interprétation donnée d'une FNC donnée
    | Paramètres : interp [in], Un tableau d'entiers correspondant à une interprétation
                   fnc [in], La FNC actuelle
    | Renvoie : Une paire contenant la nouvelle FNC avec les clauses inutiles en moins et un booléen qui vaut true si la nouvelle FNC est différente et false sinon*)
  let fnc_bcp_v interp fnc =
    let new_fnc = List.filter (fun l -> (not (List.exists (fun x -> ((interp.((abs x))*x) > 0)) l))) fnc in (new_fnc, (not ((List.length fnc) = (List.length new_fnc))));;


  (*| Enlève les atomes dont la valeur est fausse dans l'interprétation donnée d'une FNC donnée
    | Paramètres : interp [in], Un tableau d'entiers correspondant à une interprétation
                   fnc [in], La FNC actuelle
    | Renvoie : Une paire contenant la nouvelle FNC avec les atomes inutiles en moins et un booléen qui vaut true si la nouvelle FNC est différente et false sinon*)
  let fnc_bcp_f interp fnc =
    let new_fnc = List.rev_map (fun l -> (List.filter (fun x -> ((interp.((abs x))*x) >= 0)) l)) fnc in (new_fnc, (not ((List.fold_left (fun acc x -> acc+(List.length x)) 0 fnc) = (List.fold_left (fun acc x -> acc+(List.length x)) 0 new_fnc))));;


  (*| Si la FNC contient une clause ne contenant qu'un seul littéral, alors l'interprétation est mise à jour pour prendre une nouvelle valeur qui garantit que cette clause est vraie et la clause est enlevée de la FNC
    | Paramètres : interp [in/out], Un tableau d'entiers correspondant à une interprétation
                   fnc [in], La FNC actuelle
    | Renvoie : Une paire contenant la nouvelle FNC avec la clause choisie en moins et un booléen qui vaut true si l'interprétation a changée et false sinon*)
  let fnc_assume interp fnc =
    let temp = ref false in
    let new_fnc = List.filter (fun l -> match l with [x; 0] | [0; x] -> if (not (!temp)) then (if (x > 0) then (interp.((abs x)) <- 1) else (interp.((abs x)) <- -1); temp := true; false) else true | _ -> true) fnc in (new_fnc, !temp);;


  (*| Renvoie deux interprétations qui sont identiques à celle donnée en paramètre à l'exception d'une variable indéfinie qui vaut maintenant faux (ici -1) dans l'une et vraie (ici 1) dans l'autre
    | Paramètres : interp [in], Un tableau d'entiers correspondant à une interprétation
    | Renvoie : Une paire contenant les deux nouvelles interprétations*)
  let fnc_unsat interp =
    let (interp1, interp2) = (Array.copy interp, Array.copy interp) in
    let changed = ref false in
    for i = 1 to ((Array.length interp) - 1) do
      if (not (!changed)) then
      (if interp.(i)=0 then (interp1.(i) <- 1; interp2.(i) <- -1; changed := true) else ())
      else ();
    done;
    (interp1, interp2);;


  (*| Renvoie la chaine de caractères correspondant à un modèle de le FNC en paramètre
    | Paramètres : nb_var [in], Le nombre da variables dans la FNC
                   fnc [in], La FNC dont on cherche un modèle
    | Renvoie : La chaine de caractères coreespondant au modèle de la FNC*)
  let fnc_sol nb_var fnc =
    let rec f interp fnc cont =
      if (List.for_all (fun x -> match x with [] -> true | x::s -> false) fnc)
      then (cont (interp_string interp))
      else
        if (List.exists (fun x -> match x with [0] -> true | _ -> false) fnc)
        then (cont "")
        else let (new_fnc, has_changed) = fnc_bcp_v interp fnc in
          if (has_changed) then (f interp new_fnc cont)
          else let (new_fnc2, has_changed2) = fnc_bcp_f interp new_fnc in
            if (has_changed2)
            then (f interp new_fnc2 cont)
            else let (new_fnc3, has_changed3) = (fnc_assume interp new_fnc2) in
              if (has_changed3)
              then (f interp new_fnc3 cont)
              else let (interp1, interp2) = fnc_unsat interp in
                f interp1 new_fnc3 (fun r -> if ((String.length r) = 0) then (f interp2 new_fnc3 (fun s -> (cont s))) else (cont r))
    in f (Array.make (nb_var+1) 0) fnc (fun x -> x);;


  (*| Renvoie la fnc contenue dans un fichier dimacs ainsi que le nombre de variables dans la formule
    | Paramètres : nom_fichier [in], Le nom d'un fichier dimacs
    | Renvoie : La fnc contenue dans ce fichier ainsi que le nombre de variables dans la formule*)
  let lire_fnc nom_fichier =
    let file = open_in nom_fichier in
    let rec f s comment problem clause fnc nbvar =
      try
        match s with
        | [] -> f (Str.split (Str.regexp "[ \t]+") (input_line file)) comment problem clause fnc nbvar
        | "0"::h -> if comment then f h comment problem clause fnc nbvar else f h comment problem [] ((0::clause)::fnc) nbvar
        | "c"::h -> if comment then f h comment problem clause fnc nbvar else f h true problem clause fnc nbvar
        | "p"::h -> f h false true clause fnc nbvar
        | "cnf"::h -> if (not problem) then failwith "Format incorrect pour fichier Dimacs" else f h comment problem clause fnc nbvar
        | x::h -> if problem then if (nbvar=(-1)) then f h comment problem clause fnc (int_of_string x) else f h comment false clause fnc nbvar else f h comment problem ((int_of_string x)::clause) fnc nbvar
      with End_of_file -> ((clause::fnc), nbvar)
    in f (Str.split (Str.regexp "[ \t]+") (input_line file)) false false [] [] (-1);;  


  (*| Crée un fichier contenant la solution d'une grille de sudoku dont le fichier dimacs correspondant est donné en paramètre
    | Paramètres : s1 [in], Le nom du fichier dimacs correspondant à la grille que l'on veut résoudre, sous la forme d'une chaine de caractères
                   s2 [in], Le nom du fichier dans lequel la solution sera écrite, sous la forme d'une chaine de caractères
    | Renvoie : rien*)
  let sol_sudoku src dest =
    let (fnc, nb_var) = lire_fnc src in
    let solution = fnc_sol nb_var fnc in
    let file = open_out dest in
    output_string file ("SAT\n"^solution);
    close_out file;;


  (*| Resoud une liste de grilles de sudoku et écrit les solutions dans des fichiers textes
    | Paramètres : l [in], Une liste de grilles
    | Renvoie : rien*)
  let sat_fichiers_solutions l =
    let l_fnc = (liste_fnc_de_liste_grilles l) in tous_les_fichiers_dimacs l_fnc;
    for i=0 to ((List.length l_fnc)-1) do
      sol_sudoku ("dimacs/grille"^(string_of_int i)^".dimacs") ("dimacs/grille"^(string_of_int i)^"solution");
    done;;


  (*| Change les valeurs d'une liste de grilles de sudoku données pour y insérer les valeurs manquantes
    | Paramètres : l [in/out], Une liste de grilles
    | Renvoie : rien*)
  let sat_resolution_grilles l =
    sat_fichiers_solutions l;
    let rec f l i =
      match l with
      | [] -> ()
      | x::s -> let sol = (liste_solutions ("dimacs/grille"^(string_of_int i)^"solution")) in resolution_grille x sol; (f s (i+1))
    in f l 0;;


  (*| Résoud les grilles de sudoku contenues dans un fichier et les place dans un nouveau fichier
    | Paramètres : src [in], Le nom du fichier contenant les grilles à résoudre
                   dest [in], Le nom du fichier dans lequel seront placées les solutions
    | Renvoie : rien*)
  let sat_resolution_file src dest =
    let l = grilles_de_sudoku src in
      (sat_resolution_grilles l;
      fichiers_finaux l dest);;
