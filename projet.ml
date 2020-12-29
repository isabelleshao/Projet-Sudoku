open Graphics;;
open Printf;; 
open Str;;
open Drawing;; 
open Formatting;;
open Sudoku;; 
open Dataio;;
open Sat;;
 
let tailleSudoku = 500+5 ;;
let () = Random.self_init ();; 


let get_cpt grille =
  Array.iteri ( fun i l ->
                Array.iteri ( fun j _ ->  if grille.modifiedGrid.(8-i).(j)<>0 then  grille.cpt <- grille.cpt-1 ) l
    )  grille.modifiedGrid ;
;;

let backtracking x y data grille =
    (* ajout dans la liste*) 
    grille.backtrack <- (x, y, data)::grille.backtrack;
;;

let rec update_grid x y grille =

  let update_cpt prev next grille =
    (*remplissage à la main par le joueur *)
    if prev <> next && prev=0 then (
      grille.cpt <- grille.cpt-1;
    )
  in
  
let is_updatable x y grille =
  let reponse = ref false in

      if x/grille.scalePx <9 && y/grille.scalePx<9 && grille.grid.(8-y/grille.scalePx).(x/grille.scalePx)==0 then (
        set_color magenta ;
        fill_rect (grille.scalePx*(x/grille.scalePx)) (grille.scalePx*( y/grille.scalePx))  grille.scalePx grille.scalePx ;
        reponse := true;     
    );
  
  set_color black;
  draw_case grille;
  !reponse;
in


  if is_updatable x y grille then (
    
    let e = wait_next_event [Key_pressed; Button_down] in
    if e.keypressed && is_integer e.key then (
      update_cpt (grille.modifiedGrid.(8-y/grille.scalePx).(x/grille.scalePx)) (int_of_string (Char.escaped e.key)) grille;
      backtracking (8-y/grille.scalePx) (x/grille.scalePx)  grille.modifiedGrid.(8-y/grille.scalePx).(x/grille.scalePx)  grille;
      grille.modifiedGrid.(8-y/grille.scalePx).(x/grille.scalePx) <-int_of_string (Char.escaped e.key);
      if grille.saved then grille.saved<-false;    
    );
    
    if e.button then (
      set_color  (rgb 220 220 220 ) ;
      fill_rect (grille.scalePx*(x/grille.scalePx)) (grille.scalePx*( y/grille.scalePx))  grille.scalePx grille.scalePx ;
      set_color red;
      let margeLigne = grille.scalePx/2 in
      let margeColonne = grille.scalePx/2 in
      moveto (margeColonne+( x/grille.scalePx)* grille.scalePx) (margeLigne+( y/grille.scalePx)* grille.scalePx);
      draw_string (string_of_int grille.modifiedGrid.(8-y/grille.scalePx).(x/grille.scalePx));
      set_color black;
      update_grid e.mouse_x e.mouse_y grille;
    );
    
  )else if x >550 && x <700 then  ( 
    
    (**reinit**)
    if y >400 && y<455 then
      (
        grille.colorGrid <- get_color grille.grid;
        Array.iteri ( fun i l ->
                      Array.iteri ( fun j _ -> grille.modifiedGrid.(i).(j) <- grille.grid.(i).(j)) l
          ) grille.modifiedGrid ;
        grille.cpt<-81;
        get_cpt grille ;
        grille.backtrack <-[];
        if grille.saved then grille.saved<-false;
      )
    else if y >330 && y<385 &&( grille.level.lvl = 1  || grille.level.lvl = 2 && grille.level.joue <5 ) then
      (
        (**indice**)
            let i = ref (Random.int 9) in
            let j = ref (Random.int 9) in
            if grille.cpt<>0 then
              (
                while grille.modifiedGrid.(!i).(!j) == grille.reponseGrid.(!i).(!j) do
                  i :=  Random.int 9 ;
                  j :=  Random.int 9 ;
                  
                done;
                backtracking !i !j (grille.modifiedGrid.(!i).(!j)) grille;
                update_cpt (grille.modifiedGrid.(!i).(!j)) (grille.reponseGrid.(!i).(!j)) grille;
                grille.colorGrid.(!i).(!j) <- cyan;
                grille.modifiedGrid.(!i).(!j) <- grille.reponseGrid.(!i).(!j);
                grille.level.joue <- grille.level.joue+1; 
                if grille.saved then grille.saved<-false;    
              
          )
      )
    else if y >260 && y<315  && (grille.level.lvl = 1  || grille.level.lvl = 2 && grille.level.joue <5) then
      (  (**precedent**)
        match grille.backtrack with
        |[]-> ();
        |(x, y, data)::tl ->
          (
            if grille.modifiedGrid.(x).(y) <> 0 && data = 0 then
              grille.cpt <- grille.cpt +1
            else if  grille.modifiedGrid.(x).(y) = 0 && data <> 0 then
              grille.cpt <- grille.cpt -1;
            grille.colorGrid.(x).(y) <- yellow;
            grille.modifiedGrid.(x).(y) <- data;
            grille.backtrack <- tl ;
            if grille.saved then grille.saved<-false;
            grille.level.joue <- grille.level.joue+1;
          )        
      )
    else if  y >190  && y<245  then
      (  (**regle**)
        screen_regle grille 2 ; 
      )
    else if  y >120 && y<175 then
      (  (**sauvegarder**)
        string_2_txt grille;
        grille.saved<-true;
      ) 
  )
and  screen_regle grille mode =
  if mode <>1 then
    (
      draw_empty_screen();
      draw_image (Ig.init_image "regle.ppm") 50 180;
      Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";

      create_button 548 103 154 54 black white "Retour" 585 120;

      let e = wait_next_event [Button_down] in
      if e.button then
        if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            draw_grid grille;
            draw_buttons grille;
          (* loop grille;*)
          )
        else
          screen_regle grille 1
    ) else if mode =1 then
    (
      let e = wait_next_event [Button_down] in
      if e.button then
        if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            draw_grid grille;
            draw_buttons grille;
          (*  loop grille;*)
          )
        else
          screen_regle grille 1
    )
 
and loop grille =
  if grille.cpt=0 then
    create_button 550 (400-5*70) 150 55 (rgb 220 220 220) black "VALIDER" 575 (420-5*70); 
  
  let e = wait_next_event [Button_down] in
  
  if e.button && e.mouse_x >550 &&  e.mouse_x <700 &&  e.mouse_y >50 &&  e.mouse_y <105 && grille.cpt=0 then
    (   (**soumettre grille**)
      let compteur = ref 0 in
      Array.iteri ( fun a l ->
                    Array.iteri ( fun b _ -> if grille.reponseGrid.(a).(b) != grille.modifiedGrid.(a).(b) then  compteur:= !compteur+1;  ) l
        ) grille.reponseGrid ;
      
      draw_empty_screen();
      Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";

      if !compteur==0 then (
       
        draw_image (Ig.init_image "win.ppm") 50 180;
        moveto 300 100;
        let total  = if grille.level.lvl = 1 || grille.level.lvl = 2 then 100+(100-grille.level.joue) else  200 in
        let score =  "Score : "^string_of_int(total ) in
        draw_string score;
        let records = get_score() in
        if List.length records < 10 || total > List.nth records 9 then
          ( moveto 270 80;
            draw_string "NOUVEAU RECORD!!";
           
            score_2_txt total ;
          
          (* get_keypress();*)

          )
      ) else(
        draw_image (Ig.init_image "lose.ppm") 160 180;
        moveto 300 100;
        draw_string "Score : 0" ;
      ) 
    )
  else if e.button then
    (
      update_grid e.mouse_x e.mouse_y grille ;
      clear_graph ();
      draw_grid grille;
      draw_buttons grille;
      loop grille
    )
;;

let rec screen_menu ()  =
  draw_empty_screen();
  Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
   
  let l = ["Nouvelle Partie"; "Solver"; "Charger Partie"; "Regles du jeu"; "Records du jeu"; "Quitter le jeu" ] in
  
  for i = 0 to (List.length l)-1 do
      create_button 250 (550-i*100) 300 80 black white (List.nth l i) 310 (580-i*100)
  done;

  let action i =
    if i=0 then ( clear_graph (); screen_partie 2 )   (*jouer*)
    else if i=5 then 
        screen_partie 3  
          (*Sat.sat_resolution_file "puzzles_facile.sdk" "puzzles_facile_solution.sdk";
         Sat.minisat_resolution_file "puzzles.sdk" "puzzles_solution.sdk"
         Sat.affiche_grille ( List.nth (Sat.grilles_de_sudoku "puzzles.sdk") 0 );  *)
     (*** MINISAT *****)
    else if i=1 then screen_partie 1     (*** CHARGER PARTIE *****)
    else if i=2 then screen_regle 2       (*** SCREEN REGLE*****)
    else if i=3 then screen_score (get_score())     (***HIGH SCORE***)
    else if i=4 then close_graph()     (*** FERMER FENETRE*****)
  in
  
  let e = wait_next_event [Button_down] in
  if e.button then
    (
      if e.mouse_x>250 && e.mouse_x <550 && e.mouse_y>550 && e.mouse_y<630 then action 0
      else if e.mouse_x>250 && e.mouse_x <550 && e.mouse_y>450 && e.mouse_y<530 then action 5
      else if e.mouse_x>250 && e.mouse_x <550 &&  e.mouse_y>350 && e.mouse_y<430 then action 1
      else if e.mouse_x>250 && e.mouse_x <550 && e.mouse_y>250 && e.mouse_y<330 then action 2
      else if e.mouse_x>250 && e.mouse_x <550 &&  e.mouse_y>150 && e.mouse_y<230 then action 3
      else if e.mouse_x>250 && e.mouse_x <550 && e.mouse_y>50 && e.mouse_y<130 then action 4
     else screen_menu ()
    ) 
and  screen_regle mode =
  if mode <>1 then
    (
      draw_empty_screen();
      draw_image (Ig.init_image "regle.ppm") 50 180;
      
      Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
      create_button 548 103 154 54 black white "Retour" 585 120;

      let e = wait_next_event [Button_down] in
      if e.button then
        if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            if mode = 2 then screen_menu ();
            if mode = 3 then screen_menu ()
          )
        else
          screen_regle 1
    )
  else if mode =1 then
    (
      let e = wait_next_event [Button_down] in
      if e.button then
        if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then
          screen_menu ()
        else
          screen_regle 1
    )
and screen_score  sc =
  draw_empty_screen();
  Graphics.set_font "-*-fixed-medium-r-*--20-*-*-*-*-*-iso8859-1";
  for i = 0 to ((List.length sc)-1) do
    moveto 250 (500-(i*20)) ;
    let item = string_of_int(i+1)^(". ") ^ (string_of_int(List.nth sc i)) in
    draw_string item;
  done;
  Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
  moveto 360 600 ;
  draw_string  "HIGH SCORE";
  create_button 548 103 154 54 black white "Retour" 585 120;
  
  let e = wait_next_event [Button_down] in
  if e.button then
    (
      if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then screen_menu () else screen_score sc;
    )
   
and screen_partie mode =
  draw_empty_screen();
  
  if mode=1 then (
    (****************** CHOIX PARTIE SAUVEGARDEE *******************)

    (**chargement partie sauvegardée**)
    if (Sys.file_exists "./sauvegardes" &&  Array.length (Sys.readdir "./sauvegardes")>0 ) then (
      display_liste_sauvegarde () ; 
    )else(
      moveto 250 500 ;
      Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
      draw_string "Aucune partie sauvegardée";
    );
    
    Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
    create_button 548 103 154 54 black white "Retour" 585 120;
    
    let e = wait_next_event [Button_down] in
    
    if e.button then (
      if e.mouse_x>548 && e.mouse_x <700  &&  e.mouse_y>103 && e.mouse_y<160 then
        (
          (*clic sur retour*) 
          clear_graph ();
          screen_menu ();
        )
      
      else if (Sys.file_exists "./sauvegardes" &&  Array.length (Sys.readdir "./sauvegardes")>0 ) &&
                e.mouse_x>140 && e.mouse_x <690 &&  e.mouse_y>180 && e.mouse_y<600 then
        (
          let x = (e.mouse_x-140)/200 in
          let y = (((e.mouse_y-180)/30-13 )*(-1)) in
          let nom = ref "" in
          (try
               nom := (String.sub (Sys.readdir "./sauvegardes").(x+y*3)   10 ((String.length  ((Sys.readdir "./sauvegardes").(x+y*3)))-10)) 
           with Invalid_argument x->
             screen_partie mode ) ;

          let sudoku =  setSavedSudoku !nom tailleSudoku in 

          clear_graph ();
          get_cpt sudoku ;
          draw_grid sudoku;
          draw_buttons sudoku;
          loop sudoku;
            
        ) else  screen_partie mode 
    );
  )else if mode=2 then(
    (*****************************chargement nouvelle partie****************************************)
    let rec screen_liste lvl =
      draw_empty_screen () ;
      Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
      create_button 548 103 154 54 black white "Retour" 585 120;
      Graphics.set_font "-*-fixed-medium-r-*--16-*-*-*-*-*-iso8859-1";
      set_color black;
      display_liste_grille (); 
      let e = wait_next_event [Button_down] in
      
      if e.button then (
        if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            clear_graph ();
            screen_menu ();
          )
        else if e.mouse_x>140 && e.mouse_x <690 &&  e.mouse_y>180 && e.mouse_y<600 then
          (
            let x = (e.mouse_x-140)/110 in
            let y = (((e.mouse_y-180)/30-13 )*(-1)) in
            let nom = (String.sub  ((Sys.readdir "./grids").(x+y*5)) 4 ((String.length  ((Sys.readdir "./grids").(x+y*5)))-4)) in
            let sudoku =  setNewSudoku nom tailleSudoku lvl "classique" in 
            
            clear_graph ();
            get_cpt sudoku ;
            draw_grid sudoku;
            draw_buttons sudoku;
            loop sudoku;
            
          )
        else
          screen_liste lvl;
      );
    in
    
    
    let rec screen_niveau p =

      let level = ["Facile";"Moyen"; "Difficile"] in
      let message m =
        if  m = 1 then  (
          moveto 160 330 ;
          draw_string ("Niveau Facile : Le nombre de retour arriere");
          moveto 160 310 ;
          draw_string ("et d'aide est illimite! Ideal pour s'echauffer!");
        )  else if m=2 then (
          moveto 160 330 ;
          draw_string ("Niveau Moyen : Le nombre de retour arriere");
          moveto 160 310 ;
          draw_string ("et d'aide est limité à 5!");
        ) else if m = 3 then(
          moveto 160 330 ;
          draw_string ("Niveau Difficile : Le retour arriere et l'aide");
          moveto 160 310 ;
          draw_string ("est impossible! Pret à pleurer?");
        );
        Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
        create_button 300 103 154 54 black white "Valider" 335 120
      in 
      
      
      draw_empty_screen () ; 
      Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
      
      for i = 0 to (List.length level)-1 do
        create_button (130+i*200) 500 180 80 black white (List.nth level i) (170+i*200) 530
      done;
      
      create_button 548 103 154 54 black white "Retour" 585 120;
      Graphics.set_font "-*-fixed-medium-r-*--17-*-*-*-*-*-iso8859-1";
      
      set_color black;
      
      if p = 1 then  message 1 else if p=2 then message 2 else if p=3 then message 3;
      
      let e = wait_next_event [Button_down ] in
      if e.button then (
        if e.mouse_x>130 && e.mouse_x <310 &&  e.mouse_y>500 && e.mouse_y<580 then
          screen_niveau 1
        else if e.mouse_x>330 && e.mouse_x <510 &&  e.mouse_y>500 && e.mouse_y<580 then
          screen_niveau 2
        else if e.mouse_x>530 && e.mouse_x <710 &&  e.mouse_y>500 && e.mouse_y<580 then
          screen_niveau 3
        else if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            clear_graph ();
            screen_menu ();
          )
        else if e.mouse_x>300 && e.mouse_x <454  &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            screen_liste p;
          )else screen_niveau p;
      );
    in
    screen_niveau 0;
  )else(
    (*****************************chargement partie minisat****************************************)
    let rec screen_liste lvl =
      draw_empty_screen () ;
      Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";

      create_button 548 103 154 54 black white "Retour" 585 120;
      Graphics.set_font "-*-fixed-medium-r-*--16-*-*-*-*-*-iso8859-1";
      set_color black;
          
      display_liste_minisat (); 
      let e = wait_next_event [Button_down] in
      
      if e.button then (
        if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            clear_graph ();
            screen_menu ();
          )
        else if e.mouse_x>140 && e.mouse_x <690 &&  e.mouse_y>180 && e.mouse_y<600 then
          (
            let x = (e.mouse_x-140)/110 in
            let y = (((e.mouse_y-180)/30-13 )*(-1)) in
            let nom =  (string_of_int(x+y*5)) in
            let sudoku =  setNewSudoku nom tailleSudoku (if lvl=6 then 1 else if lvl=7 then 3 else lvl) (if lvl=6 || lvl=7 then "sat" else "minisat") in 
   
            clear_graph ();
            get_cpt sudoku ;
            draw_grid sudoku;
            draw_buttons sudoku;
            loop sudoku;
          ) 
        else
          screen_liste lvl;
      );
    in
    
    
    let rec screen_niveau p =
      let level = ["Test";"Facile"; "Difficile"] in
      let message m =
        if m = 0 || m = 4 then (
          moveto 160 330 ;
          draw_string ("Bienvenue dans la partie sat solver du jeu. ");
          moveto 160 310 ;
          draw_string ("Le principe: ici nous ne possedons que des ");
          moveto 160 290 ;
          draw_string ("grilles non resolues de sudoku. La solution");
          moveto 160 270 ;
          draw_string ("sera generee par un solver. Ca vous tente?");
          if m = 4 then (
            Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
            moveto 200 230 ;
            draw_string ("Les solutions sont generees.");
          )
        ) else  if  m = 1 || m=5 then  (
          moveto 160 330 ;
          draw_string ("Niveau Test : Tester le solver au global");
          moveto 160 310 ;
          draw_string ("en generant la solution de TOUTES les grilles");
          moveto 160 290 ;
          draw_string ("dans un fichier 'puzzle_solution.sdk' et voir");
          moveto 160 270 ;
          draw_string ("les etapes en console ");         
          
        )  else if m=2 || m=6 then (
          moveto 160 330 ;
          draw_string ("Niveau Facile : Le nombre de retour arriere");
          moveto 160 310 ;
          draw_string ("et d'aide est illimité, et possibilité ");
          moveto 160 290 ;
          draw_string ("de voir la solution!");
        ) else if m = 3 ||m= 7 then(
          moveto 160 330 ;
          draw_string ("Niveau Difficile : Le retour arriere et l'aide");
          moveto 160 310 ;
          draw_string ("est impossible! Pret à pleurer?");
        );
        if m<>4 then (
          Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
          create_button 300 103 154 54 black white "Valider" 335 120
        )
      in 
      
      
      draw_empty_screen () ; 
      Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
      set_color black;
      moveto 130 600 ;
      draw_string ("MINI SAT");
      moveto 130 450 ;
      draw_string ("SAT");
      for i = 0 to (List.length level)-1 do
        create_button (130+i*200) 500 180 80 black white (List.nth level i) (170+i*200) 530
      done;
      for i = 0 to (List.length level)-1 do
        create_button (130+i*200) 360 180 80 black white (List.nth level i) (170+i*200) 390
      done;
      create_button 548 103 154 54 black white "Retour" 585 120;
      Graphics.set_font "-*-fixed-medium-r-*--17-*-*-*-*-*-iso8859-1";
      
      set_color black;
      
      if p = 0 then message 0 else if p = 1 || p=5 then  message 1 else if p=2 || p=6 then message 2 else if p=3 || p=7 then message 3 else message 4;
      
      let e = wait_next_event [Button_down] in
      if e.button then (
        if e.mouse_x>130 && e.mouse_x <310 &&  e.mouse_y>500 && e.mouse_y<580 then
          screen_niveau 1 (*niveau test minisat*) 
        else if e.mouse_x>330 && e.mouse_x <510 &&  e.mouse_y>500 && e.mouse_y<580 then
          screen_niveau 2  (*niveau facile minisat*) 
        else if e.mouse_x>530 && e.mouse_x <710 &&  e.mouse_y>500 && e.mouse_y<580 then
          screen_niveau 3  (*niveau difficile minisat*)
        else if e.mouse_x>130 && e.mouse_x <310 &&  e.mouse_y>360 && e.mouse_y<440 then
          screen_niveau 5 (*niveau test sat*) 
        else if e.mouse_x>330 && e.mouse_x <510 &&  e.mouse_y>360 && e.mouse_y<440 then
          screen_niveau 6  (*niveau facile sat*) 
        else if e.mouse_x>530 && e.mouse_x <710 &&  e.mouse_y>360 && e.mouse_y<440 then
          screen_niveau 7  (*niveau difficile sat*) 
        else if e.mouse_x>548 && e.mouse_x <700 &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            clear_graph ();
            screen_menu ();
          )
        else if  p<>4 &&  e.mouse_x>300 && e.mouse_x <454  &&  e.mouse_y>103 && e.mouse_y<160 then
          (
            if p = 1 then (
              Sat.minisat_resolution_file "puzzles.sdk" "puzzles_solution.sdk";
              screen_niveau 4;
            ) else if p = 5 then (
              Printf.printf("test");
              Sat.sat_resolution_file "puzzles_facile.sdk" "puzzles_facile_solution.sdk";
              screen_niveau 4;
	    )else (            
              screen_liste (if p = 2 then 1 else p);
            )
          )else screen_niveau p;
      );
    in
    screen_niveau 0;
  );
;;

let () =
  open_graph " 800x800";
  set_window_title "Sudoku";
  screen_menu ();
  ignore(read_key ())
;;
 
 
