open Graphics;;
open Sudoku;;

let draw_empty_screen () =
  clear_graph ();
  set_color (rgb 230 230 230) ;
  fill_rect 0 0 800 800 ;
  Graphics.set_font "-*-fixed-medium-r-*--16-*-*-*-*-*-iso8859-1";
  draw_image (Ig.init_image "sudoku.ppm") 100 614;
  set_color black ;  
;;

let create_button x y w h bg c s mx my =
  set_color c;
  fill_rect (x-2) (y-2) (w+4) (h+4);
  set_color bg; 
  fill_rect x y w h;
  set_color c;
  moveto mx my ;
  draw_string s
;;



let draw_case grille =
  set_color black ;
  for j = 0 to 3 do
    let a = j*grille.columnPx in
    for i = 0 to 9 do
      if i mod 3 = 0 then set_line_width 3 else set_line_width 1;
      let z = i*grille.scalePx in
      moveto a z ;
      lineto (grille.linePx) z
    done ;
  done;
  for j = 0 to 3 do
    let b = j*grille.linePx in
    for i = 0 to 9 do
      if i mod 3 = 0 then   set_line_width 3 else set_line_width 1;
      let x = i*grille.scalePx in
      moveto x b ;
      lineto x (grille.linePx)
    done;
  done;
;;
let draw_grid grille =
  draw_empty_screen();
  let margeLigne = grille.scalePx/2 in
  let margeColonne = grille.scalePx/2 in
  let taille_x =  grille.scalePx in
  let taille_y =  grille.scalePx in

  for i = 0 to 8 do
    for j = 0 to 8 do
      moveto (margeColonne+j*taille_x) (margeLigne+i*taille_y);
      set_color grille.colorGrid.(8-i).(j) ;
      fill_rect (taille_x*j) (taille_x*i)  taille_x taille_y ;
      if grille.grid.(8-i).(j)==0 then
        begin
          set_color red;
          (*   Graphics.set_font "-*-fixed-medium-r-semicondensed--16-*-*-*-*-*-iso8859-1";*)
          draw_string (string_of_int grille.modifiedGrid.(8-i).(j));
        end
      else
        begin
          set_color black ;
          draw_string (string_of_int grille.modifiedGrid.(8-i).(j));
        end
    done;
  done;

  draw_case grille;
;;


(**BOUTONS ANNEXE*)


let draw_buttons grille =
  let l = ["REINITIALISER"; "INDICE"; "PRECEDENT"; "REGLES"; "SAUVEGARDER"; "SAUVEGARDEE!" ] in
  
  for i = 0 to (List.length l)-2 do
    if i = (List.length l)-2 then
      if grille.saved then 
        create_button 550 (400-i*70) 150 55 (rgb 220 220 220) red (List.nth l (i+1)) 575 (420-i*70)
      else
        create_button 550 (400-i*70) 150 55 (rgb 220 220 220) black (List.nth l i) 575 (420-i*70)
    else
      if (grille.level.lvl = 3 || grille.level.lvl = 2 && grille.level.joue >= 5 )&& (i=1 || i=2) then
        create_button 550 (400-i*70) 150 55 (rgb 220 220 220) (rgb 180 180 180) (List.nth l i) 575 (420-i*70)
      else
        create_button 550 (400-i*70) 150 55 (rgb 220 220 220) black (List.nth l i) 575 (420-i*70)
  done
;;
