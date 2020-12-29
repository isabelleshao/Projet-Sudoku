CC=ocamlc
CMO_DEP=ig.cmo drawing.cmo formatting.cmo sudoku.cmo sat.cmo dataio.cmo 
CMI_DEP=ig.cmi drawing.cmi formatting.cmi sudoku.cmi sat.cmi dataio.cmi 

%.cmi : %.mli
	$(CC) -c $<

%.cmo : %.ml %.cmi
	$(CC) -c $<

projet : projet.ml $(CMO_DEP) $(CMI_DEP)
	$(CC) -o $@ graphics.cma  unix.cma  str.cma  $(CMO_DEP) $<

clean :
	rm -f *.cmi *.cmo projet

