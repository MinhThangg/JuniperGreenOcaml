
type cell_state = Unchecked | Checked;;
type cell = Cell of int * cell_state;;
type player = Player of int;;

let ligne = 10;;
let colonne = 10;;
let size = ligne*colonne;;

let nb_of_char n = String.length (string_of_int n);;
let char_per_cell = nb_of_char size;;

let creer_plateau ligne colonne = 
  Array.init (ligne*colonne) (fun i -> Cell(i+1, Unchecked));;



let affiche_plateau plt = 
  for i=0 to ligne-1 do
    print_string "|";
    for j=0 to colonne-1 do
      plt.(3)<-Cell(5, Checked);
      
      (match (plt.(j+i*colonne)) with 
        | Cell(idx, Unchecked) -> (
          for k=1 to char_per_cell-(nb_of_char idx) do
            print_string " ";
          done;
          print_int idx;)
        | Cell(_, Checked) -> (
          for k=1 to char_per_cell do
            print_string "▒";
          done;));
      
      print_string "|"
    done;
    print_endline ""
  done;;

let last = ref 0;;

let jouer cell_idx = ;;

affiche_plateau (creer_plateau ligne colonne);;


