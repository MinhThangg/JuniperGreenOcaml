
type cell_state = Unchecked | Checked;;
type cell = Cell of int * cell_state;;
type player = Player of int;;
exception CoupImpossible;;

let char_per_cell = nb_of_char size;;

let creer_plateau ligne colonne = 
  Array.init (ligne*colonne) (fun i -> Cell(i+1, Unchecked));;

let affiche_plateau plt = 
  for i=0 to ligne-1 do
    print_string "|";
    for j=0 to colonne-1 do
      
      
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

let mult_or_divi a b =
  if b <> 0 && a <> 0 then
    a mod b = 0 || b mod a = 0;
  else
    false;

let ligne = 5;;
let colonne = 5;;
let size = ligne*colonne;;

let plateau = creer_plateau ligne colonne
let last = ref 0;;
let nb_of_char n = String.length (string_of_int n);;

let player_list = [|Player(1), Player(2)|];;
let last_player_idx = 0;;

let jouer plt idx = 
  if !last = 0 || (mult_or_divi idx !last) then (
    plt.(idx-1)<-Cell(idx+1, Checked);
    last := idx;
  else
    print_string "Coup impossible";
  
  (affiche_plateau plt);;

jouer plateau 4;;



