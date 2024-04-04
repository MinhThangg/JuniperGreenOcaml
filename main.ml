type cell_state = Unchecked | Checked;;
type cell = Cell of int * cell_state;;
type player = Player of int
| NamedPlayer of string
| Robot of string;;
(*type plateau = Plateau of cell array * int * int;;*)
type plateau = {
  plt: cell array;
  ligne: int;
  colonne: int;
  player_queue: player Queue.t;
  current_player: player ref;
  last: int;
}
exception CoupImpossible;;




let creer_plateau ligne colonne = {
  plt = (Array.init (ligne*colonne) (fun i -> Cell(i+1, Unchecked))); 
  ligne = ligne; 
  colonne = colonne;
  player_queue = Queue.create ();
  current_player = ref (Robot("Robert"));
  last = 0;
};;


let affiche_plateau plateau =

  let nb_of_char n = String.length (string_of_int n) in
  let char_per_cell = nb_of_char (plateau.ligne*plateau.colonne) in
  for i=0 to plateau.ligne-1 do
    print_string "|";
    for j=0 to plateau.colonne-1 do
      
      
      (match (plateau.plt.(j+i*plateau.colonne)) with 
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
    a mod b = 0 || b mod a = 0
  else
    false;;

let check_victory plateau coup = 
  let v = ref true in

  for i=0 to ((plateau.ligne*plateau.colonne)-1) do
    match plateau.plt.(i) with 
    | Cell(idx, Unchecked) -> 
      if mult_or_divi coup idx then
        v := false;
    | _ -> ();
  done;
  !v;;

let is_play_valid plateau coup last = 
  if coup < 1 || coup > (plateau.ligne*plateau.colonne) then begin
    false
  end else
  match plateau.plt.(coup-1) with
    | Cell(idx, Unchecked) -> last = 0 || mult_or_divi idx last
    | Cell(_, Checked) -> false;;

let player_to_string player = match player with
  | Player(n) -> "joueur "^(string_of_int n)
  | NamedPlayer(name) -> name
  | Robot(name) -> ("robot \""^name^"\"");;

let player_queue = 
  let queue = Queue.create () in
  Queue.add (Player(2)) queue;
  Queue.add (Player(1)) queue;
  queue;;

let current_player = ref (NamedPlayer("Roger"));;




let last = ref 0;;


let jouer plateau idx = 

  if is_play_valid plateau idx !last  then begin
    plateau.plt.(idx-1)<-Cell(idx+1, Checked);
    last := idx;
    if check_victory plateau idx then
      print_string ("Victoire de "^(player_to_string !current_player)^" !\n")
    else begin
      affiche_plateau plateau;
      Queue.add !(plateau.current_player) plateau.player_queue;
      
      plateau.current_player := Queue.pop plateau.player_queue;
      print_string ("Au tour de "^(player_to_string !current_player)^".\n");
    end
  end else
    print_string "Coup impossible\n";;

let ligne = 2;;
let colonne = 2;;
let size = ligne*colonne;;

let plateau = creer_plateau ligne colonne;;

affiche_plateau plateau;;

print_string ("Au tour de "^(player_to_string !current_player)^".\n");;
