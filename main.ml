type cell_state = Unchecked | Checked;;
type cell = Cell of int * cell_state;;
type bot_level = Random | Normal;;
type player = Player of int
| NamedPlayer of string
| Robot of string * bot_level;;
(*type plateau = Plateau of cell array * int * int;;*)
type plateau = {
  plt: cell array;
  ligne: int;
  colonne: int;
  player_queue: player Queue.t;
  current_player: player ref;
  last: int ref;
}
exception CoupImpossible;;




let creer_plateau ligne colonne = {
  plt = (Array.init (ligne*colonne) (fun i -> Cell(i+1, Unchecked))); 
  ligne = ligne; 
  colonne = colonne;
  player_queue = Queue.create ();
  current_player = ref (Robot("Robert", Random));
  last = ref 0;
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

let is_play_valid plateau coup = 
  if coup < 1 || coup > (plateau.ligne*plateau.colonne) then begin
    false
  end else
  match plateau.plt.(coup-1) with
    | Cell(idx, Unchecked) -> !(plateau.last) = 0 || mult_or_divi idx !(plateau.last)
    | Cell(_, Checked) -> false;;

let player_to_string player = match player with
  | Player(n) -> "joueur "^(string_of_int n)
  | NamedPlayer(name) -> name
  | Robot(name, _) -> ("robot \""^name^"\"");;


let possible_plays plateau = let l: int list ref = ref [] in
  for i=1 to plateau.ligne*plateau.colonne do 
    if is_play_valid plateau i then
      l := List.append !l [i];
  done;
  !l;;


let rec jouer plateau idx = 

  let rec bot_turn plateau level = 
    let rec get l n = match l with
    | [] -> 0
    | a::r -> if n=0 then a else get r n-1 in
    match level with
    | Random -> let l = possible_plays plateau in
      let coup = get l (Random.int (List.length l)) in
      print_string ((player_to_string !(plateau.current_player))^" joue "^(string_of_int (coup))^".\n");
      jouer plateau coup;
    | Normal -> () in

  if is_play_valid plateau idx  then begin
    plateau.plt.(idx-1)<-Cell(idx+1, Checked);
    plateau.last := idx;
    if check_victory plateau idx then
      print_string ("Victoire de "^(player_to_string !(plateau.current_player))^" !\n")
    else begin
      affiche_plateau plateau;
      Queue.add !(plateau.current_player) plateau.player_queue;
      plateau.current_player := Queue.pop plateau.player_queue;
      print_string ("Au tour de "^(player_to_string !(plateau.current_player))^".\n");

      match !(plateau.current_player) with 
      | Robot(_, lvl) -> bot_turn plateau lvl
      | NamedPlayer(_) -> jouer plateau (read_int())
      | _ -> ()

    end
  end else
    print_string "Coup impossible\n";;

let ligne = 5;;
let colonne = 5;;
let size = ligne*colonne;;

let plateau = creer_plateau ligne colonne;;


Queue.add (Robot("Clavier", Random)) plateau.player_queue;;

plateau.current_player := NamedPlayer("Roger");;

affiche_plateau plateau;;

print_string ("Au tour de "^(player_to_string !(plateau.current_player))^".\n");;
