type cell_state = Unchecked | Checked;;
type cell = Cell of int * cell_state;;
type player = Player of int
| NamedPlayer of string
| Robot of string;;
type plateau = Plateau of cell array * int * int;;
exception CoupImpossible;;




let creer_plateau ligne colonne = 
  Plateau(Array.init (ligne*colonne) (fun i -> Cell(i+1, Unchecked)), ligne, colonne);;


let affiche_plateau plateau =

  match plateau with
  | Plateau(plt, l, c) -> begin

  let nb_of_char n = String.length (string_of_int n) in
  let char_per_cell = nb_of_char (l*c) in
  for i=0 to l-1 do
    print_string "|";
    for j=0 to c-1 do
      
      
      (match (plt.(j+i*c)) with 
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
  done
  end;;

let mult_or_divi a b =
  if b <> 0 && a <> 0 then
    a mod b = 0 || b mod a = 0
  else
    false;;

let check_victory plateau coup = 
  let v = ref true in
  match plateau with
  | Plateau(plt, l, c) -> begin
    for i=0 to ((l*c)-1) do
      match plt.(i) with 
      | Cell(idx, Unchecked) -> 
        if mult_or_divi coup idx then
          v := false;
      | _ -> ();
    done;
  end;
  !v;;

let is_play_valid plateau coup last = match plateau with
  | Plateau(plt, _, _) -> match plt.(coup-1) with
    | Cell(idx, Unchecked) -> mult_or_divi idx last
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

let ligne = 2;;
let colonne = 2;;
let size = ligne*colonne;;


let last = ref 0;;


let jouer plateau idx = 
  match plateau with
  | Plateau(plt, _, _) -> begin

  if !last = 0 || is_play_valid plateau idx !last  then begin

    plt.(idx-1)<-Cell(idx+1, Checked);
    last := idx;
    if check_victory plateau idx then
      print_string ("Victoire de "^(player_to_string !current_player)^" !\n");
    
    affiche_plateau plateau;
    Queue.add !current_player player_queue;
    
    current_player := Queue.pop player_queue;
    print_string ("Au tour de "^(player_to_string !current_player)^".\n");
  end else
    print_string "Coup impossible\n";
  ()
  end;;


let plateau = creer_plateau ligne colonne;;

affiche_plateau plateau;;

print_string ("Au tour de "^(player_to_string !current_player)^".\n");;



