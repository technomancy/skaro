(* ocamlfind ocamlc -linkpkg -thread -package core skaro.ml -o skaro *)

open Core.Std

type board =
    { width: int; height: int;
      enemies: (int * int) list; player: (int * int); piles: (int * int) list }

(* rules *)

let move { player = (x,y); width; height} input =
  match input with
    | Some "up" -> Some (x, y + 1)
    | Some "down" -> Some (x, y - 1)
    | Some "left" -> Some (x - 1, y)
    | Some "right" -> Some (x + 1, y)
    | Some "teleport" -> Some (Random.int width, Random.int height)
    | _ -> None

let allowed { width; height } (x,y) =
  x >= 0 && x < width && y >= 0 && y < height

let collision obstacles position =
  List.length (List.filter obstacles ((=) position)) > 1

let get_collisions enemies obstacles =
  List.filter enemies (collision obstacles)

let killed { player; enemies; piles } =
  List.mem (enemies @ piles) player

(* drawing *)

let place marker rows position =
  List.Assoc.add rows position marker

let get_piece pieces row col =
  match List.Assoc.find pieces (col,row) with
    | Some x -> x
    | None -> "-"

let get_pieces { player; enemies; piles } =
  let pieces = [(player, "O")] in
  let pieces = List.fold ~init:pieces ~f:(place "M") enemies in
  let pieces = List.fold ~init:pieces ~f:(place "X") piles in
  pieces

let draw_row pieces width row =
  let _ =  List.init (width + 1) (Fn.compose (printf " %s ")
                                    (get_piece pieces row)) in
  printf "%s" "\n"

let draw_board board =
  let pieces = get_pieces board in
  let _ = List.init (board.height - 1) (draw_row pieces board.width) in
  printf "%s%!" "\n"

(* game loop *)

let move_player board input =
  match move board input with
    | Some new_position -> { board with player = new_position }
    | None -> board

let move_enemy (px,py) (x,y) =
  let (dx,dy) = (px - x, py - y) in
  if (abs dx) > (abs dy) then
    if dx > 0 then
      (x + 1, y) else
      (x - 1, y) else
    if dy > 0 then
      (x, y + 1) else
      (x, y - 1)

let move_enemies board =
  { board with enemies = List.map board.enemies (move_enemy board.player) }

let collisions ({ enemies; piles } as board) =
  let colls = get_collisions enemies enemies @ piles in
  let surviving = (Fn.compose not (List.mem colls)) in
  { board with piles = piles @ colls;
               enemies = List.filter enemies surviving }

let round board input =
  collisions (move_enemies (move_player board input))

let rec play board input =
  if killed board then
    printf "You died.\n"
  else if input = Some "quit" then
    printf "Bye.\n"
  else if List.length board.enemies = 0 then
    printf "You won. Nice job.\n"
  else
    let board = (round board input) in
    draw_board board;
    play board (In_channel.input_line In_channel.stdin)

let make_board width height enemies =
  { width = width; height = height; piles = [];
    enemies = List.init enemies (fun _  -> (Random.int width, Random.int height));
    player = (Random.int width, Random.int height) }

let _ = Random.self_init ()

let _ = let board = make_board 10 10 4 in
        draw_board board;
        play board (In_channel.input_line In_channel.stdin)
