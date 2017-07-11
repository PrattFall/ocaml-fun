(*
 * Conway's Game of Life in OCaml
 * ---
 * Arrays used for update performance
 * NOT a pure module due to updating the state of the arrays in place
 *)

type t = { width  : int
         ; height : int
         ; grid   : bool array array
         }

type neighborhood = { top : int
                    ; right : int
                    ; down : int
                    ; left : int
                    }

let (%) f1 f2 a = f1 (f2 a)

let make width height =
  let make_grid =
    Array.init height (fun _ -> Array.make width false)
  in

  { width  = width
  ; height = height
  ; grid   = make_grid
  }

let get x y g_grid =
  g_grid.grid.(y).(x)

let set g_grid x y value =
  g_grid.grid.(y).(x) <- value

let get_neighbor_bounds x y grid =
  { top   = if y > 0 then y - 1 else grid.height - 1
  ; left  = if x > 0 then x - 1 else grid.width  - 1
  ; down  = if y < (grid.height - 1) then y + 1 else 0
  ; right = if x < (grid.width  - 1) then x + 1 else 0
  }

let get_neighbors x y grid =
  let n = get_neighbor_bounds x y grid in

  [
    get n.left  n.top  grid;
    get n.left  y      grid;
    get n.left  n.down grid;
    get x       n.top  grid;
    get x       n.down grid;
    get n.right n.top  grid;
    get n.right y      grid;
    get n.right n.down grid;
  ]

let count_living x y grid =

  let add_if_alive acc x =
    if x then acc + 1 else acc
  in

  get_neighbors x y grid
  |> List.fold_left add_if_alive 0

let map f grid =
  Array.map (fun y -> Array.map (fun x -> f x) y) grid

let mapi f grid =
  Array.mapi (fun yi y -> Array.mapi (fun xi _ -> f xi yi) y) grid

let get_grid grid = grid.grid

let check_life grid x y =
  let living = get x y grid in
  let numLiving = count_living x y grid in

  if living && (numLiving < 2 || numLiving > 3)
  then false
  else
  if not living && numLiving = 3
  then true
  else living

let clear grid =
  { grid with grid = map (fun _ -> false) grid.grid }

let update grid =
  let updates =
    mapi (check_life grid) grid.grid
  in

  let set_updated x y = set grid x y (updates.(y).(x)) in

  Array.iteri (fun yi y ->
      Array.iteri (fun xi _ -> set_updated xi yi) y) grid.grid

let string_of_node x =
  match x with
  | true  -> "█"
  | false -> "░"

let string_of_grid grid =
  let string_concat del = String.concat del % Array.to_list in

  grid.grid
  |> Array.map (string_concat "" % Array.map string_of_node)
  |> string_concat "\n"

let print_grid = print_endline % string_of_grid
