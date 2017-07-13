open Result
open Tsdl
(* Merlin's having trouble with this one for some reason, but it compiles *)
open Tsdl_image

module Screen = struct
  let width  = 640
  let height = 480
end

type sdl_err =
  | InitError               of string
  | CreateWindowError       of string
  | GetWindowSurfaceError   of string
  | ImageLoadFail           of string
  | CouldNotWait            of string
  | CouldNotOptimizeSurface of string
  | CouldNotAllocate        of string

exception SDLException of sdl_err

let show_error e =
  let show s e = print_endline (s ^ ": " ^ e) in

  match e with
  | InitError e               -> show "Init error" e
  | CreateWindowError e       -> show "Failed to create window" e
  | GetWindowSurfaceError e   -> show "Could not get window surface" e
  | ImageLoadFail e           -> show "Failed to load image" e
  | CouldNotWait e            -> show "Could not wait" e
  | CouldNotOptimizeSurface e -> show "Could not optimize surface" e
  | CouldNotAllocate e        -> show "Could not allocate" e

let load_surface window_surface path =
  match Image.load path with
  | Error (`Msg e) -> raise (SDLException (ImageLoadFail e))
  | Ok surface     ->
    (match Sdl.convert_surface surface window_surface with
     | Error (`Msg e) -> raise (SDLException (CouldNotOptimizeSurface e))
     | Ok optimized_surface ->
       Sdl.free_surface surface;
       optimized_surface)

let init_video () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> raise (SDLException (InitError e))
  | Ok () -> ()

let get_window () =
  match Sdl.create_window ~w:Screen.width ~h:Screen.height "SDL OpenGL" Sdl.Window.opengl with
  | Error (`Msg e) -> raise (SDLException (CreateWindowError e))
  | Ok w -> w

let get_window_surface w =
  match Sdl.get_window_surface w with
  | Error (`Msg e) -> raise (SDLException (GetWindowSurfaceError e))
  | Ok window_surface -> window_surface

let init_window () =
  init_video ();
  get_window ()

let close window surfaces =
  List.iter Sdl.free_surface surfaces;
  Sdl.destroy_window window

let wait_event e =
  match Sdl.wait_event e with
  | Error (`Msg e) -> raise (SDLException (CouldNotWait e))
  | Ok () -> ()

let update_window_surface window window_surface surface =
  let stretch =
    Sdl.Rect.create ~x:0 ~y:0 ~w:Screen.width ~h:Screen.height
  in

  ignore (Sdl.blit_scaled ~src:surface stretch
                          ~dst:window_surface (Some stretch));

  ignore (Sdl.update_window_surface window)

let surface_format surface =
  match Sdl.alloc_format (Sdl.get_surface_format_enum surface) with
  | Error (`Msg e) -> raise (SDLException (CouldNotAllocate e))
  | Ok format -> format

let () =
  try
    let window = init_window () in
    let window_surface = get_window_surface window in
    let surface = load_surface (surface_format window_surface) "loaded.png" in
    let update_surface = update_window_surface window window_surface in

    update_surface surface;
    Sdl.delay 3000l;

    close window [ window_surface
                 ; surface
                 ];
    Sdl.quit ();
    exit 0
  with
  | SDLException e -> (show_error e; exit 1)
