open Result
open Tsdl

type sdl_err =
  | InitError of string
  | CreateWindowError of string
  | GetWindowSurfaceError of string
  | ImageLoadFail of string

exception SDLException of sdl_err

let show_error e =
  let show s e = print_endline (s ^ ": " ^ e) in

  match e with
  | InitError e             -> show "Init error" e
  | CreateWindowError e     -> show "Failed to create window" e
  | GetWindowSurfaceError e -> show "Could not get window surface" e
  | ImageLoadFail e         -> show "Failed to load image" e

let init_video () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> raise (SDLException (InitError e))
  | Ok () -> ()

let get_window () =
  match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
  | Error (`Msg e) -> raise (SDLException (CreateWindowError e))
  | Ok w -> w

let get_window_surface w =
  match Sdl.get_window_surface w with
  | Error (`Msg e)    -> raise (SDLException (GetWindowSurfaceError e))
  | Ok window_surface -> window_surface

let init_window () =
  init_video ();
  get_window ()

let loadMedia () =
  match Sdl.load_bmp "hello_world.bmp" with
  | Error (`Msg e) -> raise (SDLException (ImageLoadFail e))
  | Ok surface     -> surface

let close window surfaces =
  List.iter Sdl.free_surface surfaces;

  Sdl.destroy_window window

let () =
  try
    print_endline "Starting...";
    let window = init_window () in
    let window_surface = get_window_surface window in
    let image_surface = loadMedia () in

    Sdl.blit_surface image_surface None window_surface None |> ignore;
    Sdl.update_window_surface window |> ignore;
    Sdl.delay 2000l;
    close window [image_surface; window_surface];
    Sdl.quit ();
    exit 0
  with
  | SDLException e -> (show_error e; exit 1)
