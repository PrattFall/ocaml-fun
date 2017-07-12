open Result
open Tsdl

type sdl_err =
  | InitError             of string
  | CreateWindowError     of string
  | GetWindowSurfaceError of string
  | ImageLoadFail         of string
  | CouldNotWait          of string

exception SDLException of sdl_err

let show_error e =
  let show s e = print_endline (s ^ ": " ^ e) in

  match e with
  | InitError e             -> show "Init error" e
  | CreateWindowError e     -> show "Failed to create window" e
  | GetWindowSurfaceError e -> show "Could not get window surface" e
  | ImageLoadFail e         -> show "Failed to load image" e
  | CouldNotWait e          -> show "Could not wait" e

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
  match Sdl.load_bmp "x.bmp" with
  | Error (`Msg e) -> raise (SDLException (ImageLoadFail e))
  | Ok surface     -> surface

let close window surfaces =
  List.iter Sdl.free_surface surfaces;
  Sdl.destroy_window window

let wait_event e =
  match Sdl.wait_event e with
  | Error (`Msg e) -> raise (SDLException (CouldNotWait e))
  | Ok () -> ()

let () =
  try
    let window         = init_window () in
    let window_surface = get_window_surface window in
    let image_surface  = loadMedia () in

    let event_loop () =
      let e = Sdl.Event.create () in

      let rec loop () =
        wait_event (Some e);

        match Sdl.Event.(enum (get e typ)) with
        | `Quit -> print_endline "Application terminated by user"
        | _     -> loop ()
      in

      Sdl.blit_surface image_surface None window_surface None |> ignore;
      Sdl.update_window_surface window |> ignore;

      loop ()
    in

    event_loop();

    close window [image_surface; window_surface];
    Sdl.quit ();
    exit 0
  with
  | SDLException e -> (show_error e; exit 1)
