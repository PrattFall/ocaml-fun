open Result
open Tsdl

type sdl_err =
  | InitError             of string
  | CreateWindowError     of string
  | GetWindowSurfaceError of string
  | ImageLoadFail         of string
  | CouldNotWait          of string

let show_error e =
  let show s e = print_endline (s ^ ": " ^ e) in

  match e with
  | InitError e             -> show "Init error" e
  | CreateWindowError e     -> show "Failed to create window" e
  | GetWindowSurfaceError e -> show "Could not get window surface" e
  | ImageLoadFail e         -> show "Failed to load image" e
  | CouldNotWait e          -> show "Could not wait" e

exception SDLException of sdl_err

let load_surface path =
  match Sdl.load_bmp path with
  | Error (`Msg e) -> raise (SDLException (ImageLoadFail e))
  | Ok surface     -> surface

module KeyPressSurfaces = struct
  type surface =
    { default : Sdl.surface
    ; up      : Sdl.surface
    ; down    : Sdl.surface
    ; left    : Sdl.surface
    ; right   : Sdl.surface
    }

  let load () =
    { default = load_surface "press.bmp"
    ; up      = load_surface "up.bmp"
    ; down    = load_surface "down.bmp"
    ; left    = load_surface "left.bmp"
    ; right   = load_surface "right.bmp"
    }

  let default s = s.default
  let up s = s.up
  let down s = s.down
  let left s = s.left
  let right s = s.right
end

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

let close window surfaces =
  List.iter Sdl.free_surface surfaces;
  Sdl.destroy_window window

let wait_event e =
  match Sdl.wait_event e with
  | Error (`Msg e) -> raise (SDLException (CouldNotWait e))
  | Ok () -> ()

let update_window_surface window window_surface surface =
  Sdl.blit_surface surface None window_surface None |> ignore;
  Sdl.update_window_surface window |> ignore

let () =
  try
    let window         = init_window () in
    let window_surface = get_window_surface window in
    let surfaces = KeyPressSurfaces.load () in

    let update_surface = update_window_surface window window_surface in

    let e = Sdl.Event.create () in

    let rec loop () =
      wait_event (Some e);

      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> print_endline "Application terminated by user"
      | `Key_down ->
        (* This is where I wish this library had some docs *)
        (match Sdl.Event.(Sdl.Scancode.enum (get e keyboard_scancode)) with
          | `Up    -> update_surface (KeyPressSurfaces.up      surfaces)
          | `Down  -> update_surface (KeyPressSurfaces.down    surfaces)
          | `Left  -> update_surface (KeyPressSurfaces.left    surfaces)
          | `Right -> update_surface (KeyPressSurfaces.right   surfaces)
          | _      -> update_surface (KeyPressSurfaces.default surfaces));
        loop ()
      | _ -> loop ()
    in

    update_surface (KeyPressSurfaces.default surfaces);

    loop ();

    close window [ window_surface
                 ; KeyPressSurfaces.default surfaces
                 ; KeyPressSurfaces.up      surfaces
                 ; KeyPressSurfaces.down    surfaces
                 ; KeyPressSurfaces.left    surfaces
                 ; KeyPressSurfaces.right   surfaces
                 ];
    Sdl.quit ();
    exit 0
  with
  | SDLException e -> (show_error e; exit 1)
