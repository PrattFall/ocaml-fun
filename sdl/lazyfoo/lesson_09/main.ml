open Result
open Tsdl
open Tsdl_image

module Screen = struct
  let width  = 640
  let height = 480
end

type sdl_err =
  | InitError               of string
  | CreateWindowError       of string
  | CreateRendererError     of string
  | GetWindowSurfaceError   of string
  | ImageLoadFail           of string
  | CouldNotWait            of string
  | CouldNotOptimizeSurface of string
  | CouldNotAllocate        of string
  | CouldNotLoadTexture     of string

exception SDLException of sdl_err

let show_error e =
  let show s e = print_endline (s ^ ": " ^ e) in

  match e with
  | InitError e               -> show "Init error" e
  | CreateWindowError e       -> show "Failed to create window" e
  | CreateRendererError e     -> show "Failed to create renderer" e
  | GetWindowSurfaceError e   -> show "Could not get window surface" e
  | ImageLoadFail e           -> show "Failed to load image" e
  | CouldNotWait e            -> show "Could not wait" e
  | CouldNotOptimizeSurface e -> show "Could not optimize surface" e
  | CouldNotAllocate e        -> show "Could not allocate" e
  | CouldNotLoadTexture e     -> show "Could not load texture" e

let init_video () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> raise (SDLException (InitError e))
  | Ok () -> ()

let get_window () =
  let window_result =
    Sdl.create_window
      ~w:Screen.width
      ~h:Screen.height
      "SDL OpenGL"
      Sdl.Window.opengl
  in

  match window_result with
  | Error (`Msg e) -> raise (SDLException (CreateWindowError e))
  | Ok w -> w

let get_renderer w =
  match Sdl.create_renderer w ~index:(-1) ~flags:Sdl.Renderer.accelerated with
  | Error (`Msg e) -> raise (SDLException (CreateRendererError e))
  | Ok r -> r

let close window renderer textures =
  List.iter Sdl.destroy_texture textures;
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window

let wait_event e =
  match Sdl.wait_event e with
  | Error (`Msg e) -> raise (SDLException (CouldNotWait e))
  | Ok () -> ()

let get_event_type e =
  Sdl.Event.(enum (get e typ))

let top_left_viewport =
  Sdl.Rect.create
    ~x:0
    ~y:0
    ~w:(Screen.width / 2)
    ~h:(Screen.height / 2)

let top_right_viewport =
  Sdl.Rect.create
    ~x:(Screen.width / 2)
    ~y:0
    ~w:(Screen.width / 2)
    ~h:(Screen.height / 2)

let bottom_viewport =
  Sdl.Rect.create
    ~x:0
    ~y:(Screen.height / 2)
    ~w:Screen.width
    ~h:(Screen.height / 2)

let load_initial_surface path =
  match Image.load path with
  | Error (`Msg e) -> raise (SDLException (ImageLoadFail e))
  | Ok surface     -> surface

let load_texture renderer path =
  let loaded_surface = load_initial_surface path in
  match Sdl.create_texture_from_surface renderer loaded_surface with
  | Error (`Msg e) -> raise (SDLException (CouldNotLoadTexture e))
  | Ok texture ->
    Sdl.free_surface loaded_surface;
    texture

let () =
  try
    init_video ();
    let window = get_window () in
    let renderer = get_renderer window in
    let texture = load_texture renderer "viewport.png" in

    let e = Sdl.Event.create () in

    let do_things () =
      ignore (Sdl.render_set_viewport renderer (Some top_left_viewport));
      ignore (Sdl.render_copy renderer texture);

      ignore (Sdl.render_set_viewport renderer (Some top_right_viewport));
      ignore (Sdl.render_copy renderer texture);

      ignore (Sdl.render_set_viewport renderer (Some bottom_viewport));
      ignore (Sdl.render_copy renderer texture);

      ignore (Sdl.render_present renderer)
    in

    let rec loop () =
      wait_event (Some e);

      match get_event_type e with
      | `Quit -> ()
      | _     -> do_things (); loop ()
    in

    loop ();

    close window renderer [ texture ];
    Image.quit ();
    Sdl.quit ();
    exit 0
  with
  | SDLException e -> (show_error e; exit 1)
