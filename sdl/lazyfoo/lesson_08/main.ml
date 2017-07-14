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
  match Sdl.create_window ~w:Screen.width ~h:Screen.height "SDL OpenGL" Sdl.Window.opengl with
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

let () =
  try
    init_video ();
    let window = get_window () in
    let renderer = get_renderer window in

    let e = Sdl.Event.create () in

    let do_things () =
      ignore (Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF);
      ignore (Sdl.render_clear renderer);

      (* Render red filled quad *)
      let fill_rect =
        Sdl.Rect.create
          ~x:(Screen.width / 4)
          ~y:(Screen.height / 4)
          ~w:(Screen.width / 2)
          ~h:(Screen.height / 2)
      in
      ignore (Sdl.set_render_draw_color renderer 0xFF 0x00 0x00 0xFF);
      ignore (Sdl.render_fill_rect renderer (Some fill_rect));

      (* Render green outlined quad *)
      let outline_rect =
        Sdl.Rect.create
          ~x:(Screen.width / 6)
          ~y:(Screen.height / 6)
          ~w:(Screen.width * 2/ 3)
          ~h:(Screen.height * 2 / 3)
      in
      ignore (Sdl.set_render_draw_color renderer 0x00 0xFF 0x00 0xFF);
      ignore (Sdl.render_draw_rect renderer (Some outline_rect));

      (* Draw blue horizontal line *)
      ignore (Sdl.set_render_draw_color renderer 0x00 0x00 0xFF 0xFF);
      ignore (Sdl.render_draw_line renderer 0 (Screen.height / 2) Screen.width (Screen.height / 2));

      (* Draw vertical line of yellow dots *)
      ignore (Sdl.set_render_draw_color renderer 0xFF 0xFF 0x00 0xFF);
      let rec loop_dot x =
        ignore (Sdl.render_draw_point renderer (Screen.width / 2) x);

        if x < Screen.height
        then loop_dot (x + 4)
        else ();
      in

      loop_dot 0;

      ignore (Sdl.render_present renderer)
    in

    let rec loop () =
      wait_event (Some e);

      match get_event_type e with
      | `Quit -> ()
      | _     -> do_things (); loop ()
    in

    loop ();

    close window renderer [ ];
    Image.quit ();
    Sdl.quit ();
    exit 0
  with
  | SDLException e -> (show_error e; exit 1)
