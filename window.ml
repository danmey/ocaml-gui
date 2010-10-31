open Draw

type window = {
  mutable pos : Rect.t;
  mutable children : window list;
  mutable painter : Rect.t -> unit;
}

open Rect
open BatFloat

let center_coord c = float c + 0.5

let center_rect = lift22 center_coord float


let default_painter rect =
   let x, y, w, h = center_rect rect in
   GlDraw.begins `line_loop;
   GlDraw.color (1.,1.,1.);
   GlDraw.vertex ~x ~y ();
   GlDraw.vertex ~x:(x + w) ~y ();
   GlDraw.vertex ~x:(x + w) ~y:(y + h) ();
   GlDraw.vertex ~x ~y:(y + h) ();
   GlDraw.ends ();
   ()

let default rect = { pos = rect; children = []; painter = default_painter }

let empty_window () = default Rect.o
let desktop = default Rect.o

let shelf rect = desktop.pos <- rect
open BatInt

let with_scisor rect f =
  let screen_width, screen_height = Display.display_size in
  let coords {Rect.x; y; w; h; } =
    let open BatInt in
        (x, screen_height-y-h, w, h)
    in
    GlMat.mode `modelview;
    GlMat.load_identity ();
    let x, y, width, height = coords rect in
    GlMisc.scissor ~x ~y ~width ~height;
    Gl.enable `scissor_test;
    f ();
    Gl.disable `scissor_test

let desktop_rect = Rect.rect (0,0) Display.display_size

let rec draw_window window =
  let rec draw_client_window rect { pos; children; painter } =
    let client_rect = Rect.place_in pos rect in
    with_scisor rect (fun () ->
      painter client_rect;
      List.iter (draw_client_window (together rect client_rect)) children)
  in
  draw_client_window desktop_rect window

let draw_desktop () = 
  draw_window desktop

let find_window position window =
  let rec find_loop = function
    | [] -> None
    | { pos; children } :: rest ->
      (match children with
        | [] -> 
          if Rect.is_in pos position then 
            Some window
          else find_loop rest
        | windows -> find_loop windows)
  in
  find_loop [window]

let window_path window =
  let bool_of_option = function Some _ -> true | None -> false  in
  let rec find_loop path ({ children; } as window') =
    if window' == window 
    then Some path 
    else
      match children with
        | [] -> None
        | windows ->
          try List.find bool_of_option
            (List.map (fun w -> find_loop (w :: path) w) windows)
          with _ -> None
  in
  match find_loop [] desktop with
    | None -> failwith "window_path: window not found."
    | Some path -> List.rev path

let add parent window =
  parent.children <-  window :: parent.children;
  window

let abs_pos window =
  let path = window_path window in
  List.fold_left 
    (fun rect { pos } -> 
      Rect.place_in pos rect) desktop.pos path

let relative_pos window_relative window =
  let window_relative_pos = abs_pos window_relative in
  let window_pos = abs_pos window in
  Rect.subr window_relative_pos window_pos

let client_pos window global_pos = 
  Pos.sub global_pos (pos (abs_pos window))
