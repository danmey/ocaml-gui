open GL
open Draw

type window = {
  mutable pos : Rect.t;
  mutable children : window list;
  mutable painter : Rect.t -> unit;
}

open Rect
open BatFloat


let default_painter _ = ()

let default rect = { pos = rect; 
                     children = []; 
                     painter = default_painter }

let empty_window () = default Rect.o

let desktop = default Rect.o

let shelf rect = desktop.pos <- rect
open BatInt

let with_scisor rect f =
  let open BatInt in
      let screen_width, screen_height = Display.display_size () in
      let coords { Rect.x; y; w; h; } =
        (x, screen_height-y-h, w, h)
      in
    glMatrixMode GL_MODELVIEW;
    glLoadIdentity ();
    let x, y, width, height = coords rect in
    glScissor ~x ~y ~width ~height;
    glEnable GL_SCISSOR_TEST;
    f ();
    glDisable GL_SCISSOR_TEST

let desktop_rect () = Rect.rect (0,0) (Display.display_size ())

let rec draw_window window =
  let rec draw_client_window rect { pos; children; painter } =
    let client_rect = Rect.place_in pos rect in
    with_scisor rect (fun () ->
      painter client_rect;
      List.iter (draw_client_window (together rect client_rect)) children)
  in
  draw_client_window (desktop_rect ()) window

let draw_desktop () = 
  Texgen.update_texture();
  draw_window desktop

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
    | None -> []
    | Some path -> List.rev path

let abs_pos window =
  let path = window_path window in
  List.fold_left 
    (fun rect { pos } ->
      Rect.place_in pos rect) desktop.pos path

let find_window position =
  let rec loop rect window =
    let rect = Rect.place_in window.pos rect  in
    (if Rect.is_in rect position then
        [window] @ List.concat (List.map (loop rect) window.children)
     else 
        [])
  in
  List.rev (loop (Rect.rect (0,0) (0,0)) desktop)
      

  

let add parent window =
  parent.children <-  parent.children @ [window];
  ()

let remove parent window =
  parent.children <- BatList.remove_if ((==) window) parent.children;
  ()

let relative_pos window_relative window =
  let window_relative_pos = abs_pos window_relative in
  let window_pos = abs_pos window in
  Rect.subr window_relative_pos window_pos

let client_pos window global_pos = 
  Pos.sub global_pos (pos (abs_pos window))
