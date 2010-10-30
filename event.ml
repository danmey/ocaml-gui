open Draw

type event =
  | MouseDown of Pos.t
  | MouseUp of Pos.t
  | MouseMotion of Pos.t
  | DoubleClick of Pos.t
  | Drag of Pos.t


type signal = { callback : (Window.window -> event -> unit); }

let signals : (Window.window, signal) Hashtbl.t = Hashtbl.create 100

let register window signal = Hashtbl.add signals window { callback = signal }

let pre_process_event window = 
  let last_motion_pos = ref None in
  function
  | MouseDown p -> MouseDown (Window.client_pos window p)
  | MouseUp p -> MouseUp (Window.client_pos window p)
  | MouseMotion p -> (match !last_motion_pos with
      | None -> MouseMotion (Window.client_pos window p)
      | Some last_pos -> 
        let new_pos = Pos.sub p last_pos in
        last_motion_pos := Some p;
        MouseMotion (Window.client_pos window new_pos))
  | a -> a

let run_events from_window event =
  Hashtbl.iter 
    (fun window { callback } ->
      match event with
        | MouseDown p 
        | MouseUp p ->
            (if Rect.is_in (Window.abs_pos window) p 
            then callback window (pre_process_event window event))
        | MouseMotion p -> callback window (pre_process_event window event)
        | a -> callback from_window event) signals

let mouse_handler ~button ~state ~x ~y = run_events Window.desktop
  (match state with
    | Glut.DOWN -> MouseDown (x, y)
    | Glut.UP -> MouseUp (x, y))

let mouse_motion_handler ~x ~y = run_events Window.desktop (MouseMotion (x,y))
