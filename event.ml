open Draw

type event =
  | MouseDown of Pos.t
  | MouseUp of Pos.t
  | MouseMotion of Pos.t
  | DoubleClick of Pos.t
  | Drag of Pos.t

type signal = { callback : (Window.window -> event -> bool); }

let signals : (Window.window * signal) list ref = ref []

let register window signal = signals :=  (window, { callback = signal }) :: !signals

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

let focused_window = ref None

let run_events from_window event =
  match event with
    | MouseDown p -> BatOption.may
      (fun windows ->
        (* let windows = List.rev windows in *)
        Printf.printf "hit_window: %s\n" (Rect.string_of_rect (List.hd windows).Window.pos);
        let rec event_loop = function
          | window :: rest_windows -> 
            (try 
               let { callback } = List.assoc window !signals in
               if callback from_window (pre_process_event window event) then
                 (Printf.printf "got_it: %s\n" (Rect.string_of_rect window.Window.pos);

                 focused_window := Some (window, from_window, callback))
               else event_loop rest_windows
             with _ -> event_loop rest_windows)
          | [] -> ()
        in
        event_loop windows) (Window.find_window p Window.desktop)
    | MouseUp p ->
      BatOption.may 
        (fun (window, from_window, callback) ->
          if callback from_window (pre_process_event window event) then
            focused_window := None)
        !focused_window
    | MouseMotion p ->
      BatOption.may
        (fun (window, from_window, callback) ->
          ignore(callback from_window (pre_process_event window event)))
        !focused_window
    | event -> List.iter (fun (window, { callback }) -> ignore(callback from_window (pre_process_event window event))) !signals

let mouse_handler ~button ~state ~x ~y = run_events Window.desktop
  (match state with
    | Glut.DOWN -> MouseDown (x, y)
    | Glut.UP -> MouseUp (x, y))

let mouse_motion_handler ~x ~y = run_events Window.desktop (MouseMotion (x,y))
