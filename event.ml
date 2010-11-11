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
  ignore(List.fold_right
    (fun (window, { callback }) consumed ->
      if not consumed then
        match event with
          | MouseDown p ->
              (if Rect.is_in (Window.abs_pos window) p 
               then (focused_window := Some (window, from_window, callback) ; callback from_window (pre_process_event window event)) else consumed)
            | MouseUp p ->
              (if Rect.is_in (Window.abs_pos window) p
               then (callback from_window (pre_process_event window event))
               else consumed)
          | MouseMotion _-> 
            (match !focused_window with
              | None -> consumed
              | Some (window', from_window, callback) -> 
                if window == window' then callback from_window (pre_process_event window event) else consumed)
          | _ -> callback from_window event
       else consumed) !signals false);
  match event with
    | MouseUp p ->
      (match !focused_window with
        | None -> ()
        | Some (window, from_window, callback) -> 
          callback from_window (pre_process_event window event); ())
    | _ -> ()

let run_events from_window event =
  match event with
    | MouseMotion p
    | MouseUp p
    | MouseDown p -> BatOption.map
      (fun windows ->
        let rec event_loop = function
          | window :: rest_windows -> 
            (try 
               let { callback } = List.assoc window !signals in
               (match event with
                 | MouseUp _ ->
                   BatOption.map 
                     (fun (window', from_window, callback) ->
                       if window == window' then
                         if callback from_window (pre_process_event window event) then
                           focused_window := None
                         else event_loop rest_windows)
                     !focused_window; ()
                 | MouseMotion _ ->
                   BatOption.map 
                     (fun (window', from_window, callback) ->
                       if window == window' then
                         if not (callback from_window (pre_process_event window event)) then
                         () (* event_loop rest_windows *))
                     !focused_window; ()
                 | MouseDown _ ->
               if callback from_window (pre_process_event window event) then
                 focused_window := Some (window, from_window, callback)
               else event_loop rest_windows)
             with _ -> event_loop rest_windows)
          | [] -> ()
        in
        event_loop windows) (Window.find_window p Window.desktop); 
      ()

let mouse_handler ~button ~state ~x ~y = run_events Window.desktop
  (match state with
    | Glut.DOWN -> MouseDown (x, y)
    | Glut.UP -> MouseUp (x, y))

let mouse_motion_handler ~x ~y = run_events Window.desktop (MouseMotion (x,y))
