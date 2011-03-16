open Draw

type mouse_button =
  | Left
  | Right
  | Middle

type value = 
  | Float of float
  | Int of int

type event =
  | MouseDown of mouse_button * Pos.t
  | MouseUp of mouse_button * Pos.t
  | MouseMotion of mouse_button * Pos.t
  | DoubleClick of mouse_button * Pos.t
  | Custom of string * Pos.t * string
  | ValueChanged
  | Parameters of (string * value) list
  | Drag of Pos.t
  | KeyPress of char * Pos.t
  | SpecialKey of Glut.special_key * Pos.t

type signal = { callback : (Window.window -> event -> bool); }

let signals : (Window.window * signal) list ref = ref []

let register window signal = signals :=  (window, { callback = signal }) :: !signals

let pre_process_event window = 
  let last_motion_pos = ref None in
  function
  | MouseDown (b, p) -> MouseDown (b, (Window.client_pos window p))
  | MouseUp (b, p) -> MouseUp (b, Window.client_pos window p)
  | MouseMotion (b, p) -> (match !last_motion_pos with
      | None -> MouseMotion (b, (Window.client_pos window p))
      | Some last_pos -> 
        let new_pos = Pos.sub p last_pos in
        last_motion_pos := Some p;
        MouseMotion (b,(Window.client_pos window new_pos)))
  | a -> a

let focused_window = ref None

(* This got a bit complicated *)
let run_events from_window event =
  match event with
    | MouseDown (_, p)
    | KeyPress (_, p)
    | SpecialKey (_, p) ->
        (* let windows = List.rev windows in  *)
        let rec event_loop = function
          | window :: rest_windows -> 
            (try 
               let { callback } = List.assq window !signals in
               if callback from_window (pre_process_event window event) then
                 focused_window := Some (window, from_window, callback)
               else event_loop rest_windows
             with _ -> event_loop rest_windows)
          | [] -> ()
        in
        event_loop (Window.find_window p)
    | MouseUp (b, p) ->
      BatOption.may 
        (fun (window, from_window, callback) ->
          if callback from_window (pre_process_event window event) then
            focused_window := None)
        !focused_window
    | MouseMotion (b, p) ->
      BatOption.may
        (fun (window, from_window, callback) ->
          ignore(callback from_window (pre_process_event window event)))
        !focused_window
    | event -> List.iter 
      (fun (window, { callback }) -> 
        ignore
          (callback from_window 
             (pre_process_event window event))) !signals

let mouse_handler ~button ~state ~x ~y = 
  let b = match button with
    | Glut.GLUT_LEFT_BUTTON -> Left
    | Glut.GLUT_MIDDLE_BUTTON -> Middle
    | Glut.GLUT_RIGHT_BUTTON -> Right
    | _ -> Middle
  in
  run_events Window.desktop
    (match state with
      | Glut.GLUT_DOWN -> MouseDown (b, (x, y))
      | Glut.GLUT_UP -> MouseUp (b, (x, y)))

let keyboard_handler ~key ~x ~y =
  run_events Window.desktop (KeyPress (key, (x,y)))

let special_handler ~key ~x ~y =
  run_events Window.desktop (SpecialKey (key, (x,y)))
  

let mouse_motion_handler ~x ~y = 
  run_events Window.desktop (MouseMotion (Left, (x,y)))
