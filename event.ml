open Draw

type event =
  | Click of Pos.t
  | DoubleClick of Pos.t
  | Drag

type signal = { callback : (event -> unit); }

let signals : (Window.window, signal) Hashtbl.t = Hashtbl.create 100

let register window signal = Hashtbl.add signals window { callback = signal }


let run_events event =
  Hashtbl.iter 
    (fun window { callback } ->
      callback event) signals

let mouse_handler ~button ~state ~x ~y = run_events (Click (x, y))
