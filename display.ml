let panel_height = 0

let system_with_out str =
  let o = BatUnix.open_process_in ~autoclose:true ~cleanup:true str in
    BatString.strip (BatIO.read_all o)

let display_size =
  let w,h = 
    BatString.split 
      (system_with_out "xrandr \\
                      | sed -n 's/^.*current \\([^ ]\\+\\) x \\([^ ]\\+\\), .*$/\\1 \\2/p'") " "
  in
    (* Printf.printf "'%s' '%s'" w h; *)
    flush stdout;
    int_of_string w, int_of_string h - panel_height
