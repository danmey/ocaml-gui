open Glut
let panel_height = 20

let inited = ref false
let system_with_out str =
  let o = BatUnix.open_process_in ~autoclose:true ~cleanup:true str in
    BatString.strip (BatIO.read_all o)

let screen_width, screen_height =   
  let w,h =
    BatString.split
      (system_with_out "xrandr \\
                      | sed -n 's/^.*current \\([^ ]\\+\\) x \\([^ ]\\+\\), .*$/\\1 \\2/p'") " "
  in
    (* Printf.printf "'%s' '%s'" w h; *)
  (* flush stdout; *)
  int_of_string w, int_of_string h - panel_height

let display_size () =
  if not !inited then
    (ignore( Glut.glutInit Sys.argv );
     Glut.glutInitDisplayMode [Glut.GLUT_RGBA; Glut.GLUT_DEPTH; Glut.GLUT_DOUBLE];
     ignore (Glut.glutCreateWindow ~title:"OpenGL Demo");
     inited := true);
  glutGet GLUT_WINDOW_WIDTH, glutGet GLUT_WINDOW_HEIGHT

