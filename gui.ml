open Draw
open Event
open BatPervasives
open Widget
let font_size = 11
(* let face = ref None *)

(*
   Higher level events:
   - click
   - double click
   - vertical drag
   - horizontal drag
   - drag & drop

   Motion:
   - pinned
   - horizontal pinned
   - vertical pinned
   - drag drop
   - drag-drop to some class
   - drag-drop to parent
   - drag-drop to children

   Layouts:
   - free for parent
   - horizontal
   - vertical
   - anchored left, right, top, bottom
   - left, right, top, bottom edge

*)

let mouse_state = ref (Glut.LEFT_BUTTON, Glut.UP)

open BatFloat
let with_view f =
  GlMat.mode `projection;
  GlMat.push();
  GlMat.load_identity ();
  let w,h = Display. display_size in
    GlDraw.viewport ~x:0 ~y:0 ~w:w ~h:h;
    GlMat.ortho ~x:(0.,float w) ~y:(float h,0.0) ~z:(0.0,1.);
    let res = f() in
    GlMat.mode `projection;
    GlMat.pop();
    GlMat.mode `modelview;
    res

let display () =
  Gl.disable `scissor_test;
  GlClear.color (214./255., 214./.255., 206./.255.);
  GlClear.clear [ `color; `depth];
  GluMat.perspective ~fovy:60. ~aspect:1.6 ~z:(0.1,100.0);
  Gl.disable `depth_test ;
  Gl.disable `cull_face ;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.mode `modelview;
  GlMat.load_identity ();
  with_view (fun () -> Window.draw_desktop ());
  let w,h = Display. display_size in
  Glut.swapBuffers ();
  ignore(Unix.select [] [] [] 0.001)
;;

let on_mouse_motion ~x ~y = Event.mouse_motion_handler ~x ~y

let on_mouse  ~button ~state ~x ~y =
  print_endline (Glut.string_of_button_state state);
  flush stdout;
  mouse_state := (button, state);
  Event.mouse_handler ~button ~state ~x ~y


let init build =
  ignore( Glut.init Sys.argv );
  Glut.initDisplayMode ~double_buffer:true ~depth:true ();
  ignore (Glut.createWindow ~title:"OpenGL Demo");
  build ();
  Window.shelf (Rect.rect (0,0) Display.display_size);
  Glut.displayFunc ~cb:display;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.motionFunc ~cb:on_mouse_motion;
  Glut.mouseFunc ~cb:on_mouse;
  Glut.mainLoop ()
;;

