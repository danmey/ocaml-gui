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
open GL
open Glu
let mouse_state = ref (Glut.GLUT_LEFT_BUTTON, Glut.GLUT_UP)

open BatFloat
let with_view f =
  glMatrixMode GL_PROJECTION;
  glPushMatrix();
  glLoadIdentity ();
  let w,h = Display.display_size () in
    glViewport ~x:0 ~y:0 ~width:w ~height:h;
    glOrtho ~left:0. ~right:(float w) ~bottom:(float h) ~top:0.0 ~near:0.0 ~far:1.;
    let res = f () in
    glMatrixMode GL_PROJECTION;
    glPopMatrix ();
    glMatrixMode GL_MODELVIEW;
    res

let display () =
  glDisable GL_SCISSOR_TEST;
  (* glClearColor ~r:(214./255.) ~g:(214./.255.) ~b:(206./.255.) ~a:0.0; *)
  glClearColor ~r:1. ~g:1. ~b:1. ~a:1.;
  glClear [ GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
  let w,h = Display.display_size () in
  let aspect = float w /. float h in
  gluPerspective ~fovy:60. ~aspect ~zNear:0.1 ~zFar:100.;
  glDisable GL_DEPTH_TEST ;
  glDisable GL_CULL_FACE ;
  glMatrixMode GL_PROJECTION;
  glLoadIdentity ();
  glMatrixMode GL_MODELVIEW;
  glLoadIdentity ();
  with_view (fun () -> Window.draw_desktop ());
  Glut.glutSwapBuffers ();
;;

let on_mouse_motion ~x ~y = Event.mouse_motion_handler ~x ~y

let on_mouse  ~button ~state ~x ~y =
  flush stdout;
  mouse_state := (button, state);
  Event.mouse_handler ~button ~state ~x ~y


let init build reshape =
  build ();
  Glut.glutDisplayFunc display;
  Glut.glutIdleFunc Glut.glutPostRedisplay;
  Glut.glutMotionFunc on_mouse_motion;
  Glut.glutMouseFunc on_mouse;
  Glut.glutReshapeFunc reshape;
  Glut.glutMainLoop ()
;;

