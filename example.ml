open Widget
open Window
open Draw
open Blocks

let id a = a
let center_rect = Rect.lift4 id
open BatFloat
(* class sphere_view xw yw zw ww = object ( self : 'self ) *)
(*   inherit graphics *)
(*     method draw rect = *)
(*     (\*   Gl.disable `scissor_test; *\) *)
(*       Gl.enable `depth_test ; *)
(*     (\*   Gl.disable `cull_face ; *\) *)
(*       let x,y,w,h = center_rect rect in *)
(*       (\* print_endline (string_of_float (float w/. float h)); *\) *)
(*       GlDraw.viewport ~x ~y ~w ~h; *)
(*       GlMat.mode `projection; *)
(*       GlMat.push (); *)
(*       GlMat.load_identity (); *)
(*       GluMat.perspective ~fovy:40. ~aspect:(float w/. float h) ~z:(0.1,100.0); *)
(*       GlMat.mode `modelview; *)
(*       (\* Gl.enable `light0; *\) *)
(*       (\* Gl.enable `lighting; *\) *)

(*     (\*   GlMat.push (); *\) *)
(*       GlMat.load_identity (); *)
(*       let x, y, z,w = xw#value, yw#value, zw#value, ww#value in *)
(*       let h = w in *)
      
(*       GluMat.look_at *)
(*         ~eye:(x, y, z) *)
(*         ~center:(0.0, 0.0, 0.0) *)
(*         ~up: (0.0, 1.0, 0.); *)
(*       (\* GlDraw.begins `quads; *\) *)
(*       (\* GlDraw.color (0.,1.,0.); *\) *)
(*       (\* GlDraw.vertex ~x ~y (); *\) *)
(*       (\* GlDraw.vertex ~x:(x + w) ~y (); *\) *)
(*       (\* GlDraw.vertex ~x:(x + w) ~y:(y + h) (); *\) *)
(*       (\* GlDraw.vertex ~x ~y:(y + h) (); *\) *)
(*       (\* GlDraw.vertex ~x:0. ~y:0. ~z:1.1; *\) *)
(*       (\* GlDraw.vertex ~x:10. ~y:0. ~z:1.1; *\) *)
(*       (\* GlDraw.vertex ~x:10. ~y:10. ~z:1.1; *\) *)
(*       (\* GlDraw.vertex ~x:0. ~y:10. ~z:1.1; *\) *)
(*       (\* GlDraw.ends (); *\) *)
(*       Glut.glutSolidCube ~size:1.0;  *)
      
(*       GlMat.mode `projection; *)
(*       GlMat.pop (); *)
(*     (\*   GlMat.mode `modelview; *\) *)
(*     (\*   GlMat.pop (); *\) *)
(*       let w,h = Display. display_size in *)
(*       GlDraw.viewport ~x:0 ~y:0 ~w ~h; *)
(*     (\*   Gl.enable `scissor_test *\) *)
(*       () *)
(* end *)

open BatInt
class generate_button = object (self : 'self)
  inherit button as super
  method mouse_down a b = 
    super # mouse_down a b;
    BatOption.may (fun parent ->
      (* print_endline "ala ma kota"; *)
      (* Event.run_events self # window  *)
      (*   (Event.Custom ("menu_item",  *)
      (*                  (0,0),  *)
      (*                "ala")); *)
      print_endline (parent#first#name);
      Event.run_events
        self # window
        (Event.Parameters ["octaves",  parent#first#value])
    ) parent;
    true

end

open Blocks

class texture_generator_view () =
    let control_pane = new texture_preview(* frame (fixed_vertical_layout 5 25) *) in
    let graphical_pane = new block_canvas in
    let (_, edit_properties) :: _ = properties in
    let edit_pane = new properties edit_properties in
    let split_control = (new splitter (control_pane :> draggable) (property_pane :> draggable) Vertical) in
    let w,h = Display.display_size in
object (self)
  inherit splitter (split_control :> draggable) (graphical_pane  :> draggable) Horizontal
  initializer
    property_pane # add (edit_pane  :> fixed);

  method event wind = function
    | Event.Custom ("slide_end", _, _) -> 
      let ws = edit_pane # widgets in
      let params = 
        List.map (fun (_,w) -> 
          print_endline w#key; w # key, w # value) ws in
      let propf name = let Event.Float v = List.assoc name params in v in
      let propi name = let Event.Int v = List.assoc name params in v in
      let open Texgen in
      let op = Texgen.Clouds { octaves = propi "octaves"; persistence = propf "persistence"} in
      Printf.printf "octaves: %d\n" (propi "octaves"); flush stdout;
      let ti = Texture.Tga.gl_maketex (texture op) in
      control_pane # set_image ti;
      true
    | _ -> false
    

end

      

(* let a () = *)
(*   Gui.init *)
(*     (fun () -> *)
(*       (\* Png_loader.load_img (Filename "ala"); *\) *)
(*       let g = new desktop in *)
(*       let generate_button = new generate_button in *)
(*       let control_pane = ((new texture_preview generate_button) :> draggable)(\* frame (fixed_vertical_layout 5 25) *\) in *)
(*       let open Blocks in *)
(*       (\* let sl () = new slider (-1.0) 1.0 0.01 in *\) *)
(*       (\* let sx = (new int_slider 0 5 1.0) in *\) *)
(*       (\* let sy = sl () in *\) *)
(*       (\* let sz = sl () in *\) *)
(*       (\* let sw = sl () in *\) *)
(*       (\* let graphical_pane = new sphere_view sx sy sz sw in *\) *)
(*       (\* let control_pane = new frame (fixed_vertical_layout 5 25) in *\) *)
(*       let graphical_pane = new block_canvas in *)
(*       (\* let b1 = (new block "x") in *\) *)
(*       (\* graphical_pane#add (b1 :> draggable); *\) *)
(*       (\* b1#invalidate (Rect.rect (300,200) (80,20)); *\) *)
(*       (\* let b2 = (new block "y") in *\) *)
(*       (\* graphical_pane#add (b2 :> draggable); *\) *)
(*       (\* b2#invalidate (Rect.rect (400,200) (80,20)); *\) *)
(*       (\* let b = (new block "+") in *\) *)
(*       (\* graphical_pane#add b; *\) *)
(*       (\* b#invalidate (Rect.rect (400,200) (80,20)); *\) *)
    
(*       (\* edit_pane#add (sx :> fixed); *\) *)
(*       (\* edit_pane#add (sy :> fixed); *\) *)
(*       (\* edit_pane#add (sz :> fixed); *\) *)
(*       (\* edit_pane#add (sw :> fixed); *\) *)
(*       let (_, edit_properties) :: _ = properties in *)
(*       let edit_pane = new properties edit_properties in *)
(*       edit_pane#add (generate_button :> fixed); *)
(*       property_pane # add (edit_pane  :> fixed); *)
(*       let split_control = (new splitter control_pane (property_pane :> draggable) Vertical) in *)
(*       let split_display = ((new splitter (split_control :> draggable) (graphical_pane  :> draggable) Horizontal) :> graphical) in *)
(*       g#add split_display; *)
(*       let w,h = Display.display_size in *)
(*       split_display#invalidate (Rect.rect (10,10) (w-20,h-20)); *)
(*       g#invalidate (Rect.rect (0,0) (w,h)); *)
(*       ()) *)

let b () =
  Gui.init
    (fun () ->
      (* Png_loader.load_img (Filename "ala"); *)
      let g = new desktop in
      let tgv = (new texture_generator_view() :> graphical) in
       g#add tgv;
      let w,h = Display.display_size in
       tgv#invalidate (Rect.rect (10,10) (w-20,h-20));
       g#invalidate (Rect.rect (0,0) (w,h)); 
      ())
;;

b()
;;
