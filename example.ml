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


open Blocks

let texture_generator_view () =
  let kont = ref None in
  let open Texgen in
      let control_pane = new texture_preview(* frame (Layout.fixed_vertical 5 25) *) in
      let generate graphical_pane =
        let op_of_properties edit_pane =
          let ws = edit_pane # widgets in
          let params =
            List.map (fun (_,w) ->
              w # key, w # value) ws
          in
          params
        in
        let tree = graphical_pane#layout in
        let rec loop = function
          | Tree ((prop, block), lst) ->
            let params = op_of_properties prop in
            let propf name = let Event.Float v = List.assoc name params in v in
            let propi name = let Event.Int v = List.assoc name params in v in
            (match block # key with
              | "perlin" ->
                Clouds { octaves = propi "octaves";
                         persistence = propf "persistence"}
              | "glow" -> Glow { x0 = propf "x0";
                                 y0 = propf "y0";
                                 atten = propf "atten";
                                 xr = propf "xr";
                                 yr = propf "yr";}
              | "phi" -> Phi ({ scale = propf "scale";
                                base = propf "base"; }, loop (List.hd lst))
              | "flat" -> Flat { fx = propf "fx";
                                 fy = propf "fy";
                                 fw = propf "fw";
                                 fh = propf "fh";
                                 fg = propf "fg";
                                 bg = propf "bg"; }
              | "add" ->
                    Add (List.map loop lst)
              | "light" ->
                Light ({ lx = propf "lx";
                         ly = propf "ly";
                         ldx = propf "ldx";
                         ldy = propf "ldy"; }, loop (List.hd lst)))
        in
        let op2 = loop tree in
        Texgen.operator := Some op2;
        Texgen.reset ();
        ()
      in
      let prev_tid = ref None in
      let draw w rect =
        let tid = Texture.Tga.gl_maketex !prev_tid Texgen.ar in
        prev_tid := Some tid;
        control_pane # set_image tid;
        Texgen.update_texture ();
        w # draw;
      in
      let graphical_pane = block_canvas ~draw ~generate () in
      let (_, edit_properties) :: _ = properties_definition in
      let edit_pane = properties edit_properties in
      let split_control = splitter ~first:control_pane ~second:property_pane Widget.Vertical in
      property_pane # add (edit_pane  :> fixed);
      let main_view = splitter ~first:split_control ~second:graphical_pane Widget.Horizontal in
      let main_frame = new frame (Layout.vertical_fixed ~sizes:[25]) in
      main_frame#add ((menu_bar 
                         [Entry ("Save", 
                                 (fun () -> graphical_pane # write "canvas.txt"));
                          Entry ("Open",
                                 (fun () -> graphical_pane # read "canvas.txt"));
                         ]) :> draggable);
      main_frame#add (main_view :> draggable);
      main_frame
      
        
  


      

(* let a () = *)
(*   Gui.init *)
(*     (fun () -> *)
(*       (\* Png_loader.load_img (Filename "ala"); *\) *)
(*       let g = new desktop in *)
(*       let generate_button = new generate_button in *)
(*       let control_pane = ((new texture_preview generate_button) :> draggable)(\* frame (Layout.fixed_vertical 5 25) *\) in *)
(*       let open Blocks in *)
(*       (\* let sl () = new slider (-1.0) 1.0 0.01 in *\) *)
(*       (\* let sx = (new int_slider 0 5 1.0) in *\) *)
(*       (\* let sy = sl () in *\) *)
(*       (\* let sz = sl () in *\) *)
(*       (\* let sw = sl () in *\) *)
(*       (\* let graphical_pane = new sphere_view sx sy sz sw in *\) *)
(*       (\* let control_pane = new frame (Layout.fixed_vertical 5 25) in *\) *)
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
  let open BatInt in
  let g = new desktop in
  let tgv = ((texture_generator_view()) :> graphical) in
  Gui.init
    (fun () ->
      (* Png_loader.load_img (Filename "ala"); *)
       g#add tgv;
      ())
    (fun ~width ~height ->
       Window.shelf (Rect.rect (0,0) (width,height));
       tgv#invalidate (Rect.rect (10,10) (width-20,height-20));
       g#invalidate (Rect.rect (0,0) (width,height)))
;;

b()
;;
