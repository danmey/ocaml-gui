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

let project_name = ref "canvas.txt"

open Blocks

let texture_generator_view() =
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
        let start_rect = 
            BatOption.map (fun (_,b) -> (b # window.pos)) 
              (BatList.Exceptionless.find (fun (_,b) -> b # get_state == Selected) graphical_pane#widgets) in
        let rec find_all_macros = function
          | (_,w) :: xs -> 
            if Pervasives.(w # key = "MACRO") then
              let rect = w # window.pos in
              let tree = graphical_pane#layout (Some rect) in
              (match tree with
                | Tree ((prop, block), lst) ->
                  let params = op_of_properties prop in
                  let i name = let Event.Int v = List.assoc name params in v in
                  i "id", List.hd lst) :: find_all_macros xs
                else find_all_macros xs
          | [] -> [] in
        let macros = find_all_macros graphical_pane#widgets in
        let tree = graphical_pane#layout start_rect in
        let rec loop = function
          | Tree ((prop, block), lst) ->
            let params = op_of_properties prop in
            let f name = let Event.Float v = List.assoc name params in v in
            let i name = let Event.Int v = List.assoc name params in v in
            (match block # key with
              | "perlin" ->
                Clouds { octaves = i "octaves";
                         persistence = f "persistence";
                         seed = i "seed" }
              | "glow" -> Glow { x0 = f "x0";
                                 y0 = f "y0";
                                 atten = f "atten";
                                 xr = f "xr";
                                 yr = f "yr";}
              | "phi" -> Phi ({ scale = f "scale";
                                base = f "base"; }, loop (List.hd lst))
              | "flat" -> Flat { fx = f "fx";
                                 fy = f "fy";
                                 fw = f "fw";
                                 fh = f "fh";
                                 fg = f "fg";
                                 bg = f "bg"; }
              | "add" ->
                    Add (List.map loop lst)
              | "light" ->
                Light ({ lx = f "lx";
                         ly = f "ly";
                         ldx = f "ldx";
                         ldy = f "ldy"; }, loop (List.hd lst))
              | "distort" ->
                let op1::op2::op3::_ = lst in
                Distort ({ dtype=Radial; 
                           dscale = f "dscale"}, 
                         loop op1, loop op2, loop op3)
              | "rgb" -> Printf.printf "lst::%d\n" (List.length lst); let r::g::b::_ = lst in
                         Rgb ({ rp = f "rp";
                                gp = f "gp";
                                bp = f "bp";}, loop r, loop g, loop b)
              | "hsv" -> let r::g::b::_ = lst in
                         Hsv ({ hp = f "hp";
                                sp = f "sp";
                                vp = f "vp";}, loop r, loop g, loop b)
              | "phi3" -> Phi3 ({ scale1 = f "scale1";
                                 base1 = f "base1";
                                 scale2 = f "scale2";
                                 base2 = f "base2"; 
                                 scale3 = f "scale3";
                                 base3 = f "base3"; 
                               }, 
                               loop (List.hd lst))
              | "MACRO" -> loop (List.hd lst)
              | "CALL" -> loop (List.assoc (i "id") macros)
              | "modulate" ->
                Modulate (List.map loop lst))
              

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
                                 (fun () -> graphical_pane # write !project_name;
                                  save_texture (!project_name ^ ".tga");
                          ));
                          Entry ("Open",
                                 (fun () -> graphical_pane # read !project_name));
                         ]) :> draggable);
      main_frame#add (main_view :> draggable);
      graphical_pane # read !project_name;
      main_frame


let commandline_spec =
  let open Arg in
  let open BatArg in
  [ command ~doc:"Filename of a project to work with. Default project is \"canvas.txt\"."
    "-project"
    (Set_string project_name)]


let b () =
  BatArg.handle commandline_spec;
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
