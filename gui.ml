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

(* type motion = *)
(*   | HorizontalPinned *)
(*   | VerticalPinned *)
(* (\* *)
(*   | DragDropClass *)
(*   | DragDropParent *)
(*   | DragDropChildren *)
(* *\) *)

(* type draw_list =  *)
(*   | OwningRect of Rect.t * Draw.t list * draw_list list  *)
(*   | Simple of Draw.t list *)
(*   | Clip of Rect.t * draw_list list *)

let with_view f =
  GlMat.mode `projection;
  GlMat.push();
  GlMat.load_identity ();
  let w,h = Display. display_size in
    GlDraw.viewport ~x:0 ~y:0 ~w:w ~h:h;
    GlMat.ortho ~x:(0.,float w) ~y:(float h,0.0) ~z:(0.0,1.);

    let res = f() in
      GlMat.pop();
      res

(* let draw_lst lst =  *)
(*   List.iter (fun x ->  *)
(*       Draw.draw x) lst *)

(* let lift_option f d = function Some a -> f a | None -> d *)

(* type state = Focus | Pressed | Normal | Dragged of int * int *)

(* let color = function *)
(*   | Focus -> 1.0,0.0,0.0,1.0 *)
(*   | Pressed -> 0.0,1.0,1.0,1.0 *)
(*   | Normal -> 1.0,1.0,1.0,1.0 *)
(*   | Dragged _ ->  1.0,0.0,1.0,1.0 *)

(* let draw_by ((x,y) as p) = function *)
(*   | Draw.Line ((x1, y1, x2, y2), c) -> Draw.Line ((x1+x, y1+y, x2+x, y2+y), c) *)
(*   | Draw.Rect (rect, c) -> Draw.Rect (Rect.by rect p, c) *)
(*   | Draw.Custom  f -> Draw.Custom f *)

(* let drag_drop = [HorizontalPinned;VerticalPinned] *)

let mouse_state = ref (Glut.LEFT_BUTTON, Glut.UP)

(* type message = *)
(*   | Unfocus *)

(* let color = function *)
(*     | Focus -> 1.0,0.0,0.0,1.0 *)
(*     | Pressed -> 0.0,1.0,1.0,1.0 *)
(*     | Normal -> 1.0,1.0,1.0,1.0 *)
(*     | Dragged _ ->  1.0,0.0,1.0,1.0 *)

(* let iter f lst = ignore( List.fold_left (fun i d -> ignore(f i d); i+1) 0 lst) *)

(* type button_section = ButtonContent *)

(* type window_section = Bar | Cross | Content | Child of int *)

(* type layout = Rect.t -> Rect.t -> int -> int -> Rect.t *)

(* let button_layout parent_rect _ count nth = *)
(*   let size = Rect.size parent_rect in *)
(*     match nth with *)
(*       | Content -> Rect.border 10 (Rect.rect (0,0) size) *)
(*       | _ -> failwith "button_layout" *)

(* let horizontal_layout spacing parent_rect _ c i = *)
(*   let (h,w) = Rect.size parent_rect in *)
(*   let s = w / c in *)
(*     Rect.rect ((i * s)+spacing, 0) (s-2*spacing,h) *)

(* let window_layout height kill_size parent_rect rect count nth = *)
(*   let w,h = Rect.size parent_rect in *)
(*     match nth with *)
(*       | Bar -> Rect.rect (0,0) (w,height) *)
(*       | Content -> Rect.rect (0,height) (w,h-height) *)
(*       | Child n -> horizontal_layout 2 parent_rect rect count n *)
(*       | Cross -> Rect.rect (w-kill_size,0) (kill_size, kill_size) *)

(* let window_bar_layout kill_size parent_rect rect count nth = *)
(*   let w, h = Rect.size parent_rect in *)
(*     match nth with *)
(*       | Bar -> Rect.rect (0,0) (w-kill_size,h) *)
(*       | Cross -> Rect.rect (w-kill_size,0) (kill_size,kill_size) *)
(*       | a -> failwith "button %d" *)

(* let null_layout _ rect _ _ = rect *)

(* class graphical = object ( self : 'self ) *)
(*   val mutable rect : Rect.t = Rect.o *)
(*   val mutable parent : graphical option = None *)
(*   method draw (f : (Rect.t -> Draw.t list)) = f self#rect *)
(*   method invalidate r = rect <- r *)
(*   method rect = rect *)
(*   method hit (p : Pos.t) =  *)
(*     Rect.is_in self#rect p *)
(*   method set_parent new_parent = parent <- Some new_parent *)
(*   method parent = match parent with Some parent -> parent | None -> failwith "parent" *)
(* end *)

(* type event = *)
(*   | Click *)
(*   | DoubleClick *)
(*   | VerticalDrag *)
(*   | HorizontalDrag *)
(*   | Drag of Rect.t *)
(*   | Maximize *)

(* (\* let signals = Hashtbl.create 100 *\) *)

(* class interactive = object ( self : 'self ) *)
(*   inherit graphical as super *)

(*   val mutable state = Normal *)
(*   val mutable painter = fun state rect -> [Draw.Rect (rect, color state)] *)
(*   val mutable listeners : interactive list = [] *)
(*   method state = state *)
(*   method on_event (_:event) = () *)
(*   method on_draw = Simple (self#draw (painter self#state)) *)
(*   method on_mouse (ev:Glut.mouse_button_state_t) (p:(int*int)) =  *)
(*     if self#hit p then List.iter (fun w -> w#on_event Click) listeners *)
    
(*   method on_mouse_motion (p : (int*int)) = () *)
(*   method set_painter new_painter = painter <- new_painter *)

(* (\*  method calc_point (point : (int * int)) = point *\) *)

(*     (\* let parent_point = Rect.pos self#rect in *\) *)
(*     (\* let local_point = Pos.sub point parent_point in *\) *)
(*     (\*   local_point *\) *)

(*   method bind w = listeners <- (w::listeners) *)
(* end *)


(* let on_mouse parent widget event pos =  *)
(*   let rect = parent#rect in *)
(*   let pos2 = Rect.pos rect in *)
(*     Printf.printf "rect: %s: \n" (Rect.string_of_rect rect); *)
(*     Printf.printf "on_mouse: %s: \n" (Pos.to_string pos); *)
(*     flush stdout; *)
(*     widget#on_mouse event (Pos.sub pos pos2) *)

(* let on_mouse_motion parent widget pos =  *)
(*   let rect = parent#rect in *)
(*   let pos2 = Rect.pos rect in *)
(*     widget#on_mouse_motion (Pos.sub pos pos2) *)

(* class ['a,'b] composite layout = object ( self : 'self ) *)
(*   inherit interactive as super *)

(*   val mutable widgets : ('a * 'b) list = [] *)
(*   val mutable layout = layout *)
(*   method on_draw  =  OwningRect (self # rect,  *)
(* 				 self # draw **> painter self # state,  *)
(* 				 List.map (fun (_,w) -> w # on_draw) widgets) *)

(*   method on_mouse e p =  List.iter (fun (_,w) -> on_mouse self w e p) widgets; super#on_mouse e p *)

(*   method add k w = widgets <- (k,w)::widgets; self#invalidate self#rect; w#set_parent (self :> graphical) *)

(*   method invalidate r = *)
(*     super#invalidate r; *)
(*     let count = List.length widgets in *)
(*       List.iter (fun (i,w) ->  *)
(* 	let local_rect = layout (self # rect) (w#rect) count i in *)
(* 	  w # invalidate local_rect) widgets *)
(*   method on_mouse_motion (p : (int*int)) =  List.iter (fun (_,w) -> on_mouse_motion self w p) widgets *)
(*   method get nth = List.assoc nth widgets *)
(*   method set_layout l = layout <- l *)
(* end *)

(* class ['a,'b] draggable layout  = object ( self : 'self ) *)
(*   inherit ['a,'b] composite layout as super *)
(*   val mutable dragged = false *)
(*   val mutable drag_widget = 0 *)
(*   val mutable clicked = 0,0 *)

(*   method on_mouse e p = *)
(*     begin *)
(*       match e with *)
(* 	| Glut.DOWN -> if super#hit p then  *)
(* 	    begin *)
(* 	      dragged <- true;  *)
(* 	      clicked <- p; *)
(* 	    end *)
(* 	| Glut.UP -> dragged <- false *)
(*     end; *)
(*     super#on_mouse e p *)

(*   method on_mouse_motion p =  *)
(*     if dragged then *)
(*       begin *)
(* 	  let rel = Pos.sub p clicked in *)
(* 	    let fn widget = *)
(* 	      let rect = widget#rect in *)
(* 	      let new_rect = Rect.by rect rel in *)
(* 		new_rect *)
(* 	    in *)
(* 	List.iter (fun w -> w#on_event (Drag (fn w))) listeners; *)
(*       end; *)
(*     super#on_mouse_motion p *)
(* end *)

(* class virtual leaf = object *)
(*   inherit interactive *)
(* end *)

(* class button content = object ( self : 'self ) *)
(*   inherit [window_section, interactive] composite button_layout as super *)
(*   initializer self#add Content content; *)
(*     (self#get Content)#bind (self :> interactive) *)
      
(*   method on_event = function *)
(*     | Click -> exit 1 *)
(*     | _ -> () *)
(* end *)

(* let cross rect =  *)
(*   [Draw.Line ((rect.Rect.x, rect.Rect.y, rect.Rect.w+rect.Rect.x, rect.Rect.h+rect.Rect.y), (1.0,0.0,1.0,1.0)); *)
(*    Draw.Line ((rect.Rect.w + rect.Rect.x, rect.Rect.y, rect.Rect.x, rect.Rect.h+rect.Rect.y), (1.0,0.0,1.0,1.0))] *)

(* class kill_button_content  = object ( self : 'self ) *)
(*   inherit [int, leaf] composite null_layout as super *)
(*   method invalidate r = *)
(*     super#invalidate r; *)
(*     self#set_painter (fun state rect -> cross rect) *)
(* end *)

(* class kill_button = object ( self : 'self ) *)
(*   inherit button (new kill_button_content :> interactive) as super *)
(* end *)

(* let create_kill_button() = new kill_button *)

(* class window_bar = object ( self : 'self) *)
(*   inherit [window_section, interactive] draggable (window_bar_layout 25) *)
(*   initializer  *)
(*     self#add Bar ((new composite null_layout) :> interactive); *)
(*     self#add Cross (create_kill_button() :> interactive); *)
(* end *)

(* class window layout = object ( self : 'self) *)
(*   inherit [window_section, (window_section,interactive) composite] composite (window_layout 25 25) as super *)
(*   initializer  *)
(*   let bar = new window_bar in *)
(*     super#add Bar bar; *)
(*     super#add Content (new composite layout); *)
(*     (bar#get Cross)#bind (self :> interactive); *)
(*     bar#bind (self :> interactive) *)

(*   method add k w = (self#get Content)#add k (w :>interactive) *)
    
(*   method on_event = function *)
(*     | Drag rect -> self#invalidate rect *)
(*     | _ -> ()  *)
(* end *)

(* class ['a,'b] frame = object ( self : 'self ) *)
(*   inherit ['a,'b] composite null_layout as super *)
(*   method calc_point (point : (int * int)) = point *)
(* end *)

(* class component  = object ( self : 'self) *)
(*   inherit window null_layout *)
(*   initializer  *)
(*     self#set_layout (window_layout 10 10); *)
(*     self#invalidate (Rect.rect (0,0) (60,30)); *)
(*     (self#get Bar)#set_layout (window_bar_layout 10) *)
(* end *)

(* (\* let layout = (new frame) *\) *)

(* let inter = new interactive *)

(* let draw draw_list =  *)
(*   let sciss rect = fun () ->  *)
(*     let w, h = Display.display_size in *)
(*     let x,y = Rect.pos rect in *)
(*     let y = h/2-y-1 in *)
(*     let w,h = Rect.size rect in *)
(*     let w,h = w+1, h+1  *)
(*  in *)
(*     Gl.enable `scissor_test;  *)
(*     Printf.printf "%d %d, %d %d\n" x y w h; *)
(* (\*      GlMisc.pop_attrib(); *\) *)
      
(*     GlMisc.scissor  *)
(*       ~x:x ~y:y *)
(*       ~width:w *)
(*       ~height:h; *)

(* (\*     glmisc.push_attrib [`scissor]; *\) *)
(* (\* *)
(*     GlMisc.scissor  *)
(*       ~y:(349) ~x:50 *)
(*       ~width:401 *)
(*       ~height:401; *)
(* *\) *)
  
(* () *)
(*   in *)
(*   let rec loop parent_rect = function *)
(*     | OwningRect (rect, d, lst) ->  *)
(*       let tr = draw_by (Rect.pos parent_rect) in *)
(*       let placed = Rect.place_in rect parent_rect in *)
(* 	  (List.map tr d) @ List.flatten **> List.map (loop placed) lst *)
(*     | Simple lst -> List.map (draw_by (Rect.pos parent_rect)) lst *)
(*     | Clip (rect, lst) -> *)
(*       [Draw.Custom (sciss rect)] @ List.flatten **> List.map (loop rect) lst *)

(*   in *)
(*     loop (Rect.rect (0,0) Display.display_size) draw_list *)

open BatFloat
(* let draw_quad () = *)
(*   (\* gl.enable `color_material; *\) *)
(*   (\* gldraw.color (1.0,1.0,1.0); *\) *)
(*    GlDraw.begins `quads; *)
(*   (\* GlDraw.color (1.0,1.0,1.0); *\) *)
(*    GlDraw.vertex ~x ~y (); *)
(*   (\* GlDraw.color (1.0,1.0,1.0); *\) *)
(*    GlDraw.vertex ~x:(x + w) ~y (); *)
(*   (\* GlDraw.color (1.0,1.0,1.0); *\) *)
(*    GlDraw.vertex ~x:(x + w) ~y:(y + h) (); *)
(*   (\* GlDraw.color (1.0,1.0,1.0); *\) *)
(*    GlDraw.vertex ~x ~y:(y + h) (); *)
(*    GlDraw.ends (); *)
   
(*    () *)
external render_some : unit -> unit = "render_some_text"

let display () =
  Gl.disable `scissor_test;
  GlClear.color (0.0,0.0,1.0);
  GlClear.clear [ `color; `depth];
  GluMat.perspective ~fovy:60. ~aspect:1.6 ~z:(0.1,100.0);
  Gl.disable `depth_test ;
  Gl.disable `cull_face ;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.mode `modelview;
  GlMat.load_identity ();
  (* with_view (fun() -> draw_lst **> draw layout#on_draw); *)
  with_view (fun () -> Window.draw_desktop ());
  let w,h = Display. display_size in
 
  with_view (fun () -> render_bitmap_string 0.0 0.0 0.0 Glut.STROKE_MONO_ROMAN "ala ma kota");
  (* with_view draw_quad; *)
   Glut.swapBuffers ();
  ignore(Unix.select [] [] [] 0.001)
;;

(* let on_mouse_motion ~x ~y = () (\* layout#on_mouse_motion (x,y) *\) *)
let on_mouse_motion ~x ~y = Event.mouse_motion_handler ~x ~y

let on_mouse  ~button ~state ~x ~y =
  print_endline (Glut.string_of_button_state state);
  flush stdout;
  mouse_state := (button, state);
  Event.mouse_handler ~button ~state ~x ~y
  (* layout#on_mouse state (x,y) *)


let init build =
  ignore( Glut.init Sys.argv );
  Glut.initDisplayMode ~double_buffer:true ();
  ignore (Glut.createWindow ~title:"OpenGL Demo");
  build ();
  Window.shelf (Rect.rect (0,0) Display.display_size);

  (* Window.add Window.desktop (Rect.rect (50,50) (450,450)); *)
  (* let w = new component in *)
  (*   (\* w#invalidate (Rect.rect (0,0) (200,200)); *\) *)
  (*   layout#add 0 w; *)
  (*   layout#invalidate (Rect.rect (0,20) (400,400)); *)
  Glut.displayFunc ~cb:display;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.motionFunc ~cb:on_mouse_motion;
  Glut.mouseFunc ~cb:on_mouse;
  Glut.mainLoop ()
;;

