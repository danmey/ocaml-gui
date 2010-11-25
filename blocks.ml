open Widget
open Draw
open Window

type 'a block_tree = Block of 'a * 'a list
type stickiness = TopBottom | LeftRight

let expand_rect_left dx rect constr =
  let real_dx = ( dx / constr) * constr in
  { rect with 
    Rect.x = rect.Rect.x + real_dx; 
    Rect.w = rect.Rect.w - real_dx }

let block_cmp l r =
  let y = r.Rect.y - l.Rect.y in
  if y <> 0 then y
  else
    r.Rect.x - l.Rect.x

(* class block_canvas = object ( self : 'self ) *)
(*   inherit [ block ] composite as composite *)
(*   inherit graphical as super *)
(*   method add block =  *)
(*     composite#add block; *)
(*     block#set_canvas (self :> block_canvas) *)

type tree = Tree of string * tree list

type 'a property_value = { min : 'a; max : 'a; default : 'a; step : float }
type property_type = 
  | Float of float property_value
  | Int of int property_value

type property = string * property_type

(* class property_slider = object (self : 'self) *)
(*   inherit slider *)
    
(*   (\* method slide_end value =  *\) *)
(*   (\*   Custom ( *\) *)
(* end     *)
class properties props = object (self : 'self)
  inherit frame (fixed_vertical_layout 5 25)
    
  initializer 
    self # set_properties props;
  method delete_properties =
    List.iter (fun (_,widget) -> Window.remove self # window widget # window) widgets;
    widgets <- []

  method set_properties = List.iter
    (function
      | (name, Float { min; max; default; step }) -> 
        self # add ((new slider name min max step) :> draggable)
      | (name, Int { min; max; default; step }) -> 
        self # add ((new int_slider name min max step) :> draggable))
    
  (* method property_changed widget value = *)
  (*   () *)
end
    
(* end and block name = object ( self : 'self ) *)
class block name (properties : properties) = object ( self : 'self ) 
  inherit canvas as canvas
  inherit draggable as super
  val left_border = new draggable_constrained (HorizontalWith 10)
  val right_border = new draggable_constrained (HorizontalWith 10)
  val name = name
  val properties = properties
  initializer
    canvas#add left_border;
    canvas#add right_border
    
  method drag _ (x,y) (dx,dy) =
    let grid x = (x + 5) / 10 * 10 in
    window.pos.Rect.x <- grid x;
    window.pos.Rect.y <- grid y

  method value = ""
  method name = name
  method paint state =
    let caption = Printf.sprintf "%s: %s" name self#value in
    caption_painter caption 0 state

  method invalidate rect =
    super#invalidate rect;
    left_border#invalidate (Rect.rect (0,0) (10, rect.Rect.h));
    right_border#invalidate (Rect.rect (rect.Rect.w-10,0) (10, rect.Rect.h))

  method dragged widget (dx, dy) =
    let grid x = (x + 5) / 10 * 10 in
    let rect = self#window.pos in
    if widget == left_border then
      self # invalidate (expand_rect_left dx rect 10)
    else if widget == right_border then
      self # invalidate
        (Rect.rect
           (rect.Rect.x, rect.Rect.y)
           (grid (rect.Rect.w + dx), rect.Rect.h))

  method mouse_down b p =
    Event.run_events self # window
      (Event.Custom ("block_clicked",
                     p,
                     ""));
    super # mouse_down b p

      
  method get_properties = properties
        
end




open BatFloat
class texture_preview trigger = object ( self : 'self )
  inherit graphics as super
  val mutable texid = None
  initializer
    window.Window.painter <- self#draw;
    (* texid <- Some (Texture.Tga.gl_maketex (Perlin.Op.array_of_texture (Perlin.Op.normalize 256 (Perlin.Op.clouds 3 0.4)))) *)

    method draw rect =
      BatOption.may (fun texid ->
        GlTex.bind_texture ~target:`texture_2d texid;
        GlTex.parameter ~target:`texture_2d (`mag_filter `nearest);
        GlTex.parameter ~target:`texture_2d (`min_filter `nearest); 
        Gl.enable `texture_2d;
        let x, y, w, h = Window.center_rect rect in
        GlDraw.begins `quads;
        GlTex.coord2 (0.0, 0.0);
        GlDraw.vertex ~x ~y ();
        GlTex.coord2 (1.0, 0.0);
        GlDraw.vertex ~x:(x + w) ~y ();
        GlTex.coord2 (1.0, 1.0);
        GlDraw.vertex ~x:(x + w) ~y:(y + h) ();
        GlTex.coord2 (0.0, 1.0);
        GlDraw.vertex ~x ~y:(y + h) ();
        GlDraw.ends ();
        Gl.disable `texture_2d;
      ) texid; ()

    method event wind ev = 
      match ev with
      (* | Event.Parameters ["octaves",Event.Float oct] when wind == trigger#window -> *)
      (*   texid <- Some (Texture.Tga.gl_maketex (Perlin.Op.array_of_texture (Perlin.Op.normalize 256 (Perlin.Op.clouds 3 oct)))); true *)
      | ev -> super # event wind ev
    
end

let property_pane = new frame fill_layout

let properties = 
  ["perlin", 
   ["persistence", Float { min = 0.; max = 3.; default = 0.25; step = 0.01 };
    "octaves", Int { min=1; max=8; default=1; step = 0.2 }; ];

   "add",  
   [];

   "light", 
   ["lx", Float { min = 0.; max = 1.; default = 0.; step = 0.01 };
    "ly", Float { min = 0.; max = 1.; default = 1.; step = 0.01 };
    "ldx", Float { min = 0.; max = 1.; default = 1.; step = 1./.256. };
    "ldy", Float { min = 0.; max = 1.; default = 1.; step = 1./.256. };]]
    

class block_canvas = object ( self : 'self)
  inherit canvas as canvas
  inherit fixed as super
  val mutable last_mouse_pos = (0,0)
  method mouse_down button pos =
    match button with
      | Event.Right ->
        last_mouse_pos <- pos;
        let names, _ = List.split properties in
        let m = new menu pos names in
        self # add (m :> draggable);
        true
      | Event.Middle ->
        self # layout; true
      | _ -> super # mouse_down button pos

  method layout =
    (* let rects = List.map (fun w -> w # window.pos) widgets in *)
    (* let widget_rects = List.combine rects widgets in *)
    (* let sorted = BatList.sort ~cmp:block_cmp rects in *)
    (* let rec stack_loop acc cur_y = function *)
    (*   | x :: xs when x.Rect.y = cur_y -> *)
    (*     (match acc with *)
    (*       | [] -> stack_loop ([x] :: acc) cur_y xs *)
    (*       | a :: b -> stack_loop ((a @ [x]) :: b) cur_y xs) *)
    (*   | x :: xs -> stack_loop ([x] :: acc) x.Rect.y xs *)
    (*   | [] -> acc in *)
    
    (* let stack = stack_loop [[]] ((List.hd sorted).Rect.y) sorted in *)
    
    (* let rec loop = function *)
    (*   | x :: xs -> String.concat "\t" (List.map (fun rect -> (List.assoc rect widget_rects)#name) x) :: (loop xs) *)
    (*   | [] -> [] *)
    (* in *)
    ()
    (* let rec tree_loop parent_name = function *)
    (*   | x :: xs -> Children parent_name (List.map (fun rect -> (List.assoc rect widget_rects) # name) x) :: (tree_loop xs) *)
    (*   | [] -> [] *)
    (* in *)
    
    method event wind = 
      function
      | Event.Custom ("menu_item", _, what) -> 
        let properties = List.assq what properties in
        let properties_pane = new properties properties in
        property_pane # remove_all;
        property_pane # add (properties_pane :> draggable);
        property_pane # revalidate;
        let b = new block what properties_pane in
        self#add (b :> draggable);
        b#invalidate (Rect.rect last_mouse_pos (80, 20));
        true
      | Event.Custom ("block_clicked", _, _) ->
        (try
        (let widget = self # find wind in
        let block : block = Obj.magic widget in
        let properties_pane = block # get_properties in
        property_pane # remove_all;
        property_pane # add (properties_pane :> draggable);
        property_pane # revalidate)
        with Not_found -> print_endline "ups";);
        true
      | ev -> super # event wind ev
end
