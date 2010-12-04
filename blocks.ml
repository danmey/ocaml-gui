open Widget
open Draw
open Window
open GL
open Glu
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
class properties props change = object (self : 'self)
  inherit frame (Layout.vertical)
    
  initializer
    List.iter
      (function
      | (name, Float { min; max; default; step }) -> 
        self # add (float_slider ~min ~max ~step ~change name)
      | (name, Int { min; max; default; step }) -> 
        self # add (int_slider ~min ~max ~step ~change name))
      props

  method delete_properties =
    List.iter (fun (_,widget) -> Window.remove self # window widget # window) widgets;
    widgets <- []

end

let properties
    ?change:(change=(fun _ -> ()))
    definition =
  new properties definition change
    
(* end and block name = object ( self : 'self ) *)
class block name (properties : properties) click = object ( self : 'self ) 
  inherit canvas as canvas
  inherit draggable as super
  val left_border = new draggable_constrained (HorizontalWith 10)
  val right_border = new draggable_constrained (HorizontalWith 10)
  val name = name
  val properties = properties
  val mutable focus = false
  initializer
    canvas#add left_border;
    canvas#add right_border
    
  method drag _ (x,y) (dx,dy) =
    let grid x = (x + 5) / 10 * 10 in
    window.pos.Rect.x <- grid x;
    window.pos.Rect.y <- grid y

  method name = name
  (* method paint state = *)
  (*   let caption = Printf.sprintf "%s: %s" name self#value in *)
  (*   caption_painter caption 0 state *)

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
    click self;
    super # mouse_down b p
      
  method key = name
  method focus is = focus <- is

  method paint state = caption_painter2 self # name 0 state

  method get_properties = properties
        
end

let block ~properties ?click:(click = fun _ -> ()) name = 
  new block name properties click
open BatFloat
class texture_preview = object ( self : 'self )
  inherit graphics as super
  val mutable texid = None
  initializer
    window.Window.painter <- self#draw;

    method draw rect =
      BatOption.may (fun texid ->
        glBindTexture ~target:BindTex.GL_TEXTURE_2D ~texture:texid;
        glTexParameter ~target:TexParam.GL_TEXTURE_2D ~param:(TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_NEAREST);
        glTexParameter ~target:TexParam.GL_TEXTURE_2D ~param:(TexParam.GL_TEXTURE_MIN_FILTER Min.GL_NEAREST);
        glEnable GL_TEXTURE_2D;
        glColor3 ~r:1. ~g:1. ~b:1.;
        let x, y, w, h = Rect.coordsf rect in
        glBegin GL_QUADS;
        glTexCoord2 ~s:0.0 ~t:0.0;
        glVertex3 ~x ~y ~z:0.;
        glTexCoord2 ~s:1.0 ~t:0.0;
        glVertex3 ~x:(x + w) ~y ~z:0.;
        glTexCoord2 ~s:1.0 ~t:1.0;
        glVertex3 ~x:(x + w) ~y:(y + h) ~z:0.;
        glTexCoord2 ~s:0.0 ~t:1.0;
        glVertex3 ~x ~y:(y + h) ~z:0.;
        glEnd ();
        glDisable GL_TEXTURE_2D) texid; ()
        
    method set_image id = texid <- Some id
    method event wind ev = 
      let open Texgen in
      match ev with
      (* | Event.Parameters ["octaves",Event.Float o] when wind == trigger#window -> *)
      (*   texid <- Some (Texture.Tga.gl_maketex (Texgen.texture (Clouds { octaves = int_of_float o; persistence = 0.4; }))); *)
      (*   true *)
      | ev -> super # event wind ev
    
end

let property_pane = new frame Layout.fill

let properties_definition = 
  ["perlin",[
    "persistence", Float { min = 0.; max = 3.; default = 0.25; step = 0.01 };
    "octaves", Int { min=1; max=8; default=1; step = 0.2 }; ];
   "add",[
   ];
   "light", [
     "lx", Float { min = 0.; max = 1.; default = 0.; step = 0.01 };
    "ly", Float { min = 0.; max = 1.; default = 1.; step = 0.01 };
    "ldx", Float { min = 0.; max = 1.; default = 1.; step = 1./.256. };
    "ldy", Float { min = 0.; max = 1.; default = 1.; step = 1./.256. }; ]
  ]
    

class block_canvas = object ( self : 'self)
  inherit canvas as canvas
  inherit fixed as super
  val mutable last_mouse_pos = (0,0)
  val mutable focused_block = None
  method mouse_down button pos =
    match button with
      | Event.Right ->
        last_mouse_pos <- pos;
        let items, _ = List.split properties_definition in
        let m = menu ~pos ~items ~select:(fun pos item -> self # select_menu pos item) in
        self # add (m :> draggable);
        true
      | Event.Middle ->
        self # layout; true
      | _ -> super # mouse_down button pos

  method layout =
    let open BatInt in
    let rects = List.map (fun (_,w) -> w # window.pos) widgets in
    let widget_rects = List.combine rects widgets in
    let sorted = BatList.sort ~cmp:block_cmp rects in
    let rec stack_loop acc cur_y = function
      | x :: xs when x.Rect.y = cur_y ->
        (match acc with
          | [] -> stack_loop ([x] :: acc) cur_y xs
          | a :: b -> stack_loop ((a @ [x]) :: b) cur_y xs)
      | x :: xs -> stack_loop ([x] :: acc) x.Rect.y xs
      | [] -> acc in
    let lst = stack_loop [[]] (List.hd sorted).Rect.y sorted in
    let rec loop = function
      | (a::_)::xs -> (snd (List.assq a widget_rects)) :: loop xs
      (* | a :: [] -> print_endline ("top: " ^ (snd (List.assq a widget_rects))#key) *)
      | [] -> []
    in
    loop lst
      
  method focus_block block =
    BatOption.may (fun block -> block # focus false) focused_block;
    block # focus true;
    focused_block <- Some block

  method select_menu _ item =
        let definition = List.assq item properties_definition in
        let properties = properties definition in
        property_pane # remove_all;
        property_pane # add (properties :> draggable);
        propertyq_pane # revalidate;
        let b = block ~properties ~click:(fun block -> self # click_block block) item in
        self#add (b :> draggable);
        b#invalidate (Rect.rect last_mouse_pos (80, 20));
        self # focus_block b;
        true
          
  method click_block block =
    let properties_pane = block # get_properties in
    property_pane # remove_all;
    property_pane # add (properties_pane :> draggable);
    property_pane # revalidate;
    self # focus_block block
end
