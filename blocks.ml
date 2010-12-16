open Widget
open Draw
open Window
open GL
open Glu
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


type 'a property_value = { min : 'a; max : 'a; default : 'a; step : float }
type property_type = 
  | Float of float property_value
  | Int of int property_value

type property = string * property_type

class properties props change = object (self : 'self)
  inherit frame (Layout.vertical)
    
  initializer
    List.iter
      (function
      | (name, Float { min; max; default; step }) -> 
        self # add (float_slider ~value:default ~min ~max ~step ~change name)
      | (name, Int { min; max; default; step }) -> 
        self # add (int_slider ~value:default ~min ~max ~step ~change name))
      props

  (* method delete_properties = *)
  (*   List.iter (fun (_,widget) -> Window.remove self # window widget # window) widgets; *)
  (*   widgets <- [] *)
  method get key =
    let _, w = List.find (fun (_,w) -> w # key = key) self # widgets in
    w
end

let properties
    ?change:(change=(fun _ -> ()))
    definition =
  new properties definition change
    
(* end and block name = object ( self : 'self ) *)
class block name click = object ( self : 'self ) 
  inherit canvas as canvas
  inherit draggable as super
  val left_border = new draggable_constrained (HorizontalWith 10)
  val right_border = new draggable_constrained (HorizontalWith 10)
  val name = name
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
    if not (click b self) then
      super # mouse_down b p
    else
      true
      
  method key = name
  method focus is = focus <- is

  method paint state = caption_painter2 self # name state
        
end

let block ?click:(click = fun _ _ -> false) name =
  new block name click
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
     "ldy", Float { min = 0.; max = 1.; default = 1.; step = 1./.256. };
   ];
   "glow", [
     "x0", Float { min = -1.; max = 1.; default = 0.5; step = 0.01 };
     "y0", Float { min = -1.; max = 1.; default = 0.5; step = 0.01 };
     "atten", Float { min = -100.; max = 100.; default = 1.; step = 0.1 };
     "xr", Float { min = 0.; max = 5.; default = 0.5; step = 0.01 };
     "yr", Float { min = 0.; max = 5.; default = 0.5; step = 0.01 };
   ];
  "phi",[
    "scale", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };
     "base", Float { min = -5.; max = 5.; default = 0.; step = 0.01 };];
    "add", [];
    "flat",[
      "fx", Float { min = 0.; max = 1.; default = 0.25; step = 0.01 };
      "fy", Float { min = 0.; max = 1.; default = 0.25; step = 0.01 };
      "fw", Float { min = 0.; max = 1.; default = 0.5; step = 0.01 };
      "fh", Float { min = 0.; max = 1.; default = 0.5; step = 0.01 };
      "fg", Float { min = 0.; max = 1.; default = 0.25; step = 0.01 };
      "bg", Float { min = 0.; max = 1.; default = 0.75; step = 0.01 };];
    "distort",[
      "dscale", Float { min = 0.1; max = 512.; default = 256.; step = 1. };
    ];
      "rgb", [
        "rp", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };
        "gp", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };
        "bp", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };];
      "hsv", [
        "hp", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };
        "sp", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };
        "vp", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };];
      "phi3", [
        "scale1", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };
        "base1", Float { min = -5.; max = 5.; default = 0.; step = 0.01 };
        "scale2", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };
        "base2", Float { min = -5.; max = 5.; default = 0.; step = 0.01 };
        "scale3", Float { min = 0.; max = 10.; default = 1.; step = 0.01 };
        "base3", Float { min = -5.; max = 5.; default = 0.; step = 0.01 };
      ];
  ]
    
type 'a block_tree = Tree of 'a * 'a block_tree list
class block_canvas generate draw = object ( self : 'self)
  inherit canvas as canvas
  inherit fixed as super
  val mutable last_mouse_pos = (0,0)
  val mutable focused_block = None
  val mutable block_properties = []
  initializer
    window.Window.painter <- draw self;

  method draw = ()
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
    let block_rects = List.map (fun (_,w) -> w # window.pos,w) widgets in
    let widget_properties = List.combine rects 
      (List.map (fun (w,_) ->
        List.assq w block_properties) widgets) in
    let sorted = BatList.sort ~cmp:block_cmp rects in
    let rec stack_loop acc cur_y = function
      | x :: xs when x.Rect.y = cur_y ->
        (match acc with
          | [] -> stack_loop ([x] :: acc) cur_y xs
          | a :: b -> stack_loop (([x] @ a) :: b) cur_y xs)
      | x :: xs -> stack_loop ([x] :: acc) x.Rect.y xs
      | [] -> List.rev acc in
      let open Pervasives in
        let rec loop acc = function
          | ({ Rect.x=x1; Rect.w=w1; } as r1) :: xs, ({ Rect.x=x2; Rect.w=w2; } as r2) :: ys -> 
            if x2 >= x1 && x2 < x1 + w1 (*&& x2 + w2 <= x1 + w1*) then
              match acc with
                | [] ->  loop [r1, [r2]] ((r1 :: xs), ys)
                | (r, a) :: rest when r = r1 -> loop ((r, a @ [r2]) :: rest) ((r1 :: xs), ys)
                | rest -> loop ([r1, [r2]] @ rest ) ((r1 :: xs), ys)
            else
              loop acc (xs, (r2::ys))
          | [],_ -> acc
          | _,[] -> acc
        in
        let rec flat_loop = function
        | row1 :: row2 :: xs  -> loop [] (row1, row2) @ flat_loop (row2::xs)
        | row :: [] -> loop [] (row, [])
        | [] -> [] in
        let rec tree_loop nodes =
          function
            | (a, xs) -> Tree (a, List.map (fun el -> try tree_loop nodes (el, (List.assoc el nodes)) with Not_found -> Tree (el, []) ) xs)
        in
    let rec loop2 = function
      | lst::xs ->
        let props  = List.map
          (fun a ->
            List.assoc a block_rects,
            List.assoc a widget_properties) lst in
          props :: loop2 xs
      (* | a :: [] -> print_endline ("top: " ^ (snd (List.assq a widget_rects))#key) *)
      | [] -> []
    in
    let lst = stack_loop [[]] (List.hd sorted).Rect.y sorted in
    print_endline "stack_loop-----";
    List.iter (fun (lst) -> Printf.printf "(%s)\n" (String.concat " | " (List.map Rect.string_of_rect lst))) lst;
    
    match lst with
      | (r::_) :: [] -> Tree ((List.assoc r widget_properties, (List.assoc r block_rects)), [])
      | lst -> (let lst = flat_loop lst in
    print_endline "-----";
    List.iter (fun (a, lst) -> Printf.printf "(%s:: %s)\n" (Rect.string_of_rect a) (String.concat " | " (List.map Rect.string_of_rect lst))) lst;
    print_endline "-----";
    print_endline "-----";
    print_endline "-----";
    print_endline "-----";
    (match lst with
      | hd :: tl -> 
        begin
          let tree = tree_loop lst hd in
          let rec print_tree = function
            | Tree (r,lst) -> Printf.sprintf "(%s %s)"
              ((List.assoc r block_rects)#key)
              (String.concat " " (List.map print_tree lst))
                    in
          print_endline (print_tree tree);
          tree;
          flush stdout;
          let rec block_tree = function
            | Tree (r,lst) ->
              Tree ((List.assoc r widget_properties, (List.assoc r block_rects)), List.map block_tree lst)
          in
          block_tree tree
        end))
    
      

  method focus_block block =
    BatOption.may (fun block -> block # focus false) focused_block;
    block # focus true;
    focused_block <- Some block

  method create_block item pos =
    let definition = List.assoc item properties_definition in
    let properties = properties definition ~change:(fun _ -> generate self) in
    let b = block ~click:(fun button block -> self # click_block button block) item in
    property_pane # remove_all;
    block_properties <- (b#window, properties)::block_properties;
    self#add (b :> draggable);
    b#invalidate pos;
    self # focus_block b;
    property_pane # revalidate;
    b, properties
    
  method select_menu _ item =
    self # create_block item (Rect.rect last_mouse_pos (80, 20));
    property_pane # revalidate;
    true
          
  method click_block button block =
    match button with
      | Event.Right ->
        begin
          let properties_pane = List.assq block # window block_properties in
          property_pane # remove_all;
          property_pane # add (properties_pane :> draggable);
          property_pane # revalidate;
          self # focus_block block;
          true
        end
      | Event.Middle ->
        begin
          canvas # remove (block :> draggable);
          BatList.remove_if (fun (b,_) -> b == block#window) block_properties;
          property_pane # remove_all;
          property_pane # revalidate;
          true
        end
      | _ -> false

  method write file_name =
    let properties_string property_panel =
      String.concat " " 
        (List.map 
           (fun (_,property) -> Printf.sprintf ":%s %s" (property # key) 
             (match property # value with
               | Event.Float f -> string_of_float f
               | Event.Int i -> string_of_float (float i))) property_panel#widgets) 
    in
    let str = 
      String.concat "\n"
        (List.map
           (fun (blockw, properties) ->
             let block = self # find blockw in
             let x,y,w,h = Rect.coords block # window. pos in
             Printf.sprintf "(%s (rect %d %d %d %d) %s)" block # key x y w h (properties_string properties)) block_properties)
    in
    let ch = open_out file_name in
    output_string ch str;
    close_out ch

  method read a = " " ^ a;()
  method read file_name =
    let process_line line =
      let rec update_parameters properties offset line_rest =
        try
          Str.search_forward (Str.regexp ":\\([a-z]+\\)[ \t]+\\([.0-9]+\\)[ \t]*") line_rest offset;
          let key, value = Str.matched_group 1 line_rest, Str.matched_group 2 line_rest in
          let offset' = Str.match_end () in
          print_endline key;
          print_endline value;
          let value' = float_of_string value in
          
          ((properties # get key) :> draggable) # set_value value';
          update_parameters properties offset' line_rest
        with Not_found -> ()
      in
      (* Str.search_forward  *)
      (*   (Str.regexp  *)
(*      "(\\([a-z]+\\)[ \t]+(rect \\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\))[ \t]+") line 0; *)
      Str.search_forward (Str.regexp
        "(\\([a-z]+\\)[ \t]+(rect[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]*\\(.*\\)") line 0;
      let m i = int_of_string (Str.matched_group i line) in
      let name , x, y, w, h = Str.matched_group 1 line, m 2, m 3, m 4, m 5 in
      Printf.printf "(%s %d %d %d %d)" name x y w h;
      print_endline name;
     print_endline (string_of_int x);
      print_endline (Str.matched_group 6 line);
      flush stdout;
       let block, properties = self # create_block name (Rect.rect (x,y) (w,h)) in 
       update_parameters properties 0 (Str.matched_group 6 line);
       ()
    in
      let lines = BatFile.lines_of file_name in
      BatEnum.iter process_line lines
end

let block_canvas ?draw:(draw=fun w rect -> ()) ?generate:(generate = fun _ -> ()) ()= new block_canvas generate draw
