open Widget
open Draw
open Window

type 'a block_tree = Block of 'a * 'a list
type stickiness = TopBottom | LeftRight

let block_cmp l r =
  let y = l.Rect.y - r.Rect.y in
  if y <> 0 then y
  else
    l.Rect.x - r.Rect.x

class block_canvas = object ( self : 'self )
  inherit [ block ] composite as super
  method add block = 
    super#add block;
    block#set_canvas (self :> block_canvas)
  method layout =
    let rects = List.map (fun w -> w # window.pos) widgets in
    let sorted = BatList.sort ~cmp:block_cmp rects in
    let rec stack_loop acc cur_y = function
      | x :: xs when x.Rect.y = cur_y -> 
        (match acc with 
          | [] -> stack_loop ([x] :: acc) cur_y xs
          | a :: b -> stack_loop ((a @ [x]) :: acc) cur_y xs)
      | x :: xs -> stack_loop ([x] :: acc) x.Rect.y xs
      | [] -> acc in
    
    let stack = stack_loop [[]] ((List.hd sorted).Rect.y) sorted in
    
    let rec loop = function
      | x :: xs -> String.concat "tab: \t" (List.map Rect.string_of_rect x) :: (loop xs)
      | [] -> []
    in
    print_endline (String.concat "\n" (loop stack));
    print_endline "";
    (* print_endline (String.concat "\n" (List.map Rect.string_of_rect sorted)); *)
    let rec loop = function
      | x :: y :: xs when 
          (x.Rect.x >= y.Rect.x && x.Rect.x <= y.Rect.x + y.Rect.w
           || x.Rect.x+x.Rect.w >= y.Rect.x && x.Rect.x+x.Rect.w <= y.Rect.x + y.Rect.w) && x.Rect.y + 20 = y.Rect.y -> (TopBottom, (x,y)) :: loop (y::xs)
      | x :: y :: xs when x.Rect.x + x.Rect.w = y.Rect.x && x.Rect.y = y.Rect.y -> (LeftRight, (x,y)) :: loop (y::xs)
      | x :: xs -> loop xs
      | _ -> []
    in
    let s = loop sorted in
    (* print_endline (String.concat "\n"  *)
    (*                  (List.map  *)
    (*                     (function  *)
    (*                       | (TopBottom, (x,y)) ->  *)
    (*                       Printf.sprintf "TopBottom (%s, %s)" (Rect.string_of_rect x) (Rect.string_of_rect y) *)
    (*                      | (LeftRight, (x,y)) ->  *)
    (*                       Printf.sprintf "LeftRight (%s, %s)" (Rect.string_of_rect x) (Rect.string_of_rect y) *)
    (*                     ) s)); *)
    ()
    
      
  (*   let loop = function *)
  (*     | x :: xs ->  *)
  (*       (let loop2 = function *)
  (*         | y :: ys ->  *)

end and block = object ( self : 'self )
  inherit [ draggable ] composite as composite
  inherit draggable as super
  val left_border = new draggable
  val right_border = new draggable
  initializer
    composite#add left_border;
    composite#add right_border;
    
  (* val mutable canvas : block composite option = None *)
  val mutable canvas : block_canvas option = None
  val mutable accum_pos_y = 0
  method set_canvas c = canvas <- Some c
  method drag pos dpos =
    window.pos.Rect.x <- ( fst pos + 5 ) / 20 * 20;
    window.pos.Rect.y <- ( snd pos + 5 ) / 20 * 20;
    BatOption.may (fun x -> x#layout) canvas
  method invalidate rect =
    super#invalidate rect;
    let left_border_rect = Rect.rect (0,0) (10, rect.Rect.h) in
    let right_border_rect = Rect.rect (rect.Rect.w-10, 0) (10, rect.Rect.h) in
    left_border#invalidate left_border_rect;
    right_border#invalidate right_border_rect;
    composite#invalidate rect


  (* method event (window : Window.window) (ev : Event.event) = *)
  (*   match ev with *)
  (*     | Event.Drag (dx, dy) when split_widget#window == window -> *)
        
end
