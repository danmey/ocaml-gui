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
  let y = l.Rect.y - r.Rect.y in
  if y <> 0 then y
  else
    l.Rect.x - r.Rect.x

(* class block_canvas = object ( self : 'self ) *)
(*   inherit [ block ] composite as composite *)
(*   inherit graphical as super *)
(*   method add block =  *)
(*     composite#add block; *)
(*     block#set_canvas (self :> block_canvas) *)

(*   method layout = *)
(*     let rects = List.map (fun w -> w # window.pos) widgets in *)
(*     let sorted = BatList.sort ~cmp:block_cmp rects in *)
(*     let rec stack_loop acc cur_y = function *)
(*       | x :: xs when x.Rect.y = cur_y ->  *)
(*         (match acc with  *)
(*           | [] -> stack_loop ([x] :: acc) cur_y xs *)
(*           | a :: b -> stack_loop ((a @ [x]) :: acc) cur_y xs) *)
(*       | x :: xs -> stack_loop ([x] :: acc) x.Rect.y xs *)
(*       | [] -> acc in *)
    
(*     let stack = stack_loop [[]] ((List.hd sorted).Rect.y) sorted in *)
    
(*     let rec loop = function *)
(*       | x :: xs -> String.concat "\t" (List.map Rect.string_of_rect x) :: (loop xs) *)
(*       | [] -> [] *)
(*     in *)
(*     print_endline (String.concat "\n" (loop stack)); *)
(*     print_endline ""; *)
(*     (\* print_endline (String.concat "\n" (List.map Rect.string_of_rect sorted)); *\) *)
(*     let rec loop = function *)
(*       | x :: y :: xs when  *)
(*           (x.Rect.x >= y.Rect.x && x.Rect.x <= y.Rect.x + y.Rect.w *)
(*            || x.Rect.x+x.Rect.w >= y.Rect.x && x.Rect.x+x.Rect.w <= y.Rect.x + y.Rect.w) && x.Rect.y + 20 = y.Rect.y -> (TopBottom, (x,y)) :: loop (y::xs) *)
(*       | x :: y :: xs when x.Rect.x + x.Rect.w = y.Rect.x && x.Rect.y = y.Rect.y -> (LeftRight, (x,y)) :: loop (y::xs) *)
(*       | x :: xs -> loop xs *)
(*       | _ -> [] *)
(*     in *)
(*     (\* let s = loop sorted in *\) *)
(*     (\* print_endline (String.concat "\n" *\) *)
(*     (\*                  (List.map *\) *)
(*     (\*                     (function *\) *)
(*     (\*                       | (TopBottom, (x,y)) -> *\) *)
(*     (\*                       Printf.sprintf "TopBottom (%s, %s)" (Rect.string_of_rect x) (Rect.string_of_rect y) *\) *)
(*     (\*                      | (LeftRight, (x,y)) -> *\) *)
(*     (\*                       Printf.sprintf "LeftRight (%s, %s)" (Rect.string_of_rect x) (Rect.string_of_rect y) *\) *)
(*     (\*                     ) s)); *\) *)
(*     () *)
    
(* end and block name = object ( self : 'self ) *)
class block name = object ( self : 'self ) 
  inherit canvas as canvas
  inherit draggable as super
  val left_border = new draggable_constrained (HorizontalWith 10)
  val right_border = new draggable_constrained (HorizontalWith 10)

  initializer
    canvas#add left_border;
    canvas#add right_border
    
  method drag _ (x,y) (dx,dy) =
    let grid x = (x + 5) / 10 * 10 in
    window.pos.Rect.x <- grid x;
    window.pos.Rect.y <- grid y

  method value = ""

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
end

class block_canvas = object ( self : 'self)
  inherit canvas
  inherit fixed
    
end
