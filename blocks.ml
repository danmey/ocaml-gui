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
    print_endline "";
    print_endline (String.concat "\n" (List.map Rect.string_of_rect sorted));
    let rec loop = function
      | x :: y :: xs when 
          (x.Rect.x >= y.Rect.x && x.Rect.x <= y.Rect.x + y.Rect.w
           || x.Rect.x+x.Rect.w >= y.Rect.x && x.Rect.x+x.Rect.w <= y.Rect.x + y.Rect.w) && x.Rect.y + 20 = y.Rect.y -> (TopBottom, (x,y)) :: loop (y::xs)
      | x :: y :: xs when x.Rect.x + x.Rect.w = y.Rect.x && x.Rect.y = y.Rect.y -> (LeftRight, (x,y)) :: loop (y::xs)
      | x :: xs -> loop xs
      | _ -> []
    in
    let s = loop sorted in
    print_endline (String.concat "\n" 
                     (List.map 
                        (function 
                          | (TopBottom, (x,y)) -> 
                          Printf.sprintf "TopBottom (%s, %s)" (Rect.string_of_rect x) (Rect.string_of_rect y)
                         | (LeftRight, (x,y)) -> 
                          Printf.sprintf "LeftRight (%s, %s)" (Rect.string_of_rect x) (Rect.string_of_rect y)
                        ) s))
    
      
  (*   let loop = function *)
  (*     | x :: xs ->  *)
  (*       (let loop2 = function *)
  (*         | y :: ys ->  *)

end and block = object ( self : 'self )
  inherit draggable
  (* val mutable canvas : block composite option = None *)
  val mutable canvas : block_canvas option = None
  val mutable accum_pos_y = 0
  method set_canvas c = canvas <- Some c
  method drag pos dpos =
    window.pos.Rect.x <- fst pos / 20 * 20;
    window.pos.Rect.y <- snd pos / 20 * 20;
    BatOption.may (fun x -> x#layout) canvas
end
