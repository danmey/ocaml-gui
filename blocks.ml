open Widget
open Draw
open Window

type 'a block_tree = Block of 'a * 'a list
type stickiness = TopBottom | LeftRight

let block_cp
class block_canvas = object ( self : 'self )
  inherit [ block ] composite as super
  method add block = 
    super#add block;
    block#set_canvas (self :> block_canvas)
  (* method layout = *)
  (*   let rects = List.map (fun w -> w#window.pos) widgets in *)
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
    window.pos.Rect.y <- snd pos / 20 * 20
end
