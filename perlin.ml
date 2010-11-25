(* Must be power of two *)
let texsize = 256

let ($) f g = fun x -> f (g x)
module Layer = struct
  type 'a pixel = { x:int; y:int; color:'a}
  type 'a t = ((int*int),'a pixel) Hashtbl.t


  let add_pixel layer x y col = Hashtbl.add layer (x, y) {x=x; y=y; color=col}

  let get_pixel layer f x y = 
    try
      let pix = Hashtbl.find layer (x, y) in
	pix.color
    with Not_found -> 
      let col = f x y in
	add_pixel layer x y col;
	col
  let make f =
    let new_layer x y = Hashtbl.create (10*x*y) in
    let layer = new_layer texsize texsize in
      get_pixel layer f
end


let displace v = int_of_float (10.*. sin (3.*.v)) land (texsize-1), int_of_float (10.*. cos (3.*.v)) land (texsize-1)
(* let simple_generator () = *)
(*   let clouds = Op.normalize texsize (Op.clouds 3 0.4) in  *)
(*   let marble = Op.normalize texsize (Op.apply (fun u v -> cos (float_of_int u /. 10.0 +. 15.0 *. clouds u v))) in *)
(*   let flakes = Op.hsv (fun u v -> 0.5 +. 0.5 *. clouds u v) (fun _ _ -> 1.0) (fun _ _ -> 1.0)  in *)
(*   let terrain = Op.colorize [-0.01,(0.3,0.3,1.); 0.6,(0.1,0.8,0.1); 0.8, (0.8,0.8,0.3); 1.0, (1.,1.,1.);] marble in  *)
(*   let height u v = let h = clouds u v in h,h,h in *)
(*     Op.write "c.tga" texsize  (Op.gray3 (Op.emboss (-1.,0.) (Op.transform displace marble marble))); *)
(*     (\*    Op.write "flakes.tga" texsize flakes; *)
(* 	  Op.write "terrain.tga" texsize terrain; *)
(* 	  Op.write "height.tga" texsize height; *)
(*     *\) *)
(*     () *)
(* ;; *)

      

(* simple_generator();;  *)
(* Unix.system "feh c.tga" *)

let barsH fr = Op.apply (fun u v -> 0.5 +. 0.5 *. cos ((float u)/.fr));;
let barsV fr = Op.apply (fun u v -> 0.5 +. 0.5 *. cos ((float v)/.fr));;
(*t plasma x y = Op.(+) (barsH x) (barsV y);;*)
let plasma x y = Op.normalize texsize (Op.(+) (barsH x) (barsV y));;
let plasma5 = plasma 5. 5.;;
(*Op.write "c.tga" texsize (Op.compose3 plasma5 plasma5 plasma5);Unix.system "feh c.tga";;*)
