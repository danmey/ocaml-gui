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

module Op = struct
  let pi = 3.14159 
  let pi2 = 2. *. pi 
  let noise = Random.init 3;Layer.make (fun x y -> (Random.float 1.0) *. pi2)     
  let lift f gen = fun x y -> f (gen x y)

  let gradient_u = Layer.make (lift sin noise)
  let gradient_v = Layer.make (lift cos noise)

  let colorize color_lst layer =
    let interp a b f = (b-.a) *. f +. a in
    let interpolate (r1,g1,b1) (r2,g2,b2) f1 f2 f = 
      let f' = (f -. f1) /. (f2-.f1) in
	interp r1 r2 f',interp g1 g2 f',interp b1 b2 f' in
      Layer.make 
	(fun x y ->
	   let value = layer x y in
	   let l1,l2 = List.partition (fun (v,c) -> value > v) color_lst in
	   let (v1,c1) = List.hd (List.rev l1) in
	   let (v2,c2) = List.hd l2 in
	     interpolate c1 c2 v1 v2 value)

  let cloud w h = Layer.make
    (fun x y ->
       let gx = (x / w) * w in
       let gy = (y / h) * h in
       let grad_uv xg yg = gradient_u xg yg, gradient_v xg yg in
       let gx1,gy1 = grad_uv  gx     gy in
       let gx2,gy2 = grad_uv (gx+w)  gy in
       let gx3,gy3 = grad_uv  gx    (gy+h) in
       let gx4,gy4 = grad_uv (gx+w) (gy+h) in
       let inter v1 v2 mu =
	 let mu2 = (1.0-.cos(mu*.pi))/.2.0 in
	   v1 *. (1.0 -. mu2) +. v2 *. mu2 in
       let inter2d s t u v a b =
	 let v1 = inter s t a in
	 let v2 = inter u v a in
	   inter v1 v2 b in
       let xf = fst (modf (float_of_int x /. float_of_int w)) in
       let yf = fst (modf (float_of_int y /. float_of_int h)) in
       let dot_val gu gv x' y' = gu *. x' +. gv *. y' in
       let s,t,u,v = 
	 dot_val gx1 gy1       xf    yf, 
	 dot_val gx2 gy2 (xf-.1.0)   yf, 
	 dot_val gx3 gy3  xf        (yf-.1.0),
	 dot_val gx4 gy4 (xf-.1.0)  (yf-.1.0)
       in
       inter2d s t u v xf yf)


let clouds octaves pers =
  let rec gen_list lst a = 
    if a > 0 then (gen_list (((List.hd lst)*2)::lst)) (a-1) else lst in
  let clouds_a = gen_list [8] octaves in
  let clouds = List.map (fun a -> cloud a a) clouds_a in
    Layer.make 
      (fun x y -> fst (List.fold_left (fun (acc,p) cl -> acc +. p*.(cl x y), p*.pers) (0.0,pers) clouds))

let gray3 l = Layer.make (fun u v -> let p = l u v in (p,p,p))

let wrap c = c land (texsize - 1)
let transform f l1 l2 = Layer.make (fun x y -> let x', y' = f (l1 x y) in l2 (wrap (x'+x)) (wrap (y'+y)))
let emboss (lx, ly) l = Layer.make (fun x y -> 
				  (*    let lx, ly = float ( lx + x/2 - 128 ) /. 127.0, float ( ly + y/2 - 128 ) /. 127.0 in *)
				    let (v1,v2) = (l (wrap(x+1)) y -. l (wrap (x-1)) y), (l x (wrap (y+1)) -. l x (wrap(y-1))) in
				      let ll = 1.0 /. sqrt (v1 *. v1 +. v2 *. v2) in
				      let (v1', v2') = v1 *. ll, v2 *. ll in
				      let dot = v1' *. lx +. v2' *. ly in
					 if dot > 0. then l x y *. dot else 0.)


let normalize w layer =
  let arr = Array.make (w*w) 0.0 in
  let max = ref (-1E8) in
  let min = ref  1E8 in
    for y = 0 to (texsize-1) do
      for x = 0 to (texsize-1) do
	let c = (layer x y) in
	  if c > !max then max := c;
	  if c < !min then min := c;
	done;
      done;
    for y = 0 to w-1 do
      for x = 0 to w-1 do
	let c = layer x y in
	  Array.set arr (y*w+x) ((c -. !min) /. (!max -. !min))
	done;
      done;
    Layer.make (fun u v -> arr.(u*w+v))

let hsv l1 l2 l3 =
  Layer.make 
    (fun u v ->
       let h = l1 u v in
       let s = l2 u v in
       let v = l3 u v in
       let h' = mod_float h 1.0 in
       let hi = int_of_float (floor (360. *. h'/.60.0)) mod 6 in
       let f = h' *. 360. /. 60. -. float_of_int hi in
       let p = v*.(1.-.s) in
       let q = v*.(1.-.f*.s) in
       let t = v*.(1.-.(1.-.f)*.s) in
	 match hi with
	   | 0 -> (v,t,p)
	   | 1 -> (q,v,p)
	   | 2 -> (p,v,t)
	   | 3 -> (p,q,v)
	   | 4 -> (t,p,v)
	   | 5 -> (v,p,q)
	   | _ -> failwith "Invalid arg"
    )

let apply f = Layer.make f

let write file_name width layer =
  let ch = open_out_bin file_name in
    output_byte ch 0;
    output_byte ch 0;
    output_byte ch 2;

    output_byte ch 0; output_byte ch 0;
    output_byte ch 0; output_byte ch 0;
    output_byte ch 0;
    output_byte ch 0; output_byte ch 0;
    output_byte ch 0; output_byte ch 0;

    output_byte ch (texsize land 255); (* texsizextexsize tex*)
    output_byte ch (texsize lsr 8);

    output_byte ch (texsize land 255); 
    output_byte ch (texsize lsr 8); 

    output_byte ch 32;
    output_byte ch 0;
      for y = 0 to (texsize-1) do
	for x = 0 to (texsize-1) do
	  let r,g,b = layer x y in 
	  let r' = int_of_float (r *. 255.0) in
	  let g' = int_of_float (g *. 255.0) in
	  let b' = int_of_float (b *. 255.0) in 
	    output_byte ch b';
	    output_byte ch g';
	    output_byte ch r';
	    output_byte ch 255;
	done;
      done;
      close_out ch;
      ()

let array_of_texture layer = 
  let array = Array.make (texsize*texsize) (0.,0.,0.,1.) in
  for y = 0 to (texsize-1) do
    for x = 0 to (texsize-1) do
      (* let r,g,b = layer x y in  *)
      let v = layer x y in
      let r' = int_of_float (v *. 255.0) in
      let g' = int_of_float (v *. 255.0) in
      let b' = int_of_float (v *. 255.0) in 
      array.(y*texsize+x) <- (v, v,v,1.)
    done
  done;
  array

let (+) l1 l2 = apply (fun u v -> l1 u v +. l2 u v)
let compose3 l1 l2 l3 = (fun u v -> l1 u v, l2 u v, l3 u v)
let pixels treshold = Layer.make (fun u v -> if Random.float 1.0 < treshold then 1.0 else 0.0)
let force l = for u = 0 to (texsize-1) do for v = 0 to (texsize-1) do ignore(l u v) done done; l

end

let displace v = int_of_float (10.*. sin (3.*.v)) land (texsize-1), int_of_float (10.*. cos (3.*.v)) land (texsize-1)
let simple_generator () =
  let clouds = Op.normalize texsize (Op.clouds 3 0.4) in 
  let marble = Op.normalize texsize (Op.apply (fun u v -> cos (float_of_int u /. 10.0 +. 15.0 *. clouds u v))) in
  let flakes = Op.hsv (fun u v -> 0.5 +. 0.5 *. clouds u v) (fun _ _ -> 1.0) (fun _ _ -> 1.0)  in
  let terrain = Op.colorize [-0.01,(0.3,0.3,1.); 0.6,(0.1,0.8,0.1); 0.8, (0.8,0.8,0.3); 1.0, (1.,1.,1.);] marble in 
  let height u v = let h = clouds u v in h,h,h in
    Op.write "c.tga" texsize  (Op.gray3 (Op.emboss (-1.,0.) (Op.transform displace marble marble)));
    (*    Op.write "flakes.tga" texsize flakes;
	  Op.write "terrain.tga" texsize terrain;
	  Op.write "height.tga" texsize height;
    *)
    ()
;;

      

(* simple_generator();;  *)
(* Unix.system "feh c.tga" *)

let barsH fr = Op.apply (fun u v -> 0.5 +. 0.5 *. cos ((float u)/.fr));;
let barsV fr = Op.apply (fun u v -> 0.5 +. 0.5 *. cos ((float v)/.fr));;
(*t plasma x y = Op.(+) (barsH x) (barsV y);;*)
let plasma x y = Op.normalize texsize (Op.(+) (barsH x) (barsV y));;
let plasma5 = plasma 5. 5.;;
(*Op.write "c.tga" texsize (Op.compose3 plasma5 plasma5 plasma5);Unix.system "feh c.tga";;*)
