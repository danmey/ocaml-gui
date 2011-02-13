let texsize = 256

(*type result = Val of float | RGB of (float * float * float)*)

type cloud_params = 
    { persistence : float; 
      octaves : int;
      seed: int}

type transform_params = 
    { ox : float; 
      oy : float;
      tx : float;
      ty : float;
      sx : float;
      sy : float;
      rot : float; }

type mask_mode =
  | Inner of float * float
  | Outer of float * float

type mix_mode =
  | Lower
  | Greater

type light_params =
    { lx : float;
      ly : float;
      ldx : float;
      ldy : float }

type flat_params =
    { fx : float;
      fy : float;
      fw : float;
      fh : float;
      fg : float;
      bg : float }

type glow_params =
    { x0 : float;
      y0 : float;
      atten : float;
      xr : float;
      yr : float; }

type hsv_params =
    { hp : float;
      sp : float;
      vp : float; }

type rgb_params =
    { rp : float;
      gp : float;
      bp : float; }

type pixels_params =
    { count: int }

type blur_mode =
  | Horizontal
  | Vertical
  | Both

type blur_params =
    { radius : float;
      times : int;
      blur_mode: blur_mode; }

type distort_mode =
  | Radial

type distort_params = { dtype : distort_mode; dscale : float }
 
type phi_params =
    { scale : float;
      base : float; }

type phi3_params =
    { scale1 : float;
      base1  : float; 
      scale2 : float;
      base2  : float; 
      scale3 : float;
      base3  : float; }

type operator = 
  | Add of operator list
  | Modulate of operator list
  | Clouds of cloud_params
  | Clamp of operator
  | Transform of transform_params * operator
  | Mask of mask_mode * operator
  | Mix of mix_mode * operator * operator
  | Light of light_params * operator
  | Distort of distort_params * operator * operator * operator
  | Flat of flat_params
  | Glow of glow_params
  | Hsv of hsv_params * operator * operator * operator
  | Rgb of rgb_params * operator * operator * operator
  | Pixels of pixels_params
  | Blur of blur_params
  | Phi of phi_params * operator
  | Phi3 of phi3_params * operator
  | Custom of operator



module Layer = struct
  let make f = f
end

module Memo = struct
  type 'a pixel = 
      { x : float; 
        y : float; 
        avalue : 'a}
  type 'a t = ((int*int), 'a pixel) Hashtbl.t

  let add_pixel layer x y col = Hashtbl.add layer (x, y) {x=x; y=y; avalue=col}

  let get_pixel layer f (x, y) = 
    try
      let pix = Hashtbl.find layer (x, y) in
	pix.avalue
    with Not_found -> 
      let col = f (x, y) in
	add_pixel layer x y col;
	col

  let make f =
    let new_layer (x, y) = Hashtbl.create (10*x*y) in
    let layer = new_layer (texsize, texsize) in
      get_pixel layer f
end



let pi = BatFloat.pi
let pi2 = 2. *. BatFloat.pi 
let noise seed =
  Random.init seed;
  let state = ref (Random.get_state()) in
  Memo.make 
    (fun (_, _) -> 
      Random.set_state !state;
      let res = (Random.float 1.0) *. pi2 in
      state := Random.get_state ();
      res)

let lift f gen (x, y) = f (gen (x, y))


let gradient_u = 
  let arr = Array.make 10 (fun _ -> 0.) in
  for i = 0 to Array.length arr-1; do
    arr.(i) <- Memo.make (lift (fun x -> (sin x)) (noise i))
  done;
  arr
let gradient_v = 
  let arr = Array.make 10 (fun _ -> 0.) in
  for i = 0 to Array.length arr-1; do
    arr.(i) <- Memo.make (lift (fun x -> (cos x)) (noise i))
  done;
  arr

let colorize color_lst layer =
  let interp a b f = (b-.a) *. f +. a in
  let interpolate (r1,g1,b1) (r2,g2,b2) f1 f2 f = 
    let f' = (f -. f1) /. (f2-.f1) in
    interp r1 r2 f',interp g1 g2 f',interp b1 b2 f' in
  fun (x, y) ->
    let value = layer x y in
    let l1,l2 = List.partition (fun (v,c) -> BatFloat.(value > v)) color_lst in
    let v1,c1 = List.hd (List.rev l1) in
    let v2,c2 = List.hd l2 in
    interpolate c1 c2 v1 v2 value
    
let cloud seed w h =
  let open BatInt in
  fun (x,y) ->
    let m x w = 
      let _,v = modf (x /. w) in
      v *. w in
    let gx = m x w in
    let gy = m y h in
    let grad_uv xg yg = gradient_u.(seed) (xg, yg), gradient_v.(seed) (xg, yg) in
    let gx1, gy1 = grad_uv  gx     gy in
    let gx2, gy2 = grad_uv (gx+.w)  gy in
    let gx3, gy3 = grad_uv  gx     (gy+.h) in
    let gx4, gy4 = grad_uv (gx+.w) (gy+.h) in
    let inter v1 v2 mu =
      let mu2 = (1.0-.cos(mu*.pi))/.2.0 in
      v1 *. (1.0 -. mu2) +. v2 *. mu2 in
    let inter2d s t u v a b =
      let v1 = inter s t a in
      let v2 = inter u v a in
      inter v1 v2 b in
    let xf = fst (modf (x /.  w)) in
    let yf = fst (modf (y /.  h)) in
    let dot_val gu gv x' y' = gu *. x' +. gv *. y' in
    let s,t,u,v = 
      dot_val gx1 gy1       xf    yf, 
      dot_val gx2 gy2 (xf-.1.0)   yf, 
      dot_val gx3 gy3  xf        (yf-.1.0),
      dot_val gx4 gy4 (xf-.1.0)  (yf-.1.0)
    in
    inter2d s t u v xf yf

let clouds { seed; octaves; persistence } =
  let rec gen_list lst a = 
    if a > 0 then 
      gen_list (((List.hd lst) *. 2.) :: lst) (a-1) 
    else lst 
  in
  let clouds_a = gen_list [8./.256.] (octaves - 1) in
  let clouds = List.map (fun a -> cloud seed a a) clouds_a in
  fun (x, y) -> 
    let v,_,d = ((List.fold_left 
           (fun (acc,p,d) cl -> 
             let cl (x,y) =
             let x, y = fst (modf x), fst (modf y) in
             let l = cl in
             (x *. y *. cl (1.0-.x,1.-.y) +. (1.0 -. x) *. y          *. l (x,1.-.y) +.
                (                     (1.0 -. x) *. (1.0 -. y) *. l (x,y)) +.
                (                     (       x) *. (1.0 -. y) *. l (1.0-. x,y)))
             in
             acc +. p *. 0.65 +.  p *. (cl (x, y)), p *. persistence, d+.p*.1.35))
           (0.0, persistence,0.) clouds) in
    v/.d
      
(* let wrap l = fun (x, y) -> *)
(*   let x, y = fst (modf x), fst (modf y) in *)
(*   (x *. y *. l (x,y) +. (1.0 -. x) *. y          *. l (1.0-.x,y) +. *)
(*   (                     (1.0 -. x) *. (1.0 -. y) *. l (1.0-.x,1.0-.y)) +. *)
(*   (                     (       x) *. (1.0 -. y) *. l (     x,1.0-.y))) *)
   


(* let clouds p = wrap (clouds p) *)
  

      
let wrap c = c land (texsize - 1)

let transform f l1 l2 =
  fun (x, y) -> 
    let x', y' = f (l1 x y) in 
    l2 (wrap (x'+x)) (wrap (y'+y))

let light op { lx; ly; ldx; ldy } = 
  let open BatFloat in
  fun (x, y) -> 
    let v1,v2 = 
      op (x+ldx, y) -. op ((x-ldx), y), 
      op (x ,y+ldy) -. op (x, (y-ldy))
    in
    let dot = v1 *. lx +. v2 *. ly in
    if dot > 0. then op (x, y) *. dot else 0.

  (* let normalize w layer = *)
  (*   let arr = Array.make (w*w) 0.0 in *)
  (*   let max = ref (-1E8) in *)
  (*   let min = ref  1E8 in *)
  (*   for y = 0 to (texsize-1) do *)
  (*     for x = 0 to (texsize-1) do *)
  (*       let c = (layer x y) in *)
  (*       if c > !max then max := c; *)
  (*       if c < !min then min := c; *)
  (*     done; *)
  (*   done; *)
  (*   for y = 0 to w-1 do *)
  (*     for x = 0 to w-1 do *)
  (*       let c = layer x y in *)
  (*       Array.set arr (y*w+x) ((c -. !min) /. (!max -. !min)) *)
  (*     done; *)
  (*   done; *)
  (*   Layer.make (fun u v -> arr.(u*w+v)) *)

let hsv op1 op2 op3 { hp; sp; vp; } =
  fun p ->
    let h = hp *. op1 p in
    let s = sp *. op2 p in
    let v = vp *. op3 p in
    let h' = mod_float h 1.0 in
    let hi = abs (int_of_float (floor (360. *. h'/.60.0))) mod 6 in
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

let array_of_texture layer = 
  let array = Array.make (texsize*texsize) (0.,0.,0.,1.) in
  for y = 0 to (texsize-1) do
    for x = 0 to (texsize-1) do
        (* let r,g,b = layer x y in  *)
      let v = layer (float x /. float texsize) (float y /. float texsize) in
        (* let r' = int_of_float (v *. 255.0) in *)
        (* let g' = int_of_float (v *. 255.0) in *)
        (* let b' = int_of_float (v *. 255.0) in  *)
      array.(y*texsize+x) <- (v, v, v, 1.)
    done
  done;
  array

(* let (+) l1 l2 = (fun u v -> l1 u v +. l2 u v) *)
let compose3 l1 l2 l3 = (fun u v -> l1 u v, l2 u v, l3 u v)
(* let pixels treshold =  (fun u v -> if Random.float 1.0 < treshold then 1.0 else 0.0) *)
(* let force l = for u = 0 to (texsize-1) do for v = 0 to (texsize-1) do ignore(l u v) done done; l *)


let gray3 l = Layer.make (fun u v -> let p = l u v in (p,p,p))

module TGA =
struct
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
end

let prim op start lst (point : (float * float)) =
  List.fold_left (fun acc f -> op acc (f point)) start lst

(* let mk_op f a b = *)
(*   match a,b with *)
(*     | Val a,Val b -> Val (f a b) *)
(*     | RGB (r,g,b), Val v -> RGB (f r v, f g v, f b v) *)
(*     | Val v, RGB (r,g,b) -> RGB (f r v, f g v, f b v) *)
(*     | RGB (r1,g1,b1), RGB (r2,g2,b2) -> RGB (f r1 r2, f g1 g2, f b1 b2) *)

let add = prim ( +. ) 0.

let modulate = prim ( *. ) 1.
let modulate3 = prim (fun (r1,b1,g1) (r2,g2,b2) -> r1 *. r2, g1 *. g2, b1 *. b2) (1., 1., 1.)

let to_rad value = value *.  2. *. pi

let transform op { ox; oy; tx; ty; sx; sy; rot; } =
  let open BatFloat in
  let ang = to_rad rot in
  let ang_90 = to_rad (0.5-rot) in
  let dy1 = sin ang * sx in
  let dx1 = cos ang * sx in
  let dy2 = cos ang_90 * sy in
  let dx2 = sin ang_90 * sy in
  let start_x = (-.ox) + tx in
  let start_y = (-.oy) + ty in
  fun (x,y) ->
    let cur_x = (start_x + y * dx2) + x * dx1 in
    let cur_y = (start_y + y * dy2) + y * dy1 in
    op (cur_x, cur_y)

let distort Radial scale dir pw dst =
  let open BatFloat in
  fun (x, y) ->
    let raddir = dir (x, y) * 2. * pi in
    let xd = sin raddir * pw (x, y) /. scale in
    let yd = cos raddir * pw (x, y) /. scale in
    dst (x+xd, y+yd)

type channels = Ch of float | Ch3 of (float * float * float)
type arity = int
let rec value op =
  (match op with
    | Add lst -> add (lift_lst lst)
    | Modulate lst -> modulate (lift_lst lst)
      | Clouds params -> clouds params
      | Clamp op ->
        let clamp v = if v < 0. then 0. else if v > 1.0 then 1.0 else v in
        fun point -> clamp (value op point)
      | Transform (params, op) -> transform (value op) params
      | Mask (Inner (a, b), op) ->
        let mask v = if v >= a && v < b then v else 0. in
        fun point -> mask (value op point)
      | Mask (Outer (a, b), op) ->
        let open BatFloat in
        let mask v = if v < a || v > b then v else 0. in
        fun point -> mask (value op point)
      | Mix (mode, op1, op2) ->
        let open BatFloat in
        let op = match mode with Lower -> ( < ) | Greater -> ( >= ) in
        fun point ->
        let v1 = value op1 point in
        let v2 = value op2 point in
        if op v1 v2 then v1 else v2
      | Light (params, op) -> light (value op) params
      | Distort ({dtype=Radial;dscale},op1, op2, op3) -> distort Radial dscale (value op1) (value op2) (value op3)
      | Flat { fx; fy; fw; fh; fg; bg; } ->
        fun (x,y) ->
          if x >= fx && x < fx+.fw
            && y >= fy && y <= fy+.fh then fg else bg
      | Glow { x0; y0; atten; xr; yr; } ->
        fun (x,y) ->
        let v' = sqrt ((x-.x0)*.(x-.x0)*.xr +. (y-.y0)*.(y-.y0)*.yr) in
        let v = BatFloat.pow v' atten in
        v
      | Phi ({ base; scale; }, op) -> fun (x, y) -> scale *. (value op (x,y) +. base))
                
and channel_value op p = Ch (value op p)
and lift_lst lst = BatList.map value lst
and value3 op =
  (match op with
    | Rgb ({rp; gp; bp; }, r,g,b) ->
      fun point ->
        let Ch r = get r point in
        let Ch g = get g point in
        let Ch b = get b point in
      r*.rp, g*.gp, b*.bp
    | Distort ({dtype=Radial;dscale},op1, op2, op3) -> 
        distort Radial dscale (value op1) (value op2) (value3 op3)
    | Hsv (params, op1, op2, op3) -> 
        hsv (value op1) (value op2) (value op3) params
    | Add lst ->
      fun point ->
        let f x = let Ch3 r = get x point in r in
        (List.fold_left (fun (ar,ag,ab) (r,g,b) ->
          ar+.r, ag+.g, ab+.b) (0.,0.,0.) (List.map f lst))
    | Modulate lst -> modulate3 (List.map value3 lst)
    | Phi3 ({ scale1;
              base1;
              scale2;
              base2;
              scale3;
              base3; }, op) ->
      fun p ->
        let r,g,b = value3 op p in
        scale1 *. (r +. base1),
        scale2 *. (g +. base2),
        scale3 *. (b +. base3))
    
and channel3_value op p = Ch3 (value3 op p) 
and get op =
  let f = match get_arity op with 3 -> channel3_value | 1 -> channel_value in
  f op
and get_arity = function
  | Rgb _ -> 3
  | Hsv _ -> 3
  | Phi3 _ -> 3
  | Modulate lst -> let a::_ = lst in get_arity a
  | Add lst -> let a::_ = lst in get_arity a
  | Distort (_,_,_,dst) -> get_arity dst
  | op -> 1

let layer op x y = 
  let v = value op ((float y /. 256.0), (float x /. 256.0)) in
  (v,v,v)

let layer2 op x y = 
  let v = value op ((float y), (float x)) in
  (v,v,v)


(* let tree = *)
(*   Clamp (Phi ({base = 0.15; scale = 1.6}, *)
(*        (Clouds { octaves = 4; persistence = 0.5; })));; *)

module A = Bigarray.Genarray;;

let ar = A.create Bigarray.int8_unsigned Bigarray.c_layout [|256;256;4|]
let ar_src = Array.create (256*256) 0.
let current_line = ref 0
let working = ref true
let operator = ref None
let max = ref (-1000000.)
let min = ref ( 1000000.)

let update_texture () =
  (if !working then
      BatOption.may (fun op ->
        let y =  !current_line in
        for x = 0 to 256 - 1 do
          let r,g,b = match get op ((float y /. 256.0), (float x /. 256.0)) with
            | Ch v -> v,v,v
            | Ch3 v -> v in
          if r > !max then max := r;
          if r < !min then min := r;
          let r' = int_of_float (r *. 255.0) in 
          let g' = int_of_float (g *. 255.0) in 
          let b' = int_of_float (b *. 255.0) in 
          A.set ar [|y;x;0|] r';
          A.set ar [|y;x;1|] g';
          A.set ar [|y;x;2|] b';
          A.set ar [|y;x;3|] 255;
        done;
        current_line := !current_line + 1;
        if !current_line > 255 then
            (current_line := 0;
            working := false;
            Printf.printf "max: %f, min: %f\n" !max !min;
            flush stdout;
            max := (-1000000.);
            min := ( 1000000.);
);
  ) !operator)

let save_texture file_name =
  let image_width = 256 in
  let image_height = 256 in
  let fptr = open_out_bin file_name in
  let putc (b, ch) = output_byte ch b in
  putc(0,fptr);
  putc(0,fptr);
  putc(2,fptr);
  putc(0,fptr); putc(0,fptr);
  putc(0,fptr); putc(0,fptr);
  putc(0,fptr);
  putc(0,fptr); putc(0,fptr);
  putc(0,fptr); putc(0,fptr);
  putc(image_width land 0x00FF,fptr);
  putc((image_width land 0xFF00) / 256,fptr);
  putc((image_height land 0x00FF),fptr);
  putc((image_height land 0xFF00) / 256,fptr);
  putc(32,fptr);
   putc(0,fptr);
   for y = 0 to image_height-1 do
   for x = 0 to image_width-1 do
     let r = A.get ar [|y;x;0|] in
     let g = A.get ar [|y;x;1|] in
     let b = A.get ar [|y;x;2|] in
     let a = A.get ar [|y;x;3|] in
     putc(r,fptr);
     putc(g,fptr);
     putc(b,fptr);
     putc(a,fptr)
   done
     done

let reset () =
  current_line := 0;
  working := true




(* TGA.write "test.tga" 256 (layer tree) *)

(* let op =      *)
(*   Add [Glow { x0 = 0.0; *)
(*     y0 = 0.0; *)
(*     atten = 1.0; *)
(*     xr = 1.0; *)
(*     yr = 1.0; }; *)
(*     Glow { x0 = 0.0; *)
(*            y0 = 0.0; *)
(*            atten = 1.0; *)
(*            xr = 1.0; *)
(*            yr = 1.0; }] *)
(* ;; *)

(* let op = tree;; *)
(* TGA.write "test.tga" 256 (layer op) *)
