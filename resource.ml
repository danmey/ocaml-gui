open GL
open Glu
let resources =
  ["button-normal", ("button_round_blue.png", ref None);
   "button-push", ("button_round_blue_push.png", ref None);]


let load_texture ~filename =
  let texture, width, height, internal_format, pixel_data_format =
    Png_loader.load_img (Filename filename)
  in
  Printf.printf "%d, %d\n" width height;
  flush stdout;
    let tid = glGenTexture() in
    glBindTexture BindTex.GL_TEXTURE_2D tid;
    glTexParameter ~target:TexParam.GL_TEXTURE_2D ~param:(TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_NEAREST);
    glTexParameter ~target:TexParam.GL_TEXTURE_2D ~param:(TexParam.GL_TEXTURE_MIN_FILTER Min.GL_NEAREST); 
    gluBuild2DMipmaps
      internal_format
      width height
      pixel_data_format
      GL_UNSIGNED_BYTE
      texture;
    tid
;;

(* let load () = *)
(*   List.iter  *)
(*     (fun (_, (filename, resource)) -> *)
(*       resource := load_texture ~filename) resources *)

let get ~name =
  match List.assoc name resources with
    | filename, reference when !reference = None -> 
      let filename = "resources/" ^ filename in
      let resource = load_texture ~filename in
      reference := Some resource;
      resource
    | _, reference -> 
      let Some r = !reference in
      r

