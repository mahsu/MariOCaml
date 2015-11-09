open Object
open Sprite
module Html = Dom_html
let document = Html.document
let jstr = Js.string


let render (obj: Object.collidable_object) = 
  let sprite = obj.sprite in
  let context = sprite.context in
  let (sx, sy) = sprite.src_offset in
  let (sw, sh) = sprite.frame_size in
  let (dx, dy) = obj.pos in
  let (dw, dh) = sprite.frame_size in
  let sx = (float_of_int !(sprite.frame)) *. sw in
  (*context##clearRect(0.,0.,sw, sh);*)
  context##drawImage_full(sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)


let update (sprite: sprite) =
  sprite.frame := (!(sprite.frame) + 1) mod sprite.max_frames 

let newobjs = ref []
let rec update_loop (canvas) objs = 
  newobjs := [];
  (*check for collisions *)
  let context = canvas##getContext (Dom_html._2d_) in
 ignore context##clearRect(0.,0.,float_of_int canvas##width,float_of_int canvas##height);(* clear canvas *)
 Printf.printf "clear\n" ;  
 List.iter (fun obj -> ignore (update_frame canvas obj)) objs ;
  ignore Dom_html.window##requestAnimationFrame( 
    Js.wrap_callback (fun (t:float) ->update_loop canvas !newobjs))

and update_frame canvas obj =
   (* TODO: optimize. Draw static elements only once *)
    Printf.printf "poop %f\n" (fst obj.pos); 
        render obj;
        update obj.sprite; (* return bool * variant *)
        (* if bool *)
        newobjs := if true then obj::!newobjs else !newobjs
    (*update_loop canvas*) 
  (*ignore Dom_html.window##requestAnimationFrame(
    Js.wrap_callback (fun (t:float) ->
    render obj; update obj.sprite; update_loop canvas obj))
*)
(* let init_draw canvas = 
  (* TODO load resources once on initialization *)
  let img = obj.sprite.img in
  ignore(Html.addEventListener  img Html.Event.load 
    (Html.handler (fun ev ->  update_loop canvas obj ; Js._true)) Js._true); ()
*)
 

let debug f = Printf.ksprintf (fun s -> Firebug.console##log (jstr s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith "poo") f

