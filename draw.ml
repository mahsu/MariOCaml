open Object
open Sprite
module Html = Dom_html
let document = Html.document
let jstr = Js.string


let render (obj: Object.obj) = 
  let sprite = obj.sprite in
  let context = sprite.context in
  let (sx, sy) = sprite.src_offset in
  let (sw, sh) = sprite.frame_size in
  let (dx, dy) = obj.pos in
  let (dw, dh) = sprite.frame_size in
  let sx = (float_of_int !(sprite.frame)) *. sw in
  context##clearRect(0.,0.,sw, sh); 
  context##drawImage_full(sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)


let update (sprite: sprite) =
  sprite.frame := (!(sprite.frame) + 1) mod sprite.max_frames 

let rec update_loop canvas obj = 
  (*List.iter (fun obj -> Dom_html.window##requestAnimationFrame 
                Js.wrap_callback (fun t:float ->
    (* TODO: optimize. Draw static elements only once *)
    let context = canvas##getContext Html._2d_ in
    context##clearRect(0.,0.,canvas##width, canvas##height); (* clear canvas *)
    render obj;
    update obj.sprite
    (*update_loop sprite*)
  )) objs*)
  ignore Dom_html.window##requestAnimationFrame(
    Js.wrap_callback (fun (t:float) ->
    render obj; update obj.sprite; update_loop canvas obj))

let init_draw canvas (obj: obj) = 
  (* TODO load resources once on initialization *)
  let img = obj.sprite.img in
  ignore(Html.addEventListener  img Html.Event.load 
    (Html.handler (fun ev ->  update_loop canvas obj ; Js._true)) Js._true); ()
 

let debug f = Printf.ksprintf (fun s -> Firebug.console##log (jstr s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith "poo") f

