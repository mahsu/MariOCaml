open Object
open Sprite
module Html = Dom_html
let document = Html.document
let jstr = Js.string


let render obj = 
  let sprite = obj.sprite in
  let context = sprite.context in
  let (sx, sy) = sprite.src_offset in
  let (sw, sh) = sprite.frame_size in
  let (dx, dy) = obj.pos in
  let (dw, dh) = sprite.frame_size in
  let sx = (float_of_int !(sprite.frame)) *. sw in
  (*context##clearRect(0.,0.,sw, sh);*)
  context##drawImage_full(sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)


let clear_canvas canvas = 
  let context = canvas##getContext (Dom_html._2d_) in
  let cwidth = float_of_int canvas##width in
  let cheight = float_of_int canvas##height in
  ignore context##clearRect(0.,0.,cwidth,cheight)

let update_animation (sprite: sprite) =
  sprite.frame := (!(sprite.frame) + 1) mod sprite.max_frames 

 

let debug f = Printf.ksprintf (fun s -> Firebug.console##log (jstr s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith "poo") f

