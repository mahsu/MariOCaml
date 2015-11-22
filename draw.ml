open Object
open Sprite
module Html = Dom_html
let document = Html.document
let jstr = Js.string


let get_context canvas = canvas##getContext (Dom_html._2d_)

let render obj = 
  let sprite = obj.sprite in
  let context = sprite.context in
  let (sx, sy) = sprite.src_offset in
  let (sw, sh) = sprite.frame_size in
  let (dx, dy) = (obj.pos.x,obj.pos.y) in
  let (dw, dh) = sprite.frame_size in
  let sx = sx +. (float_of_int !(sprite.frame)) *. sw in
  (*context##clearRect(0.,0.,sw, sh);*)
  Printf.printf "xpos %f\n" sx;
  context##drawImage_full(sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)


let clear_canvas canvas = 
  let context = canvas##getContext (Dom_html._2d_) in
  let cwidth = float_of_int canvas##width in
  let cheight = float_of_int canvas##height in
  ignore context##clearRect(0.,0.,cwidth,cheight)


let fps canvas fps_val =
  let fps_str = int_of_float fps_val |> string_of_int in
  let context = canvas##getContext (Dom_html._2d_) in
  ignore context##fillText (Js.string fps_str, 10.,10.)

let debug f = Printf.ksprintf (fun s -> Firebug.console##log (jstr s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith "poo") f

