open Object
open Sprite
module Html = Dom_html
let document = Html.document
let jstr = Js.string

let get_context canvas = canvas##getContext (Dom_html._2d_)

let render sprite (posx,posy) =
  let context = sprite.context in
  let (sx, sy) = sprite.params.src_offset in
  let (sw, sh) = sprite.params.frame_size in
  let (dx, dy) = (posx,posy) in
  let (dw, dh) = sprite.params.frame_size in
  let sx = sx +. (float_of_int !(sprite.frame)) *. sw in
  (*print_endline (string_of_int !(sprite.frame));*)
  (*context##clearRect(0.,0.,sw, sh);*)
  context##drawImage_full(sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)

let draw_bgd bgd off_x =
  render bgd (~-.off_x,0.);
  render bgd ((fst bgd.params.frame_size) -. off_x, 0.)

let clear_canvas canvas =
  let context = canvas##getContext (Dom_html._2d_) in
  let cwidth = float_of_int canvas##width in
  let cheight = float_of_int canvas##height in
  ignore context##clearRect(0.,0.,cwidth,cheight)

let hud canvas score coins =
  let score_string = string_of_int score in
  let coin_string = string_of_int coins in
  let context = canvas##getContext (Dom_html._2d_) in
  ignore context##font <- (Js.string ("10px 'Press Start 2P'"));
  ignore context##fillText (Js.string ("Score: "^score_string), (float_of_int canvas##width) -. 140., 18.);
  ignore context##fillText (Js.string ("Coins: "^coin_string), 120., 18.)

let fps canvas fps_val =
  let fps_str = int_of_float fps_val |> string_of_int in
  let context = canvas##getContext (Dom_html._2d_) in
  ignore context##fillText (Js.string fps_str, 10.,18.)

let draw_background_color canvas = failwith "todo"
let debug f = Printf.ksprintf (fun s -> Firebug.console##log (jstr s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith "poo") f

