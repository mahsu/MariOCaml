open Object
module Html = Dom_html
let document = Html.document
let jstr = Js.string

type src_xy = float * float
type frame_wh = float * float

type sprite = 
  {
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    max_frames: int;
    img: Dom_html.imageElement Js.t;
    frame_size: frame_wh;
    src_offset_xy: src_xy;
  }

let render (obj: Object.obj) = 
  let sprite = obj.sprite in
  let context = sprite.context in
  let (sx, sy) = sprite.src_offset_xy in
  let (sw, sh) = sprite.frame_size in
  let (dx, dy) = obj.pos_xy in
  let (dw, dh) = sprite.frame_size in
  let sx = (float_of_int !(sprite.frame)) *. sw in
  context##clearRect(0.,0.,sw, sh); 
  context##drawImage_full(sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)


let update (sprite: sprite) =
  sprite.frame := (!(sprite.frame) + 1) mod sprite.max_frames 

let rec update_loop (canvas:Dom_html.canvasElement) (objs : sprite list) = 
  List.iter (fun obj -> Dom_html.window##requestAnimationFrame 
                Js.wrap_callback update_loop_cb ) objs
and update_loop_cb t:float=
    (* TODO: optimize. Draw static elements only once *)
    let context = canvas##getContext Html._2d_ in
    context##clearRect(0.0,0.0,canvas##width, canvas##height); (* clear canvas *)
    render obj;
    update obj.sprite
    (*update_loop sprite*)

let new_sprite context img_src frames (src: src_xy) (dest: dest_xy) (frame_wh: frame_wh) =
  let img = (Html.createImg document) in
  img##src <- (jstr img_src) ;
  let sprite =  {
    context;
    img;
    frame = ref 0;
    max_frames = frames;
    frame_size = frame_wh;
    src_offset_xy = src;
    dest_xy = dest;
  } in
  ignore(Html.addEventListener  img Html.Event.load 
    (Html.handler (fun ev ->  update_frame sprite ; Js._true)) Js._true);
  sprite


let debug f = Printf.ksprintf (fun s -> Firebug.console##log (jstr s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith "poo") f

