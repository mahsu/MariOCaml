open Graphics_js
module Html = Dom_html
let document = Html.document
let jstr = Js.string

type src_xy = float * float
type dest_xy = float * float
type frame_wh = float * float

type sprite = 
  {
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    max_frames: int;
    img: Dom_html.imageElement Js.t;
    frame_size: frame_wh;
    src_offset_xy: src_xy;
    dest_xy: dest_xy;
  }
let render (sprite: sprite) = 
  let context = sprite.context in
  let (sx, sy) = sprite.src_offset_xy in
  let (sw, sh) = sprite.frame_size in
  let (dx, dy) = sprite.dest_xy in
  let (dw, dh) = sprite.frame_size in
  let sx = (float_of_int !(sprite.frame)) *. sw in
  context##clearRect(0.,0.,sw, sh); 
  context##drawImage_full(sprite.img, sx, sy, sw, sh, dx, dy, dw, dh)


let update (sprite: sprite) =
  sprite.frame := (!(sprite.frame) + 1) mod sprite.max_frames 

let rec update_frame (sprite:sprite) = 
  ignore  Dom_html.window##requestAnimationFrame(Js.wrap_callback(fun (time:float)  -> 
    render sprite; update sprite;  (Printf.printf"frame no %d\n" !(sprite.frame)); update_frame sprite))


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
let load _ =
  let canvas_id = "canvas" in
  let canvas = 
    Js.Opt.get 
      (Js.Opt.bind ( document##getElementById(jstr canvas_id)) 
        Dom_html.CoerceTo.canvas) 
      (fun () -> alert "cant find canvas %s" canvas_id) in
  (*let () = Graphics_js.open_canvas canvas in*)
  (*let context = get_context() in*)
  let context = canvas##getContext (Html._2d_) in
  let coin = new_sprite context "coin.png" 10 (0.0,0.0) (0.0,0.0) (100.0,100.0) in
  update_frame coin ;
  Printf.printf("sdf \n");
  (*let () = set_context canvas in*)
  (*debug "asd" ; canvas*)
  ()
let _ = Dom_html.window##onload <- Dom_html.handler (fun _ -> ignore (load ()); Js._true)
