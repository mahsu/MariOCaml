type xy = float * float
type wh = float * float

type sprite_params =
  {
    max_frames: int;
    img_src: string;
    frame_size: wh;
    src_offset: xy;
  }

type sprite = 
  {
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    max_frames: int;
    img: Dom_html.imageElement Js.t;
    frame_size: wh;
    src_offset: xy;
  }

let setup_sprite img_src max_frames frame_size src_offset = 
  {
    img_src;
    max_frames;
    frame_size;
    src_offset;
  }

let new_sprite spr context  =
  let img = (Dom_html.createImg Dom_html.document) in
  img##src <- (Js.string spr.img_src) ;
  {
    context;
    img;
    frame = ref 0;
    max_frames = spr.max_frames;
    frame_size = spr.frame_size;
    src_offset = spr.src_offset;
  }
