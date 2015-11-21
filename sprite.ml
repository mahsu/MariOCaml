type xy = float * float

type animation_typ =  | Reflect | Frame

type sprite_params =
  {
    max_frames: int;
    img_src: string;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: xy;
    anim: animation_typ; 
  }

type sprite = 
  {
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    max_frames: int;
    img: Dom_html.imageElement Js.t;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: float * float;
    bbox_size: xy;
    anim: animation_typ;
    x_refl: int;
  }

let setup_sprite img_src max_frames frame_size src_offset = 
  {
    img_src;
    max_frames;
    frame_size;
    src_offset;
    bbox_offset = (0.,0.);
    bbox_size = (0.,0.);
    anim = Frame;
  }

let new_sprite actor context  =
  let img = (Dom_html.createImg Dom_html.document) in
  (*img##src <- (Js.string spr.img_src) ;*)
  {
    context;
    img;
    frame = ref 0;
    max_frames = spr.max_frames;
    frame_size = spr.frame_size;
    src_offset = spr.src_offset;
    bbox_offset= spr.bbox_offset;
    bbox_size = spr.bbox_size;
    anim = spr.anim;
    x_refl=0;
  }


let update_animation (spr: sprite) =
  match spr.anim with
  | Frame -> spr.frame := (!(spr.frame) + 1) mod spr.max_frames
  | Reflect -> reflect_sprite spr

let reflect_sprite spr = failwith "todo"
