type xy = float * float
type wh = float * float

type animation_typ =  | Reflection | Frame

type sprite_template =
  {
    max_frames: int;
    img_src: string;
    frame_size: wh;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: wh;
    anim: animation_typ;
    
  }

type sprite = 
  {
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    max_frames: int;
    img: Dom_html.imageElement Js.t;
    frame_size: wh;
    src_offset: xy;
    anim: animation_typ;
  }

let setup_sprite img_src max_frames frame_size src_offset = 
  {
    img_src;
    max_frames;
    frame_size;
    src_offset;
    bbox_offset = (0.,0.);
    bbox_size = (0.,0.);
    x_refl = 0;
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
    anim = Frame;
  }


let update_animation (sprite: sprite) =
  match sprite.anim with
  | Frame -> sprite.frame := (!(sprite.frame) + 1) mod sprite.max_frames
  | Reflect -> reflect_sprite sprite

let reflect_sprite sprite =
  failwith "TODO"
   

let get_bbox_origin = failwith "todo"

let get_bbox_center = failwith "todo"
