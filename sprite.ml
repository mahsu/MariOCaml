type xy = float * float

type animation_typ =  | Reflect | Frame

type sprite_params =
  {
    max_frames: int;
    max_ticks: int;
    img_src: string;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: xy;
    anim: animation_typ;
    loop: bool; 
  }

type sprite = 
  {
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    ticks: int ref;
    max_frames: int;
    max_ticks: int;
    img: Dom_html.imageElement Js.t;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: float * float;
    bbox_size: xy;
    anim: animation_typ;
    x_refl: int;
    loop: bool;
  }

  
let setup_sprite ?anim:(anim=Frame) ?loop:(loop=true) img_src max_frames max_ticks frame_size src_offset = 
  {
        img_src;
    max_frames;
    max_ticks;
    frame_size;
    src_offset;
    bbox_offset = (0.,0.);
    bbox_size = frame_size;
    anim;
    loop;
  }
let from_actor = function
  | _ -> setup_sprite "./sprites/general.png" 1 0 (18.,18.) (335.,188.)
  (*Question mark normal | _ -> setup_sprite "./sprites/general.png" 4 15 (18.,18.) (299.,116.) *)
  (*Question mark unbreakable setup_sprite "./sprites/general.png" 1 0 (18.,18.) (371.,116.)*)
  (*Breakable brick setup_sprite "./sprites/general.png" 5 15 (18.,18.) (299.,134.) *)
  (*COIN *| _ -> setup_sprite "./sprites/general.png" 3 15 (18.,18.) (299.,98.)*)
  (* Mushroom setup_sprite "./sprites/general.png" 1 0 (18.,18.) (299.,188.) *)
  (* Flower setup_sprite "./sprites/general.png" 1 0 (18.,18.) (335.,188.) *)
  (* Star setup_sprite "./sprites/general.png" 1 0 (18.,18.) (353.,206.) *)

let new_sprite actor context  =
  let spr = from_actor actor in
  let img = (Dom_html.createImg Dom_html.document) in
  img##src <- (Js.string spr.img_src) ;
  {
    context;
    img;
    frame = ref 0;
    ticks = ref 0;
    max_ticks = spr.max_ticks;
    max_frames = spr.max_frames;
    frame_size = spr.frame_size;
    src_offset = spr.src_offset;
    bbox_offset= spr.bbox_offset;
    bbox_size = spr.bbox_size;
    anim = spr.anim;
    x_refl=0;
    loop= spr.loop;
  }

let reflect_sprite spr = failwith "todo"
let update_animation (spr: sprite) =
  (* Only advance frame when ticked *)
  let curr_ticks = !(spr.ticks) in
  if curr_ticks = spr.max_ticks then (
    spr.ticks := 0;
    match spr.anim with
    | Frame -> spr.frame := (!(spr.frame) + 1) mod spr.max_frames
    | Reflect -> reflect_sprite spr
  ) else spr.ticks := curr_ticks + 1
