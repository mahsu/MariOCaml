open Actors
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
    params: sprite_params;
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    ticks: int ref;
    img: Dom_html.imageElement Js.t;
    x_refl: int;
  }

  
let setup_sprite ?anim:(anim=Frame) ?loop:(loop=true) 
                 img_src max_frames max_ticks frame_size src_offset = 
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

let make_player = function
  | Standing -> failwith "todo"
  | Jumping -> failwith "todo"
  | Running -> failwith "todo"
  | Crouching -> failwith "todo"

let make_enemy = function
  | Goomba -> failwith "todo" 
  | GKoopa -> failwith "todo"
  | RKoopa -> failwith "todo"
  | GKoopaShell -> failwith "todo"
  | RKoopaShell -> failwith "todo"

let make_item = function
  (* 18x18 grid with 11x8 offset *)
  | Coin -> setup_sprite "./sprites/general.png" 3 15 (18.,18.) (299.,98.)
  | FireFlower -> setup_sprite "./sprites/general.png" 1 0 (18.,18.) (335.,188.)
  | Mushroom -> setup_sprite "./sprites/general.png" 1 0 (18.,18.) (299.,188.)
  | Star -> setup_sprite "./sprites/general.png" 1 0 (18.,18.) (353.,206.)

let make_block = function
  (* 18x18 grid with 11x8 offset *)
  | Brick -> setup_sprite "./sprites/general.png" 5 15 (18.,18.) (299.,134.)
  | QBlock -> setup_sprite "./sprites/general.png" 4 15 (18.,18.) (299.,116.)
  | QBlockUsed -> setup_sprite "./sprites/general.png" 1 0 (18.,18.) (371.,116.)
  | UnBBlock -> failwith "todo"

let make_type = function
  | SPlayer t -> make_player t 
  | SEnemy t -> make_enemy t
  | SItem t -> make_item t
  | SBlock t -> make_block t
      
let make spawn context  =
  let params = make_type spawn in
  let img = (Dom_html.createImg Dom_html.document) in
  img##src <- (Js.string params.img_src) ;
  {
    params;
    context;
    img;
    frame = ref 0;
    ticks = ref 0;
    x_refl=0;
  }

let reflect_sprite spr = failwith "todo"

let update_animation (spr: sprite) =
  (* Only advance frame when ticked *)
  let curr_ticks = !(spr.ticks) in
  if curr_ticks = spr.params.max_ticks then (
    spr.ticks := 0;
    match spr.params.anim with
    | Frame -> 
        if spr.params.loop then 
        spr.frame := (!(spr.frame) + 1) mod spr.params.max_frames
    | Reflect -> reflect_sprite spr
  ) else spr.ticks := curr_ticks + 1
