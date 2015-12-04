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
    mutable params: sprite_params;
    context: Dom_html.canvasRenderingContext2D Js.t;
    frame: int ref;
    ticks: int ref;
    mutable img: Dom_html.imageElement Js.t;
  }


let setup_sprite ?anim:(anim=Frame) ?loop:(loop=true)
         ?bb_off:(bbox_offset=(0.,0.)) ?bb_sz:(bbox_size=(0.,0.))
                 img_src max_frames max_ticks frame_size src_offset =
  let bbox_size = if bbox_size = (0.,0.) then frame_size else bbox_size in
  let img_src = "./sprites/" ^ img_src in
  {
    img_src;
    max_frames;
    max_ticks;
    frame_size;
    src_offset;
    bbox_offset;
    bbox_size;
    anim;
    loop;
  }


let make_small_player (typ, dir) =
  match dir with
    (* 16x16 grid with 0x0 offset*)
    | Left -> begin match typ with
      | Standing -> setup_sprite "mario-small.png" ~bb_off:(3.,1.) ~bb_sz:(11.,15.) 1 0 (16.,16.) (0.,0.)
      | Jumping -> setup_sprite "mario-small.png" ~bb_off:(2.,1.) ~bb_sz:(13.,15.) 2 10 (16.,16.) (16.,16.)
      | Running -> setup_sprite "mario-small.png" ~bb_off:(2.,1.) ~bb_sz:(12.,15.) 3 5 (16.,16.) (16.,0.)
      | Crouching -> setup_sprite "mario-small.png" ~bb_off:(1.,5.) ~bb_sz:(14.,10.) 1 0 (16.,16.) (0.,64.)
      end
    | Right -> begin match typ with
      | Standing -> setup_sprite "mario-small.png" ~bb_off:(1.,1.) ~bb_sz:(11.,15.) 1 0 (16.,16.) (0.,32.)
      | Jumping -> setup_sprite "mario-small.png" ~bb_off:(2.,1.) ~bb_sz:(13.,15.) 2 10 (16.,16.) (16.,48.)
      | Running -> setup_sprite "mario-small.png" ~bb_off:(2.,1.) ~bb_sz:(12.,15.) 3 5 (16.,16.) (16.,32.)
      | Crouching -> setup_sprite "mario-small.png" ~bb_off:(1.,5.) ~bb_sz:(14.,10.) 1 0 (16.,16.) (0.,64.)
      end

let make_big_player (typ, dir) =
  match dir with
  | Left -> begin match typ with
    | Standing -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,1.) ~bb_sz:(13.,25.) (16.,27.) (16.,5.)
    | Jumping -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,1.) ~bb_sz:(12.,25.) (16.,26.) (48.,6.)
    | Running -> setup_sprite "mario-big.png" 4 10 ~bb_off:(2.,1.) ~bb_sz:(13.,25.) (16.,27.)(0.,37.)
    | Crouching -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,10.) ~bb_sz:(13.,17.) (16.,27.) (32.,5.)
    end
  | Right -> begin match typ with
    | Standing -> setup_sprite "mario-big.png" 1 0 ~bb_off:(1.,1.) ~bb_sz:(13.,25.) (16.,26.) (16.,69.)
    | Jumping -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,1.) ~bb_sz:(12.,25.) (16.,26.) (48.,70.)
    | Running -> setup_sprite "mario-big.png" 4 10 ~bb_off:(2.,1.) ~bb_sz:(13.,25.) (16.,27.) (0.,101.)
    | Crouching -> setup_sprite "mario-big.png" 1 0 ~bb_off:(2.,10.) ~bb_sz:(13.,17.) (16.,27.) (32.,69.)
    end

let make_enemy (typ, dir) =
  match (typ, dir) with
      | (Goomba,_) -> setup_sprite "enemies.png" ~bb_off:(1.,1.) ~bb_sz:(14.,14.) 2 10 (16.,16.) (0.,128.)
      | (GKoopa,Left) -> setup_sprite "enemies.png" ~bb_off:(4.,8.) ~bb_sz:(11.,18.) 2 10 (16.,27.) (0.,69.)
      | (GKoopa,Right) -> setup_sprite "enemies.png" ~bb_off:(1.,9.) ~bb_sz:(11.,17.) 2 10 (16.,27.) (32.,69.)
      | (RKoopa,Left) -> setup_sprite "enemies.png" ~bb_off:(4.,9.) ~bb_sz:(11.,17.) 2 10 (16.,27.) (0.,5.)
      | (RKoopa,Right) -> setup_sprite "enemies.png" ~bb_off:(1.,8.) ~bb_sz:(11.,18.) 2 10 (16.,27.) (32.,5.)
      | (GKoopaShell,_) -> setup_sprite "enemies.png" ~bb_off:(2.,2.) ~bb_sz:(12.,13.) 4 10 (16.,16.) (0.,96.)
      | (RKoopaShell,_) -> setup_sprite "enemies.png" ~bb_off:(2.,2.) ~bb_sz:(12.,13.) 4 10 (16.,16.) (0.,32.)

let make_item = function
  (* 16x16 grid with 0x0 offset *)
  | Coin -> setup_sprite "items.png" ~bb_off:(3.,0.) ~bb_sz:(12.,0.) 3 15 (16.,16.) (0.,80.)
  | FireFlower -> setup_sprite "items.png" 1 0 (16.,16.) (0.,188.)
  | Mushroom -> setup_sprite "items.png" 1 0 (16.,16.) (0.,0.)
  | Star -> setup_sprite "items.png" 1 0 (16.,16.) (16.,48.)

let make_block = function
  (* 16x16 grid with 0x0 offset *)
  | Brick -> setup_sprite "blocks.png" 5 10 (16.,16.) (0.,0.)
  | QBlock _ -> setup_sprite "blocks.png" 4 15 (16.,16.) (0.,16.)
  | QBlockUsed -> setup_sprite "blocks.png" 1 0 (16.,16.) (0.,32.)
  | UnBBlock -> setup_sprite "blocks.png" 1 0 (16.,16.) (0.,48.)


let make_player pt spr_type =
  match pt with
  | BigM -> make_big_player spr_type
  | SmallM -> make_small_player spr_type

let make_type typ (dir : Actors.dir_1d) =
  match typ with
  | SPlayer(pt,st) -> make_player pt (st,dir)
  | SEnemy t -> make_enemy (t,dir)
  | SItem t -> make_item t
  | SBlock t -> make_block t

let make_from_params params context =
  let img = (Dom_html.createImg Dom_html.document) in
  img##src <- (Js.string params.img_src) ;
  {
    params;
    context;
    img;
    frame = ref 0;
    ticks = ref 0;
  }

let make spawn dir context  =
  let params = make_type spawn dir in
  make_from_params params context

let make_bgd context =
  let params = setup_sprite "bgd-1.png" 1 0 (512.,256.) (0.,0.) in
  make_from_params params context

let transform_enemy enemy_typ spr dir =
  let params = make_enemy  (enemy_typ,dir) in
  let img = (Dom_html.createImg Dom_html.document) in
  img##src <- (Js.string params.img_src) ;
  spr.params <- params;
  spr.img <- img

let reflect_sprite spr = failwith "todo"

let update_animation (spr: sprite) =
  (* Only advance frame when ticked *)
  let curr_ticks = !(spr.ticks) in
  if curr_ticks >= spr.params.max_ticks then (
    spr.ticks := 0;
    match spr.params.anim with
    | Frame ->
        if spr.params.loop then
        spr.frame := (!(spr.frame) + 1) mod spr.params.max_frames
    | Reflect -> reflect_sprite spr
  ) else spr.ticks := curr_ticks + 1
