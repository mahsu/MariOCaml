open Sprite
open Actors

let friction = 0.8
let gravity = 0.05

type xy = {
  mutable x: float;
  mutable y: float;
}


type aabb = {
  center: xy;
  half: xy;
}

type obj_params = {
  has_gravity: bool;
  speed: float;
}

let id_counter = ref min_int

type obj = {
  params: obj_params;
  pos: xy;
  vel: xy;
  id: int;
  mutable jumping: bool;
  mutable grounded: bool;
  mutable dir: Actors.dir_1d;
  mutable invuln: int;
  mutable kill: bool;
}

type collidable =
  | Player of sprite * obj
  | Enemy of enemy_typ * sprite * obj
  | Item of item_typ * sprite * obj
  | Block of block_typ * sprite * obj

type noncollidable =
  (*| Dead of dead_type * sprite*)
  | Scenery of sprite * obj

let setup_obj ?g:(has_gravity=true) ?spd:(speed=3.) () =
  {
    has_gravity;
    speed;
  }

let make_player () = setup_obj ()

let make_item = function
  | Mushroom -> setup_obj ()
  | FireFlower -> setup_obj ()
  | Star -> setup_obj ()
  | Coin -> setup_obj ~g:false ()

let make_enemy = function
  | Goomba -> setup_obj ()
  | GKoopa -> setup_obj ()
  | RKoopa -> setup_obj ()
  | GKoopaShell -> setup_obj ~spd:0. ()
  | RKoopaShell -> setup_obj ~spd:0. ()

let make_block = function
  | QBlock i -> setup_obj ~g:false ()
  | QBlockUsed -> setup_obj ~g:false ()
  | Brick -> setup_obj ~g:false ()
  | UnBBlock -> setup_obj ~g:false ()

let make_type = function
  | SPlayer t -> make_player ()
  | SEnemy t -> make_enemy t
  | SItem t -> make_item t
  | SBlock t -> make_block t

let spawn spawnable context (posx, posy) =
  let spr = Sprite.make spawnable Right context in
  let params = make_type spawnable in
  id_counter := !id_counter + 1;
  let obj = {
    params;
    pos = {x=posx; y=posy};
    vel = {x=0.0;y=0.0};
    id = !id_counter;
    jumping = false;
    grounded = false;
    dir = Left;
    invuln = 0;
    kill = false;
  } in
  match spawnable with
  | SPlayer t -> Player(spr,obj)
  | SEnemy t -> Enemy(t,spr,obj)
  | SItem t -> Item(t,spr,obj)
  | SBlock t -> Block(t,spr,obj)

  
let get_sprite = function
  | Player (s,_) | Enemy (_,s, _) | Item (_,s, _) | Block (_,s, _)  -> s

let get_obj = function
  | Player (_,o)
  | Enemy (_,_,o) | Item (_,_,o) | Block (_,_,o) -> o

let is_player = function
  | Player(_,_) -> true
  | _ -> false

let equals col1 col2 = (get_obj col1).id = (get_obj col2).id

let update_player_keys (player : obj) (controls : controls) : unit =
  match controls with
  | CLeft ->
    if player.vel.x > ~-.(player.params.speed)
    then player.vel.x <- player.vel.x -. 1.;
    player.dir <- Left
  | CRight ->
    if player.vel.x < player.params.speed
    then player.vel.x <- player.vel.x +. 1.;
    player.dir <- Right
  | CUp ->
    if (not player.jumping) then begin
      player.jumping <- true; 
      player.grounded <- false;
      player.vel.y <- ~-.(player.params.speed)
    end
  | CDown ->
    if (not player.jumping) then print_endline "crouch"

let update_player player keys context =
  let prev_jumping = player.jumping in
  let prev_dir = player.dir in
  List.iter (update_player_keys player) keys;
  let v = player.vel.x *. friction in
  let vel_damped = if abs_float v < 0.1 then 0. else v in
  let () = player.vel.x <- vel_damped in
  if not prev_jumping && player.jumping
  then Some (Sprite.make (SPlayer Jumping) player.dir context)
  else if prev_dir <> player.dir && not player.jumping
  then Some (Sprite.make (SPlayer Running) player.dir context)
  else if prev_dir <> player.dir && player.jumping && prev_jumping
  then Some (Sprite.make (SPlayer Jumping) player.dir context)
  else if player.vel.y = 0. && player.vel.x = 0.
  then Some (Sprite.make (SPlayer Standing) player.dir context)
  else None

let update_vel obj =
  if obj.grounded then obj.vel.y <- 0.
  else if obj.params.has_gravity then obj.vel.y <- (obj.vel.y +. gravity)

let update_pos obj =
  obj.pos.x <- (obj.vel.x +. obj.pos.x);
  if obj.params.has_gravity then obj.pos.y <- (obj.vel.y +. obj.pos.y)

let process_obj col context = 
  let obj = get_obj col in
  update_vel obj;
  update_pos obj 
  (*match col with
  | Player(t,s,o) ->

  | Enemy(t,s,o) ->
  | Item(t,s,o) ->
  | Block(t,s,o) ->
*)

(* Converts an origin based on the bottom left of the bounding box to the top
 * right of the sprite, to make it easier to place objects flush with the ground.*)
let normalize_origin pos (spr:Sprite.sprite) =
  let p = spr.params in
  let (box,boy) = p.bbox_offset and (_,bh) = p.bbox_size in
  pos.x <- pos.x -. box;
  pos.y <- pos.y -. (boy +. bh)

let normalize_pos pos (oldspr:Sprite.sprite) (newspr:Sprite.sprite) =
    let p1 = oldspr.params and p2 = newspr.params in
    let (box1,boy1) = p1.bbox_offset and (box2,boy2) = p2.bbox_offset in
    let (bw1,bh1) = p1.bbox_size and (bw2,bh2) = p2.bbox_size in
    pos.x <- pos.x +. (bw2 +. box2) -. (bw1 +. box1);
    pos.y <- pos.y +. (bh2 +. boy2) -. (bh1 +. boy1)


let collide_block dir obj =
  match dir with
  | North -> obj.vel.y <- 0.
  | South -> 
      obj.vel.y <- 0.;
      obj.grounded <- true;
      obj.jumping <- false;
  | East | West-> obj.vel.x <- 0.

let evolve_enemy typ spr obj = 
  match typ with
  | GKoopa -> failwith "todo"
  | RKoopa -> failwith "todo"
  | GKoopaShell -> failwith "todo"
  | RKoopaShell -> failwith "todo"
  | _ -> obj.kill <- true 

let process_collision dir c1 c2 =
  match (c1, c2, dir) with
  | (Player(s1,o1), Enemy(typ,s2,o2), North) -> 
      o1.jumping <- false;
      evolve_enemy typ s2 o2
  | (Player(s1,o1), Enemy(t2,s2,o2), _) -> o1.kill <- true
  | (Player(s1,o1), Item(t2,s2,o2), _) -> 
      o2.kill <- true (*& stuff happens to player*)
  | (Player(s1,o1), Block(t2,s2,o2), dir) -> 
      collide_block dir o1
  | (Enemy(t1,s1,o1), Player(s2,o2), South) -> 
      o1.kill <- true
  | (Enemy(t1,s1,o1), Player(s2,o2), _) -> 
      o2.kill <- true
  | (Enemy(t1,s1,o1), Enemy(t2,s2,o2), dir) ->
      begin match dir with
      | West | East -> 
          o1.vel.x <- ~-.(o1.vel.x); 
          o2.vel.x <- ~-.(o2.vel.x)
      | _ -> ()
      end
  | (Enemy(typ,s,obj), Block(typ2,s2,obj2), dir) -> collide_block dir obj 
  | (Item(typ,s,obj), Block(typ2,s2,obj2), dir) -> collide_block dir obj
  | (Item(typ,s,obj), Player(s2,obj2), _) -> obj.kill <- true (*& stuff happens to player*)
  (*| (Block(typ,s,obj), Player(s2,obj2), dir) -> collide_block dir obj2
  | (Block(typ,s,obj), Enemy(typ2,s2,obj2), dir) -> collide_block dir obj2
  | (Block(typ,s,obj), Item(typ2,s2,obj2), dir) -> collide_block dir obj2*)
  | (_, _, _) -> ()

let get_aabb obj  =
  let spr = ((get_sprite obj).params)  in
  let obj = get_obj obj in
  let (offx, offy) = spr.bbox_offset in
  let (box,boy) = (obj.pos.x+.offx,obj.pos.y+.offy) in
  let (sx,sy) = spr.bbox_size in
  {
    center = {x=(box+.sx/.2.);y=(boy+.sy/.2.)};
    half = {x=sx/.2.;y=sy/.2.};
  }

let check_collision o1 o2 =
  let b1 = get_aabb o1 and b2 = get_aabb o2 in
  let o1 = get_obj o1 and o2 = get_obj o2 in
  let vx = (b1.center.x) -. (b2.center.x) in
  let vy = (b1.center.y) -. (b2.center.y) in
  let hwidths = (b1.half.x) +. (b2.half.x) in
  let hheights = (b1.half.y) +. (b2.half.y) in
  Printf.printf "%f %f (%f %f) (%f %f) \n" vx vy o1.pos.x o1.pos.y o2.pos.x o2.pos.y;
  if abs_float vx < hwidths && abs_float vy < hheights then begin
    let ox = hwidths -. abs_float vx in
    let oy = hheights -. abs_float vy in
    Printf.printf "%f %f\n" ox oy;
    if ox >= oy then begin
      if vy > 0. then (o1.pos.y <- (o1.pos.y+.oy); print_endline "North"; Some North)
      else (o1.pos.y <- (o1.pos.y -. oy); Printf.printf "%f %f" o1.pos.y oy; print_endline "South"; Some South)
    end else begin
      if vx > 0. then (o1.pos.x <- o1.pos.x +.ox; print_endline "West"; Some West)
      else (o1.pos.x <- o1.pos.x -. ox; print_endline "East"; Some East)
    end
  end else None

let kill = function
  | _ -> []
