open Sprite
open Actors

(*Variables*)
let friction = 0.9
let gravity = 0.2
let max_y_vel = 4.5
let player_speed = 2.8
let player_jump = 5.7
let player_max_jump = -6.
let dampen_jump = 4.
let invuln = 60

type aabb = { center : xy; half : xy }
type obj_params = { has_gravity : bool; speed : float }

let id_counter = ref min_int

type obj = {
  params : obj_params;
  pos : xy;
  vel : xy;
  id : int;
  mutable jumping : bool;
  mutable grounded : bool;
  mutable dir : Actors.dir_1d;
  mutable invuln : int;
  mutable kill : bool;
  mutable health : int;
  mutable crouch : bool;
  mutable score : int;
}

type collidable =
  | Player of pl_typ * sprite * obj
  | Enemy of enemy_typ * sprite * obj
  | Item of item_typ * sprite * obj
  | Block of block_typ * sprite * obj

(*setup_obj is used to set gravity and speed, with default values true and 1.*)
let setup_obj ?g:(has_gravity = true) ?spd:(speed = 1.) () =
  { has_gravity; speed }

(* Sets an object's x velocity to the speed specified in its params based on
 * its direction *)
let set_vel_to_speed obj =
  let speed = obj.params.speed in
  match obj.dir with
  | Left -> obj.vel.x <- ~-.speed
  | Right -> obj.vel.x <- speed

(* The following make functions all set the objects' has_gravity and speed,
 * returning an [obj_params] that can be directly plugged into the [obj]
 * during creation. *)
let make_player () = setup_obj ~spd:player_speed ()

let make_item = function
  | Mushroom -> setup_obj ()
  | FireFlower -> setup_obj ()
  | Star -> setup_obj ()
  | Coin -> setup_obj ~g:false ()

let make_enemy = function
  | Goomba -> setup_obj ()
  | GKoopa -> setup_obj ()
  | RKoopa -> setup_obj ()
  | GKoopaShell -> setup_obj ~spd:3. ()
  | RKoopaShell -> setup_obj ~spd:3. ()

let make_block = function
  | QBlock _i -> setup_obj ~g:false ()
  | QBlockUsed -> setup_obj ~g:false ()
  | Brick -> setup_obj ~g:false ()
  | UnBBlock -> setup_obj ~g:false ()
  | Cloud -> setup_obj ~g:false ()
  | Panel -> setup_obj ~g:false ()
  | Ground -> setup_obj ~g:false ()

let make_type = function
  | SPlayer (_pt, _t) -> make_player ()
  | SEnemy t -> make_enemy t
  | SItem t -> make_item t
  | SBlock t -> make_block t

(*Used in object creation and to compare two objects.*)
let new_id () =
  id_counter := !id_counter + 1;
  !id_counter

(*Used to return a new sprite and object of a created spawnable object*)
let make ?(id = None) ?(dir = Left) spawnable context (posx, posy) =
  let spr = Sprite.make spawnable dir context in
  let params = make_type spawnable in
  let id = match id with None -> new_id () | Some n -> n in
  let obj =
    {
      params;
      pos = { x = posx; y = posy };
      vel = { x = 0.0; y = 0.0 };
      id;
      jumping = false;
      grounded = false;
      dir;
      invuln = 0;
      kill = false;
      health = 1;
      crouch = false;
      score = 0;
    }
  in
  (spr, obj)

(*spawn returns a new collidable*)
let spawn spawnable context (posx, posy) =
  let spr, obj = make spawnable context (posx, posy) in
  match spawnable with
  | SPlayer (typ, _t) -> Player (typ, spr, obj)
  | SEnemy t ->
      set_vel_to_speed obj;
      Enemy (t, spr, obj)
  | SItem t -> Item (t, spr, obj)
  | SBlock t -> Block (t, spr, obj)

(*Helper methods for getting sprites and objects from their collidables*)
let get_sprite = function
  | Player (_, s, _) | Enemy (_, s, _) | Item (_, s, _) | Block (_, s, _) -> s

let get_obj = function
  | Player (_, _, o) | Enemy (_, _, o) | Item (_, _, o) | Block (_, _, o) -> o

let is_player = function Player (_, _, _) -> true | _ -> false
let is_enemy = function Enemy (_, _, _) -> true | _ -> false
let equals col1 col2 = (get_obj col1).id = (get_obj col2).id

(*Matches the controls being used and updates each of the player's params.*)
let update_player_keys (player : obj) (controls : controls) : unit =
  let lr_acc = player.vel.x *. 0.2 in
  match controls with
  | CLeft ->
      if not player.crouch then (
        if player.vel.x > ~-.(player.params.speed) then
          player.vel.x <- player.vel.x -. (0.4 -. lr_acc);
        player.dir <- Left)
  | CRight ->
      if not player.crouch then (
        if player.vel.x < player.params.speed then
          player.vel.x <- player.vel.x +. (0.4 +. lr_acc);
        player.dir <- Right)
  | CUp ->
      if (not player.jumping) && player.grounded then (
        player.jumping <- true;
        player.grounded <- false;
        player.vel.y <-
          max
            (player.vel.y -. (player_jump +. (abs_float player.vel.x *. 0.25)))
            player_max_jump)
  | CDown ->
      if (not player.jumping) && player.grounded then player.crouch <- true

(*Used for sprite changing. If sprites change to different dimensions as a result
 *of some action, the new sprite must be normalized so that things aren't
 *jumpy*)
let normalize_pos pos (p1 : Sprite.sprite_params) (p2 : Sprite.sprite_params) =
  let box1, boy1 = p1.bbox_offset and box2, boy2 = p2.bbox_offset in
  let bw1, bh1 = p1.bbox_size and bw2, bh2 = p2.bbox_size in
  pos.x <- pos.x -. (bw2 +. box2) +. (bw1 +. box1);
  pos.y <- pos.y -. (bh2 +. boy2) +. (bh1 +. boy1)

(*Update player is constantly being called to check for if big or small
 *Mario sprites/collidables should be used.*)
let update_player player keys context =
  let prev_jumping = player.jumping in
  let prev_dir = player.dir and prev_vx = abs_float player.vel.x in
  List.iter (update_player_keys player) keys;
  let v = player.vel.x *. friction in
  let vel_damped = if abs_float v < 0.1 then 0. else v in
  player.vel.x <- vel_damped;
  let pl_typ = if player.health <= 1 then SmallM else BigM in
  if (not prev_jumping) && player.jumping then
    Some (pl_typ, Sprite.make (SPlayer (pl_typ, Jumping)) player.dir context)
  else if
    prev_dir <> player.dir
    || ((prev_vx = 0. && abs_float player.vel.x > 0.) && not player.jumping)
  then Some (pl_typ, Sprite.make (SPlayer (pl_typ, Running)) player.dir context)
  else if prev_dir <> player.dir && player.jumping && prev_jumping then
    Some (pl_typ, Sprite.make (SPlayer (pl_typ, Jumping)) player.dir context)
  else if player.vel.y = 0. && player.crouch then
    Some (pl_typ, Sprite.make (SPlayer (pl_typ, Crouching)) player.dir context)
  else if player.vel.y = 0. && player.vel.x = 0. then
    Some (pl_typ, Sprite.make (SPlayer (pl_typ, Standing)) player.dir context)
  else None

(*The following two helper methods update velocity and position of the player*)
let update_vel obj =
  if obj.grounded then obj.vel.y <- 0.
  else if obj.params.has_gravity then
    obj.vel.y <-
      min (obj.vel.y +. gravity +. (abs_float obj.vel.y *. 0.01)) max_y_vel

let update_pos obj =
  obj.pos.x <- obj.vel.x +. obj.pos.x;
  if obj.params.has_gravity then obj.pos.y <- obj.vel.y +. obj.pos.y

(*Calls two above helper functions to update velocity and position of player.*)
let process_obj obj mapy =
  update_vel obj;
  update_pos obj;
  if obj.pos.y > mapy then obj.kill <- true

(* Converts an origin based on the bottom left of the bounding box to the top
 * right of the sprite, to make it easier to place objects flush with the ground.*)
let normalize_origin pos (spr : Sprite.sprite) =
  let p = spr.params in
  let box, boy = p.bbox_offset and _, bh = p.bbox_size in
  pos.x <- pos.x -. box;
  pos.y <- pos.y -. (boy +. bh)

(*Checks upon collision of block and updates the values of the object.*)
let collide_block ?(check_x = true) dir obj =
  match dir with
  | North -> obj.vel.y <- -0.001
  | South ->
      obj.vel.y <- 0.;
      obj.grounded <- true;
      obj.jumping <- false
  | East | West -> if check_x then obj.vel.x <- 0.

(*Simple helper method that reverses the direction in question*)
let opposite_dir dir = match dir with Left -> Right | Right -> Left

(*Used for enemy-enemy collisions*)
let reverse_left_right obj =
  obj.vel.x <- ~-.(obj.vel.x);
  obj.dir <- opposite_dir obj.dir

(*Actually creates a new enemy and deletes the previous. The positions must be
 *normalized. This method is typically called when enemies are killed and a
 *new sprite must be used (i.e., koopa to koopa shell). *)
let evolve_enemy player_dir typ (spr : Sprite.sprite) obj context =
  match typ with
  | GKoopa ->
      let new_spr, new_obj =
        make ~dir:obj.dir (SEnemy GKoopaShell) context (obj.pos.x, obj.pos.y)
      in
      normalize_pos new_obj.pos spr.params new_spr.params;
      Some (Enemy (GKoopaShell, new_spr, new_obj))
  | RKoopa ->
      let new_spr, new_obj =
        make ~dir:obj.dir (SEnemy RKoopaShell) context (obj.pos.x, obj.pos.y)
      in
      normalize_pos new_obj.pos spr.params new_spr.params;
      Some (Enemy (RKoopaShell, new_spr, new_obj))
  | GKoopaShell | RKoopaShell ->
      obj.dir <- player_dir;
      if obj.vel.x <> 0. then obj.vel.x <- 0. else set_vel_to_speed obj;
      None
  | _ ->
      obj.kill <- true;
      None

(*Updates the direction of the sprite. *)
let rev_dir o t (s : sprite) =
  reverse_left_right o;
  let old_params = s.params in
  Sprite.transform_enemy t s o.dir;
  normalize_pos o.pos old_params s.params

(*Used for killing enemies, or to make big Mario into small Mario*)
let dec_health obj =
  let health = obj.health - 1 in
  if health = 0 then obj.kill <- true
  else if obj.invuln = 0 then obj.health <- health

(*Used for deleting a block and replacing it with a used block*)
let evolve_block obj context =
  dec_health obj;
  let new_spr, new_obj =
    make (SBlock QBlockUsed) context (obj.pos.x, obj.pos.y)
  in
  Block (QBlockUsed, new_spr, new_obj)

(*Used for making a small Mario into a Big Mario*)
let _evolve_player (spr : Sprite.sprite) obj context =
  let new_spr, new_obj =
    make (SPlayer (BigM, Standing)) context (obj.pos.x, obj.pos.y)
  in
  normalize_pos new_obj.pos spr.params new_spr.params;
  Player (BigM, new_spr, new_obj)

(*Used for spawning items above question mark blocks*)
let spawn_above player_dir obj typ context =
  let item = spawn (SItem typ) context (obj.pos.x, obj.pos.y) in
  let item_obj = get_obj item in
  item_obj.pos.y <- item_obj.pos.y -. snd (get_sprite item).params.frame_size;
  item_obj.dir <- opposite_dir player_dir;
  set_vel_to_speed item_obj;
  item

(*Used to get the bounding box.*)
let get_aabb obj =
  let spr = (get_sprite obj).params in
  let obj = get_obj obj in
  let offx, offy = spr.bbox_offset in
  let box, boy = (obj.pos.x +. offx, obj.pos.y +. offy) in
  let sx, sy = spr.bbox_size in
  {
    center = { x = box +. (sx /. 2.); y = boy +. (sy /. 2.) };
    half = { x = sx /. 2.; y = sy /. 2. };
  }

let col_bypass c1 c2 =
  let o1 = get_obj c1 and o2 = get_obj c2 in
  let ctypes =
    match (c1, c2) with
    | Item (_, _, _), Enemy (_, _, _)
    | Enemy (_, _, _), Item (_, _, _)
    | Item (_, _, _), Item (_, _, _) ->
        true
    | Player (_, _, o1), Enemy (_, _, _) ->
        if o1.invuln > 0 then true else false
    | _ -> false
  in
  o1.kill || o2.kill || ctypes

(*Used for checking if collisions occur. Compares half-widths and half-heights
 *and adjusts for when collisions do occur, by changing position so that
 *a second collision does not occur again immediately. This causes snapping.*)
let check_collision c1 c2 =
  let b1 = get_aabb c1 and b2 = get_aabb c2 in
  let o1 = get_obj c1 in
  if col_bypass c1 c2 then None
  else
    let vx = b1.center.x -. b2.center.x in
    let vy = b1.center.y -. b2.center.y in
    let hwidths = b1.half.x +. b2.half.x in
    let hheights = b1.half.y +. b2.half.y in
    if abs_float vx < hwidths && abs_float vy < hheights then
      let ox = hwidths -. abs_float vx in
      let oy = hheights -. abs_float vy in
      if ox >= oy then
        if vy > 0. then (
          o1.pos.y <- o1.pos.y +. oy;
          Some North)
        else (
          o1.pos.y <- o1.pos.y -. oy;
          Some South)
      else if vx > 0. then (
        o1.pos.x <- o1.pos.x +. ox;
        Some West)
      else (
        o1.pos.x <- o1.pos.x -. ox;
        Some East)
    else None

(*"Kills" the matched object by setting certain parameters for each.*)
let kill collid ctx =
  match collid with
  | Enemy (t, _s, o) ->
      let pos = (o.pos.x, o.pos.y) in
      let score =
        if o.score > 0 then [ Particle.make_score o.score pos ctx ] else []
      in
      let remains =
        match t with
        | Goomba -> [ Particle.make GoombaSquish pos ctx ]
        | _ -> []
      in
      score @ remains
  | Block (t, _s, o) -> (
      match t with
      | Brick ->
          let pos = (o.pos.x, o.pos.y) in
          let p1 =
            Particle.make ~vel:(-5., -5.) ~acc:(0., 0.2) BrickChunkL pos ctx
          in
          let p2 =
            Particle.make ~vel:(-3., -4.) ~acc:(0., 0.2) BrickChunkL pos ctx
          in
          let p3 =
            Particle.make ~vel:(3., -4.) ~acc:(0., 0.2) BrickChunkR pos ctx
          in
          let p4 =
            Particle.make ~vel:(5., -5.) ~acc:(0., 0.2) BrickChunkR pos ctx
          in
          [ p1; p2; p3; p4 ]
      | _ -> [])
  | Item (t, _s, o) -> (
      match t with
      | Mushroom -> [ Particle.make_score o.score (o.pos.x, o.pos.y) ctx ]
      | _ -> [])
  | _ -> []
