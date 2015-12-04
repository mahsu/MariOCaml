open Sprite
open Object
open Actors
open Viewport
open Particle

(*keys represents the arrow keys pressed via boolean*)
type keys = {
  mutable left: bool;
  mutable right: bool;
  mutable up: bool;
  mutable down: bool;
}

(*st represents the state of the game. It includes a background sprite (e.g.,
 * (e.g., hills), a context (used for rendering onto the page), a viewport
 * (used for moving the player's "camera"), a score (which is kept track
 * throughout the game), coins (also kept track through the game),
 * a multiplier (used for when you kill multiple enemies before ever touching
 * the ground, as in the actual Super Mario), and a game_over bool (which
 * is only true when the game is over). *)
type st = {
  bgd: sprite;
  ctx: Dom_html.canvasRenderingContext2D Js.t;
  vpt: viewport;
  map: float;
  mutable score: int;
  mutable coins: int;
  mutable multiplier: int;
  mutable game_over: bool;
}

(*pressed_keys instantiates the keys.*)
let pressed_keys = {
  left = false;
  right = false;
  up = false;
  down = false;
}

(*collid_objs is a list of collidable objects that have to be checked. This
 *is used in a list, so must be a reference. last_time is used for calculating
 *fps. *)
let collid_objs = ref []
let particles = ref []
let last_time = ref 0.

(*game_over displays a black screen when you finish a game.*)
let game_over state =
  state.ctx##rect (0.,0.,512.,512.);
  state.ctx##fillStyle <- (Js.string "black");
  state.ctx##fill ();
  state.ctx##fillStyle <- (Js.string "white");
  state.ctx##font <- (Js.string "20px 'Press Start 2P'");
  state.ctx##fillText (Js.string ("You win!"), 180., 128.);
  failwith "Game over."

let game_loss state =
  state.ctx##rect (0.,0.,512.,512.);
  state.ctx##fillStyle <- (Js.string "black");
  state.ctx##fill ();
  state.ctx##fillStyle <- (Js.string "white");
  state.ctx##font <- (Js.string "20px 'Press Start 2P'");
  state.ctx##fillText (Js.string ("GAME OVER. You lose!"), 60., 128.);
  failwith "Game over."

let calc_fps t0 t1 =
  let delta = (t1 -. t0) /. 1000. in
  1. /. delta

let update_score state i =
  state.score <- state.score + i

(*player_attack_enemy is called for a player hitting an enemy from the north.
 *This causes the player to either kill the enemy or move the enemy, in the
 *case that the enemy is a shell. Invulnerability, jumping, and grounded
 *are used for fine tuning the movements.*)
let player_attack_enemy s1 o1 typ s2 o2 state context =
  o1.invuln <- invuln;
  o1.jumping <- false;
  o1.grounded <- true;
  begin match typ with
  | GKoopaShell | RKoopaShell ->
      let r2 = evolve_enemy o1.dir typ s2 o2 context in
      o1.vel.y <- ~-. dampen_jump;
      o1.pos.y <- o1.pos.y -. 5.;
      (None,r2)
  | _ ->
      dec_health o2;
      o1.invuln <- invuln;
      o1.vel.y <- ~-. dampen_jump;
      if state.multiplier = 8 then begin
        update_score state 800;
        o2.score <- 800;
        (None, evolve_enemy o1.dir typ s2 o2 context)
      end else begin
        let score = 100 * state.multiplier in
        update_score state score;
        o2.score <- score;
        state.multiplier <- state.multiplier * 2;
        (None,(evolve_enemy o1.dir typ s2 o2 context))
      end
  end

(*enemy_attack_player is used when an enemy kills a player.*)
let enemy_attack_player s1 (o1:Object.obj) t2 s2 (o2:Object.obj) context =
  o1.invuln <- invuln;
  begin match t2 with
  | GKoopaShell |RKoopaShell ->
      let r2 = if o2.vel.x = 0. then evolve_enemy o1.dir t2 s2 o2 context
              else (dec_health o1; None) in
      (None,r2)
  | _ -> dec_health o1; (None,None)
  end

(*In the case that two enemies collide, they are to reverse directions. However,
 *in the case that one or more of the two enemies is a koopa shell, then
 *the koopa shell kills the other enemy. *)
let col_enemy_enemy t1 s1 o1 t2 s2 o2 dir =
  begin match (t1, t2) with
  | (GKoopaShell, GKoopaShell)
  | (GKoopaShell, RKoopaShell)
  | (RKoopaShell, RKoopaShell)
  | (RKoopaShell, GKoopaShell) ->
      dec_health o1;
      dec_health o2;
      (None,None)
  | (RKoopaShell, _) | (GKoopaShell, _) -> if o1.vel.x = 0. then
      (rev_dir o2 t2 s2;
      (None,None) )
      else ( dec_health o2; (None,None) )
  | (_, RKoopaShell) | (_, GKoopaShell) -> if o2.vel.x = 0. then
      (rev_dir o1 t1 s1;
      (None,None) )
      else ( dec_health o1; (None,None) )
  | (_, _) ->
      begin match dir with
      | West | East ->
          rev_dir o1 t1 s1;
          rev_dir o2 t2 s2;
          (None,None)
      | _ -> (None,None)
      end
  end

let obj_at_pos dir (pos: xy) (collids: Object.collidable list) : Object.collidable list =
  match dir with
  | Left -> (List.filter (fun (col: Object.collidable) -> (get_obj col).pos.y = pos.y && (get_obj col).pos.x = pos.x -. 16.)
            collids)
  | _ -> (List.filter (fun (col: Object.collidable) -> (get_obj col).pos.y = pos.y && (get_obj col).pos.x = pos.x +. 16.) collids)

let is_block dir pos collids =
  match obj_at_pos dir pos collids with
  | [] -> false
  | [Block (_,_,_)] -> true
  | _ -> false

let is_rkoopa collid =
  match collid with
  | Enemy(RKoopa,_,_) -> true
  | _ -> false

(*Process collision is called to match each of the possible collisions that
 *may occur. *)
let process_collision dir c1 c2  state =
  let context = state.ctx in
  match (c1, c2, dir) with
  | (Player(_,s1,o1), Enemy(typ,s2,o2), South)
  | (Enemy(typ,s2,o2),Player(_,s1,o1), North) ->
      player_attack_enemy s1 o1 typ s2 o2 state context
  | (Player(_,s1,o1), Enemy(t2,s2,o2), _)
  | (Enemy(t2,s2,o2), Player(_,s1,o1), _) ->
      enemy_attack_player s1 o1 t2 s2 o2 context
  | (Player(_,s1,o1), Item(t2,s2,o2), _)
  | (Item(t2,s2,o2), Player(_,s1,o1), _) ->
      begin match t2 with
      | Mushroom -> dec_health o2; o1.health <- o1.health + 1;
                    o1.vel.x <- 0.; o1.vel.y <- 0.;
                    update_score state 1000; (None, None)
      | Coin -> state.coins <- state.coins + 1; dec_health o2;
          update_score state 100;
          Printf.printf "Coins: %d \n" state.coins; (None, None)
      | _ -> dec_health o2; update_score state 1000; (None, None)
      end
  | (Enemy(t1,s1,o1), Enemy(t2,s2,o2), dir) ->
      col_enemy_enemy t1 s1 o1 t2 s2 o2 dir
  | (Enemy(t,s1,o1), Block(typ2,s2,o2), East)
  | (Enemy(t,s1,o1), Block(typ2,s2,o2), West)->
    begin match (t,typ2) with
    | (RKoopaShell, Brick) | (GKoopaShell, Brick) ->
        dec_health o2;
        reverse_left_right o1;
        (None,None)
    | (RKoopaShell, QBlock typ) | (GKoopaShell, QBlock typ) ->
        ( let updated_block = evolve_block o2 context in
        let spawned_item = spawn_above o1.dir o2 typ context in
        collide_block dir o1; rev_dir o1 t s1;
        (Some updated_block, Some spawned_item) )
    | (_,_) ->
        rev_dir o1 t s1;
      (None,None)
    end
  | (Item(_,s1,o1), Block(typ2,s2,o2), East)
  | (Item(_,s1,o1), Block(typ2,s2,o2), West) ->
      reverse_left_right o1;
      (None, None)
  | (Enemy(_,s1,o1), Block(typ2,s2,o2), _)
  | (Item(_,s1,o1), Block(typ2,s2,o2), _) ->
      collide_block dir o1;
      (None, None)
  | (Player(t1,s1,o1), Block(t,s2,o2), North) ->
      begin match t with
      | QBlock typ ->
          let updated_block = evolve_block o2 context in
          let spawned_item = spawn_above o1.dir o2 typ context in
          collide_block dir o1;
          (Some spawned_item, Some updated_block)
      | Brick -> if t1 = BigM then (collide_block dir o1; dec_health o2; (None, None))
                 else (collide_block dir o1; (None,None))
      | Panel -> game_over state
      | _ -> collide_block dir o1; (None,None)
      end
  | (Player(_,s1,o1), Block(t,s2,o2), _) ->
    begin match t with
    | Panel -> game_over state
    | _ ->
        begin match dir with
        | South -> state.multiplier <- 1 ; collide_block dir o1; (None, None)
        | _ -> collide_block dir o1; (None, None)
        end
    end
  | (_, _, _) -> (None,None)

let broad_cache = ref []
let broad_phase collid =
  !broad_cache

(*narrow_phase of collision is used in order to continuously loop through
 *each of the collidable objects to constantly check if collisions are
 *occurring.*)
let rec narrow_phase c cs state =
  let rec narrow_helper c cs state acc =
    match cs with
    | [] -> acc
    | h::t ->
      let c_obj = get_obj c in
      let invuln = c_obj.invuln in
      let new_objs = if not (equals c h) && invuln <= 0 then
        begin match Object.check_collision c h with
        | None -> (None,None)
        | Some dir ->
          if (get_obj h).id <> c_obj.id
          then
            ( (if (if is_rkoopa c then
            begin match c_obj.dir with
            | Left -> is_block c_obj.dir {x= c_obj.pos.x -. 16.; y= c_obj.pos.y -. 27.} cs
            | _ -> is_block c_obj.dir {x= c_obj.pos.x +. 16.; y= c_obj.pos.y -. 27.} cs
            end else false) then rev_dir c_obj RKoopa (Object.get_sprite c) else
            ());
            process_collision dir c h state )
          else (None,None)
      end else (None,None) in
      let acc = match new_objs with
        | (None, Some o) -> o::acc
        | (Some o, None) -> o::acc
        | (Some o1, Some o2) -> o1::o2::acc
        | (None, None) -> acc
      in
      c_obj.invuln <- if invuln > 0 then invuln-1 else invuln;
      narrow_helper c t state acc
  in narrow_helper c cs state []

let check_collisions collid state =
  match collid with
  | Block(_,_,_) -> []
  | _ ->
    let broad = broad_phase collid in
    narrow_phase collid broad state

(*update_collidable is primarily used for updating animation*)
let update_collidable state (collid:Object.collidable) all_collids =
 (* TODO: optimize. Draw static elements only once *)
  let obj = Object.get_obj collid in
  let spr = Object.get_sprite collid in
  let viewport_filter = in_viewport state.vpt obj.pos || is_player collid ||
      out_of_viewport_below state.vpt obj.pos.y in
  if not obj.kill &&  viewport_filter then begin
    obj.grounded <- false;
    Object.process_obj obj state.map;
    (* Run collision detection if moving object*)
    let evolved = check_collisions collid state in
    (* Render and update animation *)
    let vpt_adj_xy = coord_to_viewport state.vpt obj.pos in
    Draw.render spr (vpt_adj_xy.x,vpt_adj_xy.y);
    if obj.vel.x <> 0. || not (is_enemy collid) then Sprite.update_animation spr;
    evolved
  end else []

let translate_keys () =
  let k = pressed_keys in
  let ctrls = [(k.left,CLeft);(k.right,CRight);(k.up,CUp);(k.down,CDown)] in
  List.fold_left (fun a x -> if fst x then (snd x)::a else a) [] ctrls

(*run_update is used to update all of the collidables at once. Primarily used
 *as a wrapper method.*)
let run_update_collid state collid all_collids =
  match collid with
  | Player(t,s,o) as p ->
      let keys = translate_keys () in
      o.crouch <- false;
      let player = begin match Object.update_player o keys state.ctx with
        | None -> p
        | Some (new_typ, new_spr) ->
            Object.normalize_pos o.pos s.params new_spr.params;
            Player(new_typ,new_spr,o)
      end in
      let evolved = update_collidable state player all_collids in
      collid_objs := !collid_objs @ evolved;
      player
  | _ ->
      let obj = get_obj collid in
      let evolved = update_collidable state collid all_collids in
      if not obj.kill then (collid_objs := collid::(!collid_objs@evolved));
      let new_parts = if obj.kill then Object.kill collid state.ctx else [] in
      particles := !particles @ new_parts;
      collid

let run_update_particle state part =
  Particle.process part;
  let x = part.pos.x -. state.vpt.pos.x and y = part.pos.y -. state.vpt.pos.y in
  Draw.render part.params.sprite (x,y);
  if not part.kill then particles := part :: !particles

(*update_loop is constantly being called to check for collisions and to
 *update each of the objects in the game.*)
let update_loop canvas pair =
  let ctx = canvas##getContext (Dom_html._2d_) in
  let cwidth = float_of_int canvas##width in
  let cheight = float_of_int canvas##height in
  let viewport = Viewport.make (cwidth,cheight) (cwidth +. 1600.,cheight) in
  let player = fst pair in
  let objs = snd pair in
  let state = {
      bgd = Sprite.make_bgd ctx;
      vpt = Viewport.update viewport (get_obj player).pos;
      ctx;
      score = 0;
      coins = 0;
      multiplier = 1;
      map = float_of_int canvas##height +. 500.;
      game_over = false;
  } in
  let rec update_helper time state player objs parts =
      if state.game_over = true then game_over state else begin
        collid_objs := [];
        particles := [];

        let fps = calc_fps !last_time time in
        last_time := time;

        broad_cache := objs;

        Draw.clear_canvas canvas;

        (* Parallax background *)
        let vpos_x_int = int_of_float (state.vpt.pos.x /. 5.)  in
        let bgd_width = int_of_float (fst state.bgd.params.frame_size) in
        Draw.draw_bgd state.bgd (float_of_int (vpos_x_int mod bgd_width));

        let player = run_update_collid state player objs in
        if (get_obj player).kill = true then game_loss state else
        (let state = {state with vpt = Viewport.update state.vpt (get_obj player).pos} in
        List.iter (fun obj -> ignore (run_update_collid state obj objs)) objs ;
        List.iter (fun part -> run_update_particle state part) parts;
        Draw.fps canvas fps;
        Draw.hud canvas state.score state.coins;
        ignore Dom_html.window##requestAnimationFrame(
        Js.wrap_callback (fun (t:float) -> update_helper t state player !collid_objs !particles)))
      end
  in update_helper 0. state player objs []

let keydown evt =
  let () = match evt##keyCode with
  | 38 | 32 -> pressed_keys.up <- true; print_endline  "Jump"
  | 39 -> pressed_keys.right <- true; print_endline "Right"
  | 37 -> pressed_keys.left <- true; print_endline "Left"
  | 40 -> pressed_keys.down <- true; print_endline "Crouch"
  | _ -> ()
  in Js._true

let keyup evt =
  let () = match evt##keyCode with
  | 38 | 32 -> pressed_keys.up <- false
  | 39 -> pressed_keys.right <- false
  | 37 -> pressed_keys.left <- false
  | 40 -> pressed_keys.down <- false
  | _ -> ()
  in Js._true
