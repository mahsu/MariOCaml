open Sprite
open Object
open Actors

type keys = {
  mutable left: bool;
  mutable right: bool;
  mutable up: bool;
  mutable down: bool;
}

type viewport = {
  pos: Object.xy;
  v_dim: Object.xy;
  m_dim: Object.xy;
}

type st = {
  bgd: sprite;
}

let make_viewport (vx,vy) (mx,my) = 
  {
    pos = {x = 0.; y = 0.;};
    v_dim = {x = vx; y = vy};
    m_dim = {x = mx; y = my};
  }

let calc_viewport_point cc vc mc = 
  let vc_half = vc /. 2. in
  min ( max (cc -. vc_half) 0. ) ( min (mc -. vc) (abs_float(cc -. vc_half)) )

let in_viewport v pos = 
  let margin = 32. in
  let (v_min_x,v_max_x) = (v.pos.x -. margin, v.pos.x +. v.v_dim.x) in
  let (v_min_y,v_max_y) = (v.pos.y -. margin, v.pos.y +. v.v_dim.y) in
  let (x,y) = (pos.x, pos.y) in 
  let test = x >= v_min_x && x <= v_max_x && y >= v_min_y && y<= v_max_y in
  if test = false then Printf.printf "%f %f %f %f\n" x y v_max_x v_max_y;
  test

let coord_to_viewport viewport coord = 
  { 
    x = coord.x -. viewport.pos.x;
    y = coord.y -. viewport.pos.y;
  }

let update_viewport vpt ctr =
  let new_x = calc_viewport_point ctr.x vpt.v_dim.x vpt.m_dim.x in
  let new_y = calc_viewport_point ctr.y vpt.v_dim.y vpt.m_dim.y in
  let pos = {x = new_x; y = new_y} in
  {vpt with pos}

let pressed_keys = {
  left = false;
  right = false;
  up = false;
  down = false;
}

let collid_objs = ref []
let last_time = ref 0.

let end_game () =
  Dom_html.window##alert (Js.string "Game over!");
  failwith "Game over."

let calc_fps t0 t1 =
  let delta = (t1 -. t0) /. 1000. in
  1. /. delta

let broad_cache = ref []
let broad_phase collid =
  !broad_cache

let rec narrow_phase c cs context =
  let rec narrow_helper c cs context acc =
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
          then Object.process_collision dir c h context
          else (None,None)
      end else (None,None) in
      let acc = match new_objs with
        | (None, Some o) -> o::acc
        | (Some o, None) -> o::acc
        | (Some o1, Some o2) -> o1::o2::acc
        | (None, None) -> acc
      in
      c_obj.invuln <- if invuln > 0 then invuln-1 else invuln;
      narrow_helper c t context acc
  in narrow_helper c cs context []

let check_collisions collid context =
  match collid with
  | Block(_,_,_) -> []
  | _ ->
    let broad = broad_phase collid in
    narrow_phase collid broad context

let update_collidable vpt (collid:Object.collidable) all_collids context =
 (* TODO: optimize. Draw static elements only once *)
  let obj = Object.get_obj collid in
  let spr = Object.get_sprite collid in
  if not obj.kill && in_viewport vpt obj.pos then begin
    obj.grounded <- false;
    Object.process_obj obj;
    (* Run collision detection if moving object*)
    let evolved = check_collisions collid context in
    (* Render and update animation *)
    let vpt_adj_xy = coord_to_viewport vpt obj.pos in
    Draw.render spr (vpt_adj_xy.x,vpt_adj_xy.y);
    if obj.vel.x <> 0. || not (is_enemy collid) then Sprite.update_animation spr;
    evolved
  end else []

let translate_keys () =
  let k = pressed_keys in
  let ctrls = [(k.left,CLeft);(k.right,CRight);(k.up,CUp);(k.down,CDown)] in
  List.fold_left (fun a x -> if fst x then (snd x)::a else a) [] ctrls

let run_update viewport collid all_collids canvas =
  let context = canvas##getContext (Dom_html._2d_) in
  match collid with
  | Player(t,s,o) as p ->
      let keys = translate_keys () in
      let player = begin match Object.update_player o keys context with
        | None -> p
        | Some (new_typ, new_spr) -> Player(new_typ,new_spr,o)
      end in
      let evolved = update_collidable viewport player all_collids context in
      collid_objs := !collid_objs @ evolved;
      player
  | _ ->
      let obj = get_obj collid in 
      let evolved = update_collidable viewport collid all_collids context in
      if not obj.kill then (collid_objs := collid::(!collid_objs@evolved));
      collid

let update_loop canvas objs =
  let context = canvas##getContext (Dom_html._2d_) in
  let cwidth = float_of_int canvas##width in
  let cheight = float_of_int canvas##height in
  let viewport = make_viewport (cwidth,cheight) (cwidth +. 500.,cheight) in
  let player = Object.spawn (SPlayer(SmallM,Standing)) context (200.,32.) in
  let viewport = update_viewport viewport (get_obj player).pos in
  let state = {
      bgd = Sprite.make_bgd context;
  } in
  let rec update_helper time canvas viewport player objs  =
      collid_objs := [];

      let fps = calc_fps !last_time time in
      last_time := time;

      broad_cache := objs;

      Draw.clear_canvas canvas;
      Draw.draw_bgd state.bgd;
      let player = run_update viewport player objs canvas in
      let viewport = update_viewport viewport (get_obj player).pos in
      List.iter (fun obj -> ignore (run_update viewport obj objs canvas)) objs ;

      Draw.fps canvas fps;
      ignore Dom_html.window##requestAnimationFrame(
          Js.wrap_callback (fun (t:float) -> update_helper t canvas viewport player !collid_objs))

  in update_helper 0. canvas viewport player objs

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
