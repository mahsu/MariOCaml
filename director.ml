open Object
open Actors

type keys = {
  mutable left: bool;
  mutable right: bool;
  mutable up: bool;
  mutable down: bool;
}

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

let translate_keys () =
  let k = pressed_keys in
  let ctrls = [(k.left,CLeft);(k.right,CRight);(k.up,CUp);(k.down,CDown)] in
  List.fold_left (fun a x -> if fst x then (snd x)::a else a) [] ctrls

let update_if_player collid context =
  match collid with
  | Player(t,s,o) as p ->
      let keys = translate_keys () in
      begin match Object.update_player o keys context with
      | None -> p
      | Some (new_typ, new_spr) -> Player(new_typ,new_spr,o)
      end
  | _ as col -> col

let check_collisions collid context =
  match collid with
  | Block(_,_,_) -> []
  | _ ->
    let broad = broad_phase collid in
    narrow_phase collid broad context

    let update_collidable (collid:Object.collidable) all_collids canvas =
 (* TODO: optimize. Draw static elements only once *)
  let context = canvas##getContext (Dom_html._2d_) in
  let collid = update_if_player collid context in
  let obj = Object.get_obj collid in
  let spr = Object.get_sprite collid in
  if not obj.kill then begin
    obj.grounded <- false;
    Object.process_obj obj;
    (* Run collision detection if moving object*)
    let evolved = check_collisions collid context in
    (* Render and update animation *)
    Draw.render spr (obj.pos.x,obj.pos.y);
    if obj.vel.x <> 0. || not (is_enemy collid) then Sprite.update_animation spr;
    if not obj.kill then (collid_objs := collid::(!collid_objs@evolved))
  end


let update_loop canvas objs =
    let rec update_helper time canvas objs  =
      collid_objs := [];

      let fps = calc_fps !last_time time in
      last_time := time;

      broad_cache := objs;

      Draw.clear_canvas canvas;
      List.iter (fun obj -> ignore (update_collidable obj objs canvas)) objs ;

      Draw.fps canvas fps;
      ignore Dom_html.window##requestAnimationFrame(
          Js.wrap_callback (fun (t:float) -> update_helper t canvas !collid_objs))

  in update_helper 0. canvas objs

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
