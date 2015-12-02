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

let friction = 0.7
let gravity = 0.6
let collid_objs = ref []
let last_time = ref 0.

let calc_fps t0 t1 =
  let delta = (t1 -. t0) /. 1000. in
  1. /. delta

let broad_cache = ref []
let broad_phase collid = 
  !broad_cache

let rec narrow_phase c cs =
  match cs with
  | [] -> ()
  | h::t ->
    let () = if not (equals c h) then begin match Object.check_collision c h with
    | None -> ()
    | Some dir -> 
        if (get_obj h).id <> (get_obj c).id 
        then Object.process_collision dir c h
    end in narrow_phase c t

let translate_keys () =
  let k = pressed_keys in
  let controls = ref [] in
  controls := 
    if k.left then (CLeft::!controls) else
    if k.right then (CRight::!controls) else
    if k.up then (CUp::!controls) else
    if k.down then (CDown::!controls)
    else !controls
    ; !controls

let update_if_player collid context =
  match collid with
  | Player(s,o) as p ->
      let keys = translate_keys () in
      begin match Object.update_player o keys context with
      | None -> p
      | Some new_spr -> Player(new_spr,o)
      end
  | _ as col -> col


let update_collidable (collid:Object.collidable) all_collids canvas = 
 (* TODO: optimize. Draw static elements only once *)
  let context = canvas##getContext (Dom_html._2d_) in
  let collid = update_if_player collid context in
  let obj = Object.get_obj collid in
  let spr = Object.get_sprite collid in
  if not obj.kill then begin
    let broad = broad_phase collid in
    Object.process_obj collid context;
    narrow_phase collid broad;
    Draw.render spr (obj.pos.x,obj.pos.y);
    Sprite.update_animation spr; (* return bool * variant *)
    if not obj.kill then (collid_objs := collid::!collid_objs)
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
