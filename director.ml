open Object

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

let calc_fps t0 t1 =
  let delta = (t1 -. t0) /. 1000. in
  1. /. delta

let update_loop canvas objs = 
  let loop_objs = ref [] in
  let last_time = ref 0. in
  let rec update_helper time canvas objs  = 
    loop_objs := [];
    
    let fps = calc_fps !last_time time in
    last_time := time;

    (*check for collisions *) 
    
    Draw.clear_canvas canvas;
    List.iter (fun obj -> ignore (update_operation canvas obj)) objs ;
    
    Draw.fps canvas fps;
    ignore Dom_html.window##requestAnimationFrame( 
        Js.wrap_callback (fun (t:float) -> update_helper t canvas !loop_objs))

  and update_operation canvas (obj:Object.obj) =
    (* TODO: optimize. Draw static elements only once *) 
    Draw.render obj;
    Sprite.update_animation obj.sprite; (* return bool * variant *)
    (* if bool *)
    loop_objs := if false = false then obj::!loop_objs else !loop_objs
  
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
