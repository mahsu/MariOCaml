open Object

let calc_fps t0 t1 =
  let delta = (t1 -. t0) /. 1000. in
  1. /. delta

let rec update_loop canvas objs = 
  let loop_objs = ref [] in
  let last_time = ref 0. in
  let rec update_helper time canvas objs  = 
    loop_objs := [];
    
    let fps = calc_fps !last_time time in
    last_time := time;
    Printf.printf "fps: %f \n" fps;

    (*check for collisions *) 
    
    Draw.clear_canvas canvas;
    List.iter (fun obj -> ignore (update_operation canvas obj)) objs ;
    
    ignore Dom_html.window##requestAnimationFrame( 
        Js.wrap_callback (fun (t:float) -> update_helper t canvas !loop_objs))

  and update_operation canvas (obj:Object.collidable_obj) =
    (* TODO: optimize. Draw static elements only once *) 
    Draw.render obj;
    Draw.update_animation obj.sprite; (* return bool * variant *)
    (* if bool *)
    loop_objs := if false = false then obj::!loop_objs else !loop_objs
  
  in update_helper 0. canvas objs

