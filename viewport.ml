open Object

type viewport = {
  pos: Object.xy;
  v_dim: Object.xy;
  m_dim: Object.xy;
}

let make (vx,vy) (mx,my) = 
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
  x >= v_min_x && x <= v_max_x && y >= v_min_y && y<= v_max_y

let out_of_viewport_below v y = 
  let v_max_y = v.pos.y +. v.v_dim.y in
  y >= v_max_y

let coord_to_viewport viewport coord = 
  { 
    x = coord.x -. viewport.pos.x;
    y = coord.y -. viewport.pos.y;
  }

let update vpt ctr =
  let new_x = calc_viewport_point ctr.x vpt.v_dim.x vpt.m_dim.x in
  let new_y = calc_viewport_point ctr.y vpt.v_dim.y vpt.m_dim.y in
  let pos = {x = new_x; y = new_y} in
  {vpt with pos}

