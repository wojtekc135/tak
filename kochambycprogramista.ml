open Printf

let width = 160
let height = 44
let z_buffer = Array.make (width * height) 0.0
let buffer = Array.make (width * height) ' '
let background_ascii_code = ' '
let k1 = 40.0
let distance_from_cam = 100.0
let horizontal_offset = ref 0.0

let r1 = 10.0
let r2 = 20.0
let theta_spacing = 0.07
let phi_spacing = 0.02

let a = ref 0.0
let b = ref 0.0
let increment_speed = 0.04

let luminance_chars = [|'.'; ','; '-'; '~'; ':'; ';'; '='; '!'; '*'; '#'; '$'; '@'|]

let calculate_for_torus theta phi =
  let cos_theta = cos theta
  and sin_theta = sin theta
  and cos_phi = cos phi
  and sin_phi = sin phi in

  let circle_x = r2 +. r1 *. cos_theta in
  let x = circle_x *. cos_phi in
  let y = circle_x *. sin_phi in
  let z = r1 *. sin_theta in

  let rotated_x = x *. cos !b -. z *. sin !b in
  let rotated_z = x *. sin !b +. z *. cos !b in
  let rotated_y = y *. cos !a -. rotated_z *. sin !a in
  let rotated_z = y *. sin !a +. rotated_z *. cos !a +. distance_from_cam in

  if rotated_z > 0.0 then
    let ooz = 1.0 /. rotated_z in
    let xp = int_of_float (float width /. 2.0 +. k1 *. ooz *. rotated_x *. 2.0) in
    let yp = int_of_float (float height /. 2.0 -. k1 *. ooz *. rotated_y) in

    if xp >= 0 && xp < width && yp >= 0 && yp < height then
      let idx = xp + (yp * width) in
      if idx >= 0 && idx < width * height && ooz > z_buffer.(idx) then (
        z_buffer.(idx) <- ooz;
        let luminance_index = min (Array.length luminance_chars - 1) (int_of_float (rotated_z /. 10.0)) in
        buffer.(idx) <- luminance_chars.(luminance_index)
      )

let () =
  while true do
    for i = 0 to (width * height) - 1 do
      buffer.(i) <- background_ascii_code;
      z_buffer.(i) <- 0.0
    done;

    let theta = ref 0.0 in
    while !theta < 6.28 do
      let phi = ref 0.0 in
      while !phi < 6.28 do
        calculate_for_torus !theta !phi;
        phi := !phi +. phi_spacing
      done;
      theta := !theta +. theta_spacing
    done;

    if Sys.os_type = "Win32" then ignore (Sys.command "cls") else ignore (Sys.command "clear");

    for k = 0 to (width * height) - 1 do
      printf "%c" buffer.(k);
      if (k mod width) = (width - 1) then printf "\n"
    done;
    flush stdout;

    a := !a +. increment_speed;
    b := !b +. increment_speed;
  done
