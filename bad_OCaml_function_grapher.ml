(*This are the import and setup of Graphics library*)
#load "graphics.cma";;
open Graphics;;
Graphics.open_graph " 400x400";;
Graphics.set_window_title "Bad OCaml function grapher";;

(*Draw the axis centered*)
let draw_axis () = 
	let x = size_x () and y = size_y () in
	draw_poly [|(x/2,0);(x/2,y)|];
	draw_poly [|(0,y/2);(x,y/2)|];;

(*No need to explain, plot the scale centerd on 0 with subdivision of 1 and in the interval [-nx, nx] and [-ny,ny]*)
let draw_scale nxx nyy =
	let nx = 2*nxx and ny = 2*nyy in
	let x = size_x () and y = size_y () in
	let subx = x/nx in
	for k = 1 to nx do
		draw_poly [|(subx*k,y/2-y/80);(subx*k,y/2+y/80)|];
	done;

    let suby = y/ny in
    for k = 1 to ny*2 do
            draw_poly [|(x/2-x/80,suby*k);(x/2+x/80,suby*k)|];
    done;;

(*Draws a simple dot grid, same argument as the scale*)
let draw_grid nxx nyy =
	let nx = 2*nxx and ny = 2*nyy in
	let x = size_x () and y = size_y () and suby = (size_y ())/(ny*2) and subx = (size_x ())/(nx*2) in
	for k = 0 to nx do
		for i = 0 to ny do
			draw_poly [|(x/nx * k,suby*2*i+5); (x/nx * k,suby*(2*i)-5)|];
		done;
	done;

	for k = 0 to ny do
		for i = 1 to nx do
			draw_poly [|(subx*2*i+5,y/ny * k); (suby*2*i-5,y/ny * k)|];
		done;
	done;;

(*Calculate the image of a function on a specific interval with a specific resolution n*)
let image_interval_function f xmin xmax n =
	let t = (Array.make n (0.,0.)) and
	subx = ((abs_float xmin +. abs_float xmax)/.(float_of_int n)) in
	for k = 0 to n-1 do
		t.(k) <- (xmin +. subx *. float_of_int k, f(xmin +. subx *. float_of_int k))
	done;
	t;;
		

(*This function was mainly usefull for debugging*)
let translate_float_arr_coord_aux range_pixel_x range_pixel_y  (x,y) =
	let center_x = (size_x ())/2 and center_y = (size_y ())/2 in
	((center_x + int_of_float (((float_of_int range_pixel_x) *. x))),
	center_y + int_of_float ((float_of_int range_pixel_y) *. y));;

(*Translate an array of the form [|(x1,f(x1));...;(xn,f(xn))|] into coordinates that can be plotted onto the screen*)
let translate_float_arr_coord t xmin xmax ymin ymax =
	let range_pixel_x = (size_x ())/((abs (xmin)) + (abs (xmax))) and
	range_pixel_y = (size_y ())/(abs (ymin) + abs (ymax)) and
	tab = (Array.make (Array.length t) (0,0)) in
	for k = 0 to (Array.length t)-1 do
		tab.(k) <- (translate_float_arr_coord_aux range_pixel_x range_pixel_y t.(k));
	done;
	tab;;


(*Plot a function on a specific interval, the window fo it to be somehow accurate needs to be square, will update that woon hopefully*)
let draw_func f n xmin xmax =
	let t = (image_interval_function f (float_of_int xmin) (float_of_int xmax) n) in
	draw_poly_line (translate_float_arr_coord t xmin xmax xmin xmax);;

