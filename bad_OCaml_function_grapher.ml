(*This are the import and setup of Graphics library*)
#load "graphics.cma";;
open Graphics;;
Graphics.open_graph " 800x800";;
Graphics.set_window_title "Bad OCaml function grapher";;

(*Draw the axis centered*)
let draw_axis () = 
	let x = size_x () and y = size_y () in
	draw_poly [|(x/2,0);(x/2,y)|];
	draw_poly [|(0,y/2);(x,y/2)|];;

(*No need to explain
Things that I might patch :
- the scale should be in the range [-nx, nx] and [-ny, ny], currently nx and ny represent the number of graduation.*)
let draw_scale nx ny =
	let x = size_x () and y = size_y () in
	let subx = x/nx in
	for k = 1 to nx*2 do
		draw_poly [|(subx*k,y/2-y/80);(subx*k,y/2+y/80)|];
	done;

        let suby = y/ny in
        for k = 1 to ny*2 do
                draw_poly [|(x/2-x/80,suby*k);(x/2+x/80,suby*k)|];
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

(*Translate an array of the form [|(x1,f(x1));...;(xn,f(xn))|] into coordinates that can be plotted onto the screen
Things that I might patch :
- Currently the x and y graduation of the axis does not corelate to the xmin xmax ymin ymax*)
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
