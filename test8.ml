
let fun error_if_int x = x andalso false in
	if error_if_int true andalso error_if_int 12 then
		()
	else
		not(error_if_int true) orelse error_if_int 12
end
