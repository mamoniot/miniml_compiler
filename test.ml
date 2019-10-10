let fun is_even n = if n = 0 then true else (is_odd (n-1))
    and is_odd  n = if n = 0 then false else (is_even (n-1))
in
	is_odd 8
end
