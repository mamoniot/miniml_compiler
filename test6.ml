
let fun factorial x = if x > 1 then x*factorial(x - 1) else 1 in
	(print (factorial 2); print (factorial 4); print (factorial 6); true)
end
