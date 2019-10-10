
(
let val fact = fn fact => fn x =>
if x > 1 then x*fact fact (x - 1) else 1
in
	print(fact fact 5)
end;
let val fib = fn fib => fn x =>
if x > 2 then fib fib (x - 2) + fib fib (x - 1) else 1
in
	print(fib fib 5)
end;
let val pow = fn pow => fn x => fn y =>
if y > 1 then (pow pow x (y - 1))*x else if y = 1 then x else 1
in
	print(pow pow 5 3)
end;
let val pow = fn pow => fn x => fn y =>
if y > 1 then (pow pow (x*x) (y/2))*(if y%2 = 1 then x else 1) else if y = 1 then x else 1
in
	print(pow pow 2 11)
end
)
