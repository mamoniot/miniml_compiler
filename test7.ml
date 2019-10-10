
let val pair = (fn x => fn y => fn is => if is then x else y) in
	let val fst = (fn p => p true) in
		let val snd = (fn p => p false) in
			let val p = pair 3 true in
				(print (snd p); print (fst p))
			end
		end
	end
end
