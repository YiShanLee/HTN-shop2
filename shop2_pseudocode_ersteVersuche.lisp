#|
SHOP 2 - Pseudocode und erste Schreibversuche:
procedure SHOP2(s, T, D)
	P = the empty plan
	T0 ← {t ∈ T : no other task in T is constrained to precede t}
	loop
		if T = empty then return P
		nondeterministically choose any t € T0
		if t is a primitive task then
			A ← {(a, θ) : a is a ground instance of an operator in D, θ is a substitution that unifies {head(a), t}, and s satisfies a’s preconditions}
			if A = empty then return failure
			nondeterministically choose a pair (a, θ) ∈ A
			modify s by deleting del(a) and adding add(a)
			append a to P
			modify T by removing t and applying θ
			T0 ← {t ∈ T : no task in T is constrained to precede t}
		else
		M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
				pre(m) is true in s, and m and θ are as general as possible}
		if M = empty then return failure
		nondeterministically choose a pair (m, θ) ∈ M
		modify T by removing t, adding sub(m), constraining each task
				in sub(m) to precede the tasks that t preceded, and applying θ
		if sub(m) is not empty then
			T0 ← {t ∈ sub(m) : no task in T is constrained to precede t}
		else T0 ← {t ∈ T : no task in T is constrained to precede t}
	repeat
end SHOP2
|#