module reservation[Resource,User]

/* PURPOSE:   manage efficient use of resources */

sig User { }

sig Resource { }

sig State {
	available : set Resource,
	reservations : User -> set Resource
}

pred provide[r : Resource, s0 : State, s1 : State] {
	s1.available = s0.available + r
}

pred retract[r : Resource, s0 : State, s1 : State] {
	r in s0.available and no u0 : User | r in s0.reservations[u0]
	s1.available = s0.available - r
}

pred reserve[r : Resource, u : User, s0 : State, s1 : State, s2 : State] {
	r in s0.available
	s1.reservations[u] = s0.reservations[u] + r
	s2.available = s1.available - r
}

pred cancel[r : Resource, u : User, s0 : State, s1 : State, s2 : State] {
	r in u -> s0.reservations
	s1.reservations[u] = s0.reservations[u] - r
	s2.available = s1.available + r
}
