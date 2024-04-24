let prog_dir = "../../progs/" 
let serialize_program p = 
  Conceptual.AstCompiler.compile_program_to_ast p 
  |> Option.map (Conceptual.Semant.typecheck_prog p)
  |> Option.map snd
  |> Option.map (Conceptual.CodeGen.translate_program_to_ir)
  |> Option.map (List.map Conceptual.SerializeAlloy.serializeProgram) 
  |> Option.value ~default: ["Error"]
  
let (~/) file = prog_dir ^ file ^ ".con" 
let (!>>) p = ~/p |> serialize_program |> List.fold_left (fun _ s -> print_endline s) ()

let %test "Program found" =
  Sys.file_exists ~/ "todo"


let %expect_test "Serialization of Reservation Concept" =
  !>> "reservation";
  [%expect {|
    module reservation[User, Resource]

    /* PURPOSE:	manage efficient use of resources */
    /* LANGUAGE:	Alloy6 */


    one sig State {
    	var available : set Resource,
    	var reservations : User -> Resource
    }

    pred provide[r : Resource] {
    	no _r1 : Resource, _u0 : User | { r in _r1 and _u0->_r1 in (State.reservations) }
    	(State.available') = (State.available) + r
    	(State.reservations') = (State.reservations)
    }

    pred retract[r : Resource] {
    	(State.available') = (State.available) - r
    	(State.reservations') = (State.reservations)
    }

    pred reserve[u : User, r : Resource] {
    	r in (State.available)
    	(State.reservations') = (State.reservations) + u->r
    	(State.available') = (State.available) - r
    }

    pred cancel[u : User, r : Resource] {
    	r in u.(State.reservations)
    	(State.reservations') = (State.reservations) - u->r
    	(State.available') = (State.available) + r
    }

    pred use[u : User, r : Resource] {
    	r in u.(State.reservations)
    	(State.available') = (State.available)
    	(State.reservations') = (State.reservations)
    }

    pred _can_use [u : User, r : Resource] { r in u.(State.reservations) }
    pred _can_cancel [u : User, r : Resource] { r in u.(State.reservations) }
    pred _can_reserve [u : User, r : Resource] { r in (State.available) }
    pred _can_retract [r : Resource] {  }
    pred _can_provide [r : Resource] { no _r1 : Resource, _u0 : User | { r in _r1 and _u0->_r1 in (State.reservations) } }

    -------------------------------------------
    pred alloy_stutter {
    	available' = available
    	reservations' = reservations
    }

    fact alloy_behavior {
    	/* The initial state */
    	no available
    	no reservations

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some r : Resource | provide[r] or
    		some r : Resource | retract[r] or
    		some r : Resource, u : User | reserve[u, r] or
    		some r : Resource, u : User | cancel[u, r] or
    		some r : Resource, u : User | use[u, r]
    	)
    }

    enum Event { alloy_stutter, provide, retract, reserve, cancel, use }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _provide : Event -> Resource { provide -> { r : Resource | provide[r] } }
    fun _retract : Event -> Resource { retract -> { r : Resource | retract[r] } }
    fun _reserve : Event -> User -> Resource { reserve -> { u : User, r : Resource | reserve[u, r] } }
    fun _cancel : Event -> User -> Resource { cancel -> { u : User, r : Resource | cancel[u, r] } }
    fun _use : Event -> User -> Resource { use -> { u : User, r : Resource | use[u, r] } }

    fun events : set Event {
    	(_use + _cancel + _reserve).Resource.User + _alloy_stutter + (_retract + _provide).Resource
    }
    -------------------------------------------

    assert _principle0 {
    	all r : Resource, u : User | { always (
    		reserve[u, r] => after (
    			cancel[u, r] releases _can_use[u, r]
    		)
    	) }
    }
    check _principle0 for 3 |}]

let %expect_test "Serialization of Trash Concept" = 
  !>> "trash";
  [%expect {|
    module trash[Item]

    /* PURPOSE:	to allow undoing of deletions */
    /* LANGUAGE:	Alloy6 */


    one sig State {
    	var accessible : set Item,
    	var trashed : set Item
    }

    pred create[x : Item] {
    	not x in (State.accessible) + (State.trashed)
    	(State.accessible') = (State.accessible) + x
    	(State.trashed') = (State.trashed)
    }

    pred delete[x : Item] {
    	x in (State.accessible) and not x in (State.trashed)
    	(State.trashed') = (State.trashed) + x
    	(State.accessible') = (State.accessible) - x
    }

    pred restore[x : Item] {
    	x in (State.trashed)
    	(State.trashed') = (State.trashed) - x
    	(State.accessible') = (State.accessible) + x
    }

    pred empty[] {
    	(State.trashed) != none
    	(State.trashed') = none
    	(State.accessible') = (State.accessible)
    }

    pred _can_empty [] { (State.trashed) != none }
    pred _can_restore [x : Item] { x in (State.trashed) }
    pred _can_delete [x : Item] { x in (State.accessible) and not x in (State.trashed) }
    pred _can_create [x : Item] { not x in (State.accessible) + (State.trashed) }

    -------------------------------------------
    pred alloy_stutter {
    	accessible' = accessible
    	trashed' = trashed
    }

    fact alloy_behavior {
    	/* The initial state */
    	no accessible
    	no trashed

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some x : Item | create[x] or
    		some x : Item | delete[x] or
    		some x : Item | restore[x] or
    		empty[]
    	)
    }

    enum Event { alloy_stutter, create, delete, restore, empty }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _create : Event -> Item { create -> { x : Item | create[x] } }
    fun _delete : Event -> Item { delete -> { x : Item | delete[x] } }
    fun _restore : Event -> Item { restore -> { x : Item | restore[x] } }
    fun _empty : Event { { e : empty | empty } }

    fun events : set Event {
    	(_restore + _delete + _create).Item + (_empty + _alloy_stutter)
    }
    -------------------------------------------

    assert _principle0 {
    	all x : Item | { always (
    		delete[x] => after (
    			restore[x] => after (
    				x in (State.accessible)
    			)
    		)
    	) }
    }
    check _principle0 for 3

    assert _principle1 {
    	all x : Item | { always (
    		delete[x] => after (
    			empty[] => after (
    				not x in (State.accessible) + (State.trashed)
    			)
    		)
    	) }
    }
    check _principle1 for 3 |}]

let %expect_test "Serialization of Email Concept" = 
  !>> "email";
  [%expect {|
    module email

    /* PURPOSE:	communicate with private messages */
    /* LANGUAGE:	Alloy6 */

    sig Message {}
    sig Content {}

    one sig State {
    	var inbox : User -> set Message,
    	var from : Message -> User,
    	var to : Message -> User,
    	var content : Message -> Content
    }

    pred send[by : User, _for : User, m : Message, c : Content] {
    	not m in _for.(State.inbox)
    	(State.content') = ((State.content) - m->Content) + m->c
    	(State.to') = ((State.to) - m->User) + m->_for
    	(State.from') = ((State.from) - m->User) + m->by
    	(State.inbox') = (State.inbox)
    }

    pred receive[by : User, m : Message] {
    	not m in by.(State.inbox) and m.(State.to) = by
    	(State.inbox') = (State.inbox) + by->m
    	(State.from') = (State.from)
    	(State.to') = (State.to)
    	(State.content') = (State.content)
    }

    pred delete[u : User, m : Message] {
    	m in u.(State.inbox)
    	(State.content') = ((State.content) - m->Content)
    	(State.to') = ((State.to) - m->User)
    	(State.from') = ((State.from) - m->User)
    	(State.inbox') = (State.inbox) - u->m
    }

    pred _can_delete [u : User, m : Message] { m in u.(State.inbox) }
    pred _can_receive [by : User, m : Message] { not m in by.(State.inbox) and m.(State.to) = by }
    pred _can_send [by : User, _for : User, m : Message, c : Content] { not m in _for.(State.inbox) }

    -------------------------------------------
    pred alloy_stutter {
    	inbox' = inbox
    	from' = from
    	to' = to
    	content' = content
    }

    fact alloy_behavior {
    	/* The initial state */
    	no inbox
    	no from
    	no to
    	no content

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some c : Content, m : Message, _for, by : User | send[by, _for, m, c] or
    		some m : Message, by : User | receive[by, m] or
    		some m : Message, u : User | delete[u, m]
    	)
    }

    enum Event { alloy_stutter, send, receive, delete }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _send : Event -> User -> User -> Message -> Content { send -> { by : User, _for : User, m : Message, c : Content | send[by, _for, m, c] } }
    fun _receive : Event -> User -> Message { receive -> { by : User, m : Message | receive[by, m] } }
    fun _delete : Event -> User -> Message { delete -> { u : User, m : Message | delete[u, m] } }

    fun events : set Event {
    	(_delete + _receive).Message.User + _send.Content.Message.User.User + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all c : Content, m : Message, f, t : User | { always (
    		send[f, t, m, c] => after (
    			receive[t, m] => after (
    				m in t.(State.inbox) and m.(State.content) = c
    			)
    		)
    	) }
    }
    check _principle0 for 3 |}]


let %expect_test "Serialization of Label Concept" = 
  !>> "label";
  [%expect {|
    module label[Item]

    /* PURPOSE:	organize items into overlapping categories */
    /* LANGUAGE:	Alloy6 */

    sig Label {}

    one sig State {
    	var labels : Item -> set Label
    }

    pred affix[i : Item, l : Label] {
    	(State.labels') = (State.labels) + i->l
    }

    pred detach[i : Item, l : Label] {
    	(State.labels') = (State.labels) - i->l
    }

    fun find [l : Label] : Item {
    	l.~(State.labels)
    }

    pred clear[i : Item] {
    	(State.labels') = ((State.labels) - i->Label)
    }

    pred _can_clear [i : Item] {  }
    pred _can_find [l : Label] {  }
    pred _can_detach [i : Item, l : Label] {  }
    pred _can_affix [i : Item, l : Label] {  }

    -------------------------------------------
    pred alloy_stutter {
    	labels' = labels
    }

    fact alloy_behavior {
    	/* The initial state */
    	no labels

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some l : Label, i : Item | affix[i, l] or
    		some l : Label, i : Item | detach[i, l] or
    		some i : Item | clear[i]
    	)
    }

    enum Event { alloy_stutter, affix, detach, clear }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _affix : Event -> Item -> Label { affix -> { i : Item, l : Label | affix[i, l] } }
    fun _detach : Event -> Item -> Label { detach -> { i : Item, l : Label | detach[i, l] } }
    fun _clear : Event -> Item { clear -> { i : Item | clear[i] } }

    fun events : set Event {
    	(_detach + _affix).Label.Item + _clear.Item + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all l : Label, i : Item | { always (
    		affix[i, l] => after (
    			(detach[i, l] or clear[i]) releases i in find[l]
    		)
    	) }
    }
    check _principle0 for 3

    assert _principle1 {
    	all l : Label, i : Item | { always (
    		historically not affix[i, l] or detach[i, l] => after (
    			not i in find[l]
    		)
    	) }
    }
    check _principle1 for 3 |}]


let %expect_test "Serialization of Style Concept" =
  !>> "style"; 
  [%expect {|
    module style[Element, Format]

    /* PURPOSE:	easing consistent formatting of elements */
    /* LANGUAGE:	Alloy6 */

    sig Style {}

    one sig State {
    	var assigned : Element -> lone Style,
    	var defined : Style -> lone Format,
    	var format : Element -> lone Format
    }

    fact _state_format {
    	always (
    		(State.format) = (State.assigned).(State.defined)
    	)
    }

    pred assign[e : Element, s : Style] {
    	(State.assigned') = ((State.assigned) - e->Style) + e->s
    	(State.defined') = (State.defined)
    }

    pred define[s : Style, f : Format] {
    	(State.defined') = ((State.defined) - s->Format) + s->f
    	(State.assigned') = (State.assigned)
    }

    pred _can_define [s : Style, f : Format] {  }
    pred _can_assign [e : Element, s : Style] {  }

    -------------------------------------------
    pred alloy_stutter {
    	assigned' = assigned
    	defined' = defined
    	format' = format
    }

    fact alloy_behavior {
    	/* The initial state */
    	no assigned
    	no defined
    	no format

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some s : Style, e : Element | assign[e, s] or
    		some s : Style, f : Format | define[s, f]
    	)
    }

    enum Event { alloy_stutter, assign, define }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _assign : Event -> Element -> Style { assign -> { e : Element, s : Style | assign[e, s] } }
    fun _define : Event -> Style -> Format { define -> { s : Style, f : Format | define[s, f] } }

    fun events : set Event {
    	_define.Format.Style + _assign.Style.Element + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all s : Style, f1, f2 : Format, e1, e2 : Element | { always (
    		define[s, f1] => after (
    			assign[e1, s] => after (
    				assign[e2, s] => after (
    					define[s, f2] => after (
    						e1.(State.format) = f2 and e2.(State.format) = f2
    					)
    				)
    			)
    		)
    	) }
    }
    check _principle0 for 3 |}]

let %expect_test "Serialization of Todo Concept" =
  !>> "todo";
  [%expect {|
    module todo

    /* PURPOSE:	keep track of tasks */
    /* LANGUAGE:	Alloy6 */

    sig Task {}

    one sig State {
    	var done : set Task,
    	var pending : set Task
    }

    pred add[t : Task] {
    	not t in (State.done) + (State.pending)
    	(State.pending') = (State.pending) + t
    	(State.done') = (State.done)
    }

    pred delete[t : Task] {
    	t in (State.done) + (State.pending)
    	(State.pending') = (State.pending) - t
    	(State.done') = (State.done) - t
    }

    pred complete[t : Task] {
    	t in (State.pending)
    	(State.pending') = (State.pending) - t
    	(State.done') = (State.done) + t
    }

    pred _can_complete [t : Task] { t in (State.pending) }
    pred _can_delete [t : Task] { t in (State.done) + (State.pending) }
    pred _can_add [t : Task] { not t in (State.done) + (State.pending) }

    -------------------------------------------
    pred alloy_stutter {
    	done' = done
    	pending' = pending
    }

    fact alloy_behavior {
    	/* The initial state */
    	no done
    	no pending

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some t : Task | add[t] or
    		some t : Task | delete[t] or
    		some t : Task | complete[t]
    	)
    }

    enum Event { alloy_stutter, add, delete, complete }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _add : Event -> Task { add -> { t : Task | add[t] } }
    fun _delete : Event -> Task { delete -> { t : Task | delete[t] } }
    fun _complete : Event -> Task { complete -> { t : Task | complete[t] } }

    fun events : set Event {
    	(_complete + _delete + _add).Task + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all t : Task | { always (
    		add[t] => after (
    			(delete[t] or complete[t]) releases t in (State.pending)
    		)
    	) }
    }
    check _principle0 for 3

    assert _principle1 {
    	all t : Task | { always (
    		complete[t] => after (
    			delete[t] releases t in (State.done)
    		)
    	) }
    }
    check _principle1 for 3 |}]

let %expect_test "Serialization of Upvote Concept" =
  !>> "upvote";
  [%expect {|
    module upvote[Item, User]

    /* PURPOSE:	gauge user sentiment of items */
    /* LANGUAGE:	Alloy6 */


    one sig State {
    	var upvotes : Item -> set User,
    	var downvotes : Item -> set User
    }

    pred upvote[i : Item, u : User] {
    	not u in i.(State.upvotes)
    	(State.downvotes') = (State.downvotes) - i->u
    	(State.upvotes') = (State.upvotes) + i->u
    }

    pred downvote[i : Item, u : User] {
    	not u in i.(State.downvotes)
    	(State.downvotes') = (State.downvotes) + i->u
    	(State.upvotes') = (State.upvotes) - i->u
    }

    pred unvote[i : Item, u : User] {
    	u in i.((State.upvotes) + (State.downvotes))
    	(State.downvotes') = (State.downvotes) - i->u
    	(State.upvotes') = (State.upvotes) - i->u
    }

    fun count [i : Item] : Int {
    	integer/sub[#i.(State.upvotes), #i.(State.downvotes)]
    }

    pred _can_count [i : Item] {  }
    pred _can_unvote [i : Item, u : User] { u in i.((State.upvotes) + (State.downvotes)) }
    pred _can_downvote [i : Item, u : User] { not u in i.(State.downvotes) }
    pred _can_upvote [i : Item, u : User] { not u in i.(State.upvotes) }

    -------------------------------------------
    pred alloy_stutter {
    	upvotes' = upvotes
    	downvotes' = downvotes
    }

    fact alloy_behavior {
    	/* The initial state */
    	no upvotes
    	no downvotes

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some i : Item, u : User | upvote[i, u] or
    		some i : Item, u : User | downvote[i, u] or
    		some i : Item, u : User | unvote[i, u]
    	)
    }

    enum Event { alloy_stutter, upvote, downvote, unvote }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _upvote : Event -> Item -> User { upvote -> { i : Item, u : User | upvote[i, u] } }
    fun _downvote : Event -> Item -> User { downvote -> { i : Item, u : User | downvote[i, u] } }
    fun _unvote : Event -> Item -> User { unvote -> { i : Item, u : User | unvote[i, u] } }

    fun events : set Event {
    	(_unvote + _downvote + _upvote).User.Item + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all i : Item, u : User | { always (
    		(upvote[i, u] or downvote[i, u]) => after (
    			_can_unvote[i, u]
    		)
    	) }
    }
    check _principle0 for 3

    assert _principle1 {
    	all i : Item, u : User | { always (
    		upvote[i, u] => after (
    			not _can_upvote[i, u]
    		)
    	) }
    }
    check _principle1 for 3

    assert _principle2 {
    	all i : Item, u : User | { always (
    		downvote[i, u] => after (
    			not _can_downvote[i, u]
    		)
    	) }
    }
    check _principle2 for 3 |}]

let %expect_test "Serialization of Todo-Label App" =
  !>> "todo-label";
  [%expect {|
    module label[Item, Label]

    /* PURPOSE:	organize items into overlapping categories */
    /* LANGUAGE:	Alloy6 */


    one sig State {
    	var labels : Item -> set Label
    }

    pred affix[i : Item, l : Label] {
    	(State.labels') = (State.labels) + i->l
    }

    pred detach[i : Item, l : Label] {
    	(State.labels') = (State.labels) - i->l
    }

    fun find [l : Label] : Item {
    	l.~(State.labels)
    }

    pred clear[i : Item] {
    	(State.labels') = ((State.labels) - i->Label)
    }

    pred _can_clear [i : Item] {  }
    pred _can_find [l : Label] {  }
    pred _can_detach [i : Item, l : Label] {  }
    pred _can_affix [i : Item, l : Label] {  }

    -------------------------------------------
    pred alloy_stutter {
    	labels' = labels
    }

    fact alloy_behavior {
    	/* The initial state */
    	no labels

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some l : Label, i : Item | affix[i, l] or
    		some l : Label, i : Item | detach[i, l] or
    		some i : Item | clear[i]
    	)
    }

    enum Event { alloy_stutter, affix, detach, clear }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _affix : Event -> Item -> Label { affix -> { i : Item, l : Label | affix[i, l] } }
    fun _detach : Event -> Item -> Label { detach -> { i : Item, l : Label | detach[i, l] } }
    fun _clear : Event -> Item { clear -> { i : Item | clear[i] } }

    fun events : set Event {
    	(_detach + _affix).Label.Item + _clear.Item + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all l : Label, i : Item | { always (
    		affix[i, l] => after (
    			(detach[i, l] or clear[i]) releases i in find[l]
    		)
    	) }
    }
    check _principle0 for 3

    assert _principle1 {
    	all l : Label, i : Item | { always (
    		historically not affix[i, l] or detach[i, l] => after (
    			not i in find[l]
    		)
    	) }
    }
    check _principle1 for 3
    module todo

    /* PURPOSE:	keep track of tasks */
    /* LANGUAGE:	Alloy6 */

    sig Task {}

    one sig State {
    	var done : set Task,
    	var pending : set Task
    }

    pred add[t : Task] {
    	not t in (State.done) + (State.pending)
    	(State.pending') = (State.pending) + t
    	(State.done') = (State.done)
    }

    pred delete[t : Task] {
    	t in (State.done) + (State.pending)
    	(State.pending') = (State.pending) - t
    	(State.done') = (State.done) - t
    }

    pred complete[t : Task] {
    	t in (State.pending)
    	(State.pending') = (State.pending) - t
    	(State.done') = (State.done) + t
    }

    pred _can_complete [t : Task] { t in (State.pending) }
    pred _can_delete [t : Task] { t in (State.done) + (State.pending) }
    pred _can_add [t : Task] { not t in (State.done) + (State.pending) }

    -------------------------------------------
    pred alloy_stutter {
    	done' = done
    	pending' = pending
    }

    fact alloy_behavior {
    	/* The initial state */
    	no done
    	no pending

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some t : Task | add[t] or
    		some t : Task | delete[t] or
    		some t : Task | complete[t]
    	)
    }

    enum Event { alloy_stutter, add, delete, complete }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _add : Event -> Task { add -> { t : Task | add[t] } }
    fun _delete : Event -> Task { delete -> { t : Task | delete[t] } }
    fun _complete : Event -> Task { complete -> { t : Task | complete[t] } }

    fun events : set Event {
    	(_complete + _delete + _add).Task + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all t : Task | { always (
    		add[t] => after (
    			(delete[t] or complete[t]) releases t in (State.pending)
    		)
    	) }
    }
    check _principle0 for 3

    assert _principle1 {
    	all t : Task | { always (
    		complete[t] => after (
    			delete[t] releases t in (State.done)
    		)
    	) }
    }
    check _principle1 for 3
    module todo_label


    /* LANGUAGE:	Alloy6 */

    open todo
    open label[todo/Task, String]

    fact _state__sync_todo_delete0 {
    	always (
    		all t : todo/Task | { todo/delete[t] => label/clear[t] }
    	)
    }

    fact _state__sync_todo_add1 {
    	always (
    		all t : todo/Task | { todo/add[t] => label/affix[t, "pending"] }
    	)
    }

    fact _state__sync_todo_complete2 {
    	always (
    		all t : todo/Task | { todo/complete[t] => label/detach[t, "pending"] }
    	)
    }

    fact _state__sync_label_detach3 {
    	always (
    		all t : todo/Task | { label/detach[t, "pending"] => todo/complete[t] }
    	)
    }

    fact _state__sync_label_affix4 {
    	always (
    		all t : todo/Task | { label/affix[t, "pending"] => todo/add[t] }
    	)
    } |}]

let %expect_test "Serialization of Email-Label App" =
  !>> "todo-label-email";
  [%expect {|
    module email

    /* PURPOSE:	communicate with private messages */
    /* LANGUAGE:	Alloy6 */

    sig Message {}
    sig Content {}

    one sig State {
    	var inbox : User -> set Message,
    	var from : Message -> User,
    	var to : Message -> User,
    	var content : Message -> Content
    }

    pred send[by : User, t : User, m : Message, c : Content] {
    	not m in t.(State.inbox)
    	(State.content') = ((State.content) - m->Content) + m->c
    	(State.to') = ((State.to) - m->User) + m->t
    	(State.from') = ((State.from) - m->User) + m->by
    	(State.inbox') = (State.inbox)
    }

    pred receive[by : User, m : Message] {
    	not m in by.(State.inbox) and m.(State.to) = by
    	(State.inbox') = (State.inbox) + by->m
    	(State.from') = (State.from)
    	(State.to') = (State.to)
    	(State.content') = (State.content)
    }

    pred delete[u : User, m : Message] {
    	m in u.(State.inbox)
    	(State.content') = ((State.content) - m->Content)
    	(State.to') = ((State.to) - m->User)
    	(State.from') = ((State.from) - m->User)
    	(State.inbox') = (State.inbox) - u->m
    }

    pred _can_delete [u : User, m : Message] { m in u.(State.inbox) }
    pred _can_receive [by : User, m : Message] { not m in by.(State.inbox) and m.(State.to) = by }
    pred _can_send [by : User, t : User, m : Message, c : Content] { not m in t.(State.inbox) }

    -------------------------------------------
    pred alloy_stutter {
    	inbox' = inbox
    	from' = from
    	to' = to
    	content' = content
    }

    fact alloy_behavior {
    	/* The initial state */
    	no inbox
    	no from
    	no to
    	no content

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some c : Content, m : Message, by, t : User | send[by, t, m, c] or
    		some m : Message, by : User | receive[by, m] or
    		some m : Message, u : User | delete[u, m]
    	)
    }

    enum Event { alloy_stutter, send, receive, delete }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _send : Event -> User -> User -> Message -> Content { send -> { by : User, t : User, m : Message, c : Content | send[by, t, m, c] } }
    fun _receive : Event -> User -> Message { receive -> { by : User, m : Message | receive[by, m] } }
    fun _delete : Event -> User -> Message { delete -> { u : User, m : Message | delete[u, m] } }

    fun events : set Event {
    	(_delete + _receive).Message.User + _send.Content.Message.User.User + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all c : Content, m : Message, b, t : User | { always (
    		send[b, t, m, c] => after (
    			receive[t, m] => after (
    				m in t.(State.inbox) and m.(State.content) = c
    			)
    		)
    	) }
    }
    check _principle0 for 3
    module todo[Task]

    /* PURPOSE:	keep track of tasks */
    /* LANGUAGE:	Alloy6 */


    one sig State {
    	var done : set Task,
    	var pending : set Task
    }

    pred add[t : Task] {
    	not t in (State.done) + (State.pending)
    	(State.pending') = (State.pending) + t
    	(State.done') = (State.done)
    }

    pred delete[t : Task] {
    	t in (State.done) + (State.pending)
    	(State.pending') = (State.pending) - t
    	(State.done') = (State.done) - t
    }

    pred complete[t : Task] {
    	t in (State.pending)
    	(State.pending') = (State.pending) - t
    	(State.done') = (State.done) + t
    }

    pred _can_complete [t : Task] { t in (State.pending) }
    pred _can_delete [t : Task] { t in (State.done) + (State.pending) }
    pred _can_add [t : Task] { not t in (State.done) + (State.pending) }

    -------------------------------------------
    pred alloy_stutter {
    	done' = done
    	pending' = pending
    }

    fact alloy_behavior {
    	/* The initial state */
    	no done
    	no pending

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some t : Task | add[t] or
    		some t : Task | delete[t] or
    		some t : Task | complete[t]
    	)
    }

    enum Event { alloy_stutter, add, delete, complete }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _add : Event -> Task { add -> { t : Task | add[t] } }
    fun _delete : Event -> Task { delete -> { t : Task | delete[t] } }
    fun _complete : Event -> Task { complete -> { t : Task | complete[t] } }

    fun events : set Event {
    	(_complete + _delete + _add).Task + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all t : Task | { always (
    		add[t] => after (
    			(delete[t] or complete[t]) releases t in (State.pending)
    		)
    	) }
    }
    check _principle0 for 3

    assert _principle1 {
    	all t : Task | { always (
    		complete[t] => after (
    			delete[t] releases t in (State.done)
    		)
    	) }
    }
    check _principle1 for 3
    module label[Item]

    /* PURPOSE:	organize items into overlapping categories */
    /* LANGUAGE:	Alloy6 */


    one sig State {
    	var labels : Item -> set Label
    }

    pred affix[i : Item, l : Label] {
    	(State.labels') = (State.labels) + i->l
    }

    pred detach[i : Item, l : Label] {
    	(State.labels') = (State.labels) - i->l
    }

    fun find [l : Label] : Item {
    	l.~(State.labels)
    }

    pred clear[i : Item] {
    	(State.labels') = ((State.labels) - i->Label)
    }

    pred _can_clear [i : Item] {  }
    pred _can_find [l : Label] {  }
    pred _can_detach [i : Item, l : Label] {  }
    pred _can_affix [i : Item, l : Label] {  }

    -------------------------------------------
    pred alloy_stutter {
    	labels' = labels
    }

    fact alloy_behavior {
    	/* The initial state */
    	no labels

    	/* The state transitions */
    	always (
    		alloy_stutter or
    		some l : Label, i : Item | affix[i, l] or
    		some l : Label, i : Item | detach[i, l] or
    		some i : Item | clear[i]
    	)
    }

    enum Event { alloy_stutter, affix, detach, clear }

    fun _alloy_stutter : Event { { e : alloy_stutter | alloy_stutter } }
    fun _affix : Event -> Item -> Label { affix -> { i : Item, l : Label | affix[i, l] } }
    fun _detach : Event -> Item -> Label { detach -> { i : Item, l : Label | detach[i, l] } }
    fun _clear : Event -> Item { clear -> { i : Item | clear[i] } }

    fun events : set Event {
    	(_detach + _affix).Label.Item + _clear.Item + _alloy_stutter
    }
    -------------------------------------------

    assert _principle0 {
    	all l : Label, i : Item | { always (
    		affix[i, l] => after (
    			(detach[i, l] or clear[i]) releases i in find[l]
    		)
    	) }
    }
    check _principle0 for 3

    assert _principle1 {
    	all l : Label, i : Item | { always (
    		historically not affix[i, l] or detach[i, l] => after (
    			not i in find[l]
    		)
    	) }
    }
    check _principle1 for 3
    module todo_label_mail


    /* LANGUAGE:	Alloy6 */

    open todo[email/Content]
    open label[email/Content]
    open email

    fact _state__sync_todo_delete0 {
    	always (
    		all t : email/Content | { todo/delete[t] => label/clear[t] }
    	)
    }

    fact _state__sync_email_receive1 {
    	always (
    		some todo_user : email/User | { all m : email/Message | { email/receive[todo_user, m] => todo/add[m.(email/State.content)] } }
    	)
    } |}]
  

