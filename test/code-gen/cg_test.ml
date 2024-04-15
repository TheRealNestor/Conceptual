let pretty_ir_ast p = 
  Conceptual.AstCompiler.compile_program_to_ast p 
  |> Option.map (Conceptual.Semant.typecheck_prog p)
  |> Option.map snd
  |> Option.map (Conceptual.CodeGen.translate_program_to_ir)
  |> Option.map (List.map Conceptual.AlloyPretty.program_to_tree) 
  |> Option.map (List.map PrintBox_text.to_string)
  |> Option.value ~default: ["Error"]

let serialize_program p = 
  Conceptual.AstCompiler.compile_program_to_ast p 
  |> Option.map (Conceptual.Semant.typecheck_prog p)
  |> Option.map snd
  |> Option.map (Conceptual.CodeGen.translate_program_to_ir)
  |> Option.map (List.map Conceptual.SerializeAlloy.serializeProgram) 
  |> Option.value ~default: ["Error"]

let prog_dir = "../progs/" 
let (~/) file = prog_dir ^ file ^ ".con" 

let (!>) p =  ~/p |> pretty_ir_ast |> List.fold_left (fun _ s -> print_endline s) ()
let (!>>) p = ~/p |> serialize_program |> List.fold_left (fun _ s -> print_endline s) ()

let %expect_test "Reservation Concept Alloy AST" =
  !> "reservation";
  [%expect {| 
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mreservation[0m
    │ └─[36mParameters[0m
    │   ├─[33mUser[0m
    │   └─[33mResource[0m
    ├─[36mPurpose[0m
    │ └─[36m manage efficient use of resources[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mavailable[0m
    │ │   │ ├─[32mSet Resource[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mreservations[0m
    │ │     ├─[32mUser -> Resource[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mResource[0m
    │ │ ├─[36mNo Fields[0m
    │ └─[36mSignature[0m
    │   ├─[33mUser[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle0[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mr[0m
    │     │ │ └─[32mResource[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mu[0m
    │     │   └─[32mUser[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mreserve[0m
    │         │ └─[36mArgs[0m
    │         │   ├─[33mu[0m
    │         │   └─[33mr[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mBinop[0m
    │             ├─[34mRelease[0m
    │             ├─[36mCall[0m
    │             │ ├─[33m_can_use[0m
    │             │ └─[36mArgs[0m
    │             │   ├─[33mu[0m
    │             │   └─[33mr[0m
    │             └─[36mCall[0m
    │               ├─[33mcancel[0m
    │               └─[36mArgs[0m
    │                 ├─[33mu[0m
    │                 └─[33mr[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33mprovide[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mQuantifier[0m
      │ │   ├─[34mNo[0m
      │ │   ├─[36mVars[0m
      │ │   │ ├─[36mVar[0m
      │ │   │ │ ├─[33m_u0[0m
      │ │   │ │ └─[32mUser[0m
      │ │   │ └─[36mVar[0m
      │ │   │   ├─[33m_r1[0m
      │ │   │   └─[32mResource[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mAnd[0m
      │ │     ├─[36mBinop[0m
      │ │     │ ├─[34mIn[0m
      │ │     │ ├─[33mr[0m
      │ │     │ └─[33m_r1[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mIn[0m
      │ │       ├─[36mBinop[0m
      │ │       │ ├─[34mProduct[0m
      │ │       │ ├─[33m_u0[0m
      │ │       │ └─[33m_r1[0m
      │ │       └─[33m(State.reservations)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mr[0m
      │ │   └─[32mResource[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.available')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.available)[0m
      │   │   └─[33mr[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.reservations')[0m
      │     └─[33m(State.reservations)[0m
      ├─[36mPredicate[0m
      │ ├─[33mretract[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mAnd[0m
      │ │   ├─[36mBinop[0m
      │ │   │ ├─[34mIn[0m
      │ │   │ ├─[33mr[0m
      │ │   │ └─[33m(State.available)[0m
      │ │   └─[36mQuantifier[0m
      │ │     ├─[34mNo[0m
      │ │     ├─[36mVars[0m
      │ │     │ ├─[36mVar[0m
      │ │     │ │ ├─[33m_u0[0m
      │ │     │ │ └─[32mUser[0m
      │ │     │ └─[36mVar[0m
      │ │     │   ├─[33m_r1[0m
      │ │     │   └─[32mResource[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mAnd[0m
      │ │       ├─[36mBinop[0m
      │ │       │ ├─[34mIn[0m
      │ │       │ ├─[33mr[0m
      │ │       │ └─[33m_r1[0m
      │ │       └─[36mBinop[0m
      │ │         ├─[34mIn[0m
      │ │         ├─[36mBinop[0m
      │ │         │ ├─[34mProduct[0m
      │ │         │ ├─[33m_u0[0m
      │ │         │ └─[33m_r1[0m
      │ │         └─[33m(State.reservations)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mr[0m
      │ │   └─[32mResource[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.available')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mDifference[0m
      │   │   ├─[33m(State.available)[0m
      │   │   └─[33mr[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.reservations')[0m
      │     └─[33m(State.reservations)[0m
      ├─[36mPredicate[0m
      │ ├─[33mreserve[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mIn[0m
      │ │   ├─[33mr[0m
      │ │   └─[33m(State.available)[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mu[0m
      │ │ │ └─[32mUser[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mr[0m
      │ │   └─[32mResource[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.reservations')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.reservations)[0m
      │   │   └─[33mu->r[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.available')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.available)[0m
      │       └─[33mr[0m
      ├─[36mPredicate[0m
      │ ├─[33mcancel[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mIn[0m
      │ │   ├─[33mr[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mJoin[0m
      │ │     ├─[33mu[0m
      │ │     └─[33m(State.reservations)[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mu[0m
      │ │ │ └─[32mUser[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mr[0m
      │ │   └─[32mResource[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.reservations')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mDifference[0m
      │   │   ├─[33m(State.reservations)[0m
      │   │   └─[33mu->r[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.available')[0m
      │     └─[36mBinop[0m
      │       ├─[34mUnion[0m
      │       ├─[33m(State.available)[0m
      │       └─[33mr[0m
      └─[36mPredicate[0m
        ├─[33muse[0m
        ├─[36mCondition[0m
        │ └─[36mBinop[0m
        │   ├─[34mIn[0m
        │   ├─[33mr[0m
        │   └─[36mBinop[0m
        │     ├─[34mJoin[0m
        │     ├─[33mu[0m
        │     └─[33m(State.reservations)[0m
        ├─[36mParameters[0m
        │ ├─[36mParameter[0m
        │ │ ├─[33mu[0m
        │ │ └─[32mUser[0m
        │ └─[36mParameter[0m
        │   ├─[33mr[0m
        │   └─[32mResource[0m
        └─[36mBody[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.available')[0m
          │ └─[33m(State.available)[0m
          └─[36mAssignment[0m
            ├─[33m(State.reservations')[0m
            └─[33m(State.reservations)[0m |}]; 
  !>> "reservation";
  [%expect {|
    module reservation[User, Resource]

    /* PURPOSE:	 manage efficient use of resources */
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
    	r in (State.available) and no _r1 : Resource, _u0 : User | { r in _r1 and _u0->_r1 in (State.reservations) }
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
    pred _can_retract [r : Resource] { r in (State.available) and no _r1 : Resource, _u0 : User | { r in _r1 and _u0->_r1 in (State.reservations) } }
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

let %expect_test "Trash Concept Alloy AST" = 
  !> "trash";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mtrash[0m
    │ └─[36mParameters[0m
    │   └─[33mItem[0m
    ├─[36mPurpose[0m
    │ └─[36m  to allow undoing of deletions[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33maccessible[0m
    │ │   │ ├─[32mSet Item[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mtrashed[0m
    │ │     ├─[32mSet Item[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ └─[36mSignature[0m
    │   ├─[33mItem[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mx[0m
    │ │   │   └─[32mItem[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mdelete[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   └─[33mx[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mBinop[0m
    │ │           ├─[34mImplication[0m
    │ │           ├─[36mCall[0m
    │ │           │ ├─[33mrestore[0m
    │ │           │ └─[36mArgs[0m
    │ │           │   └─[33mx[0m
    │ │           └─[36mUnop[0m
    │ │             ├─[34mAfter[0m
    │ │             └─[36mBinop[0m
    │ │               ├─[34mIn[0m
    │ │               ├─[33mx[0m
    │ │               └─[33m(State.accessible)[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle1[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mx[0m
    │     │   └─[32mItem[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mdelete[0m
    │         │ └─[36mArgs[0m
    │         │   └─[33mx[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mBinop[0m
    │             ├─[34mImplication[0m
    │             ├─[36mCall[0m
    │             │ ├─[33mempty[0m
    │             │ └─[36mArgs[0m
    │             └─[36mUnop[0m
    │               ├─[34mAfter[0m
    │               └─[36mUnop[0m
    │                 ├─[34mNot[0m
    │                 └─[36mBinop[0m
    │                   ├─[34mIn[0m
    │                   ├─[33mx[0m
    │                   └─[36mBinop[0m
    │                     ├─[34mUnion[0m
    │                     ├─[33m(State.accessible)[0m
    │                     └─[33m(State.trashed)[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33mcreate[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mUnop[0m
      │ │   ├─[34mNot[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mIn[0m
      │ │     ├─[33mx[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mUnion[0m
      │ │       ├─[33m(State.accessible)[0m
      │ │       └─[33m(State.trashed)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mx[0m
      │ │   └─[32mItem[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.accessible')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.accessible)[0m
      │   │   └─[33mx[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.trashed')[0m
      │     └─[33m(State.trashed)[0m
      ├─[36mPredicate[0m
      │ ├─[33mdelete[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mAnd[0m
      │ │   ├─[36mBinop[0m
      │ │   │ ├─[34mIn[0m
      │ │   │ ├─[33mx[0m
      │ │   │ └─[33m(State.accessible)[0m
      │ │   └─[36mUnop[0m
      │ │     ├─[34mNot[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mIn[0m
      │ │       ├─[33mx[0m
      │ │       └─[33m(State.trashed)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mx[0m
      │ │   └─[32mItem[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.trashed')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.trashed)[0m
      │   │   └─[33mx[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.accessible')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.accessible)[0m
      │       └─[33mx[0m
      ├─[36mPredicate[0m
      │ ├─[33mrestore[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mIn[0m
      │ │   ├─[33mx[0m
      │ │   └─[33m(State.trashed)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mx[0m
      │ │   └─[32mItem[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.trashed')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mDifference[0m
      │   │   ├─[33m(State.trashed)[0m
      │   │   └─[33mx[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.accessible')[0m
      │     └─[36mBinop[0m
      │       ├─[34mUnion[0m
      │       ├─[33m(State.accessible)[0m
      │       └─[33mx[0m
      └─[36mPredicate[0m
        ├─[33mempty[0m
        ├─[36mCondition[0m
        │ └─[36mBinop[0m
        │   ├─[34mNeq[0m
        │   ├─[33m(State.trashed)[0m
        │   └─[34mNone[0m
        ├─[36mParameters[0m
        └─[36mBody[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.trashed')[0m
          │ └─[34mNone[0m
          └─[36mAssignment[0m
            ├─[33m(State.accessible')[0m
            └─[33m(State.accessible)[0m |}];
  !>> "trash";
  [%expect {|
    module trash[Item]

    /* PURPOSE:	  to allow undoing of deletions */
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

let %expect_test "Email Concept Alloy AST" = 
  !> "email";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33memail[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36m communicate with private messages [0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33minbox[0m
    │ │   │ ├─[32mUser -> Set Message[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mfrom[0m
    │ │   │ ├─[32mMessage -> User[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mto[0m
    │ │   │ ├─[32mMessage -> User[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mcontent[0m
    │ │     ├─[32mMessage -> Content[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mUser[0m
    │ │ ├─[36mNo Fields[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mContent[0m
    │ │ ├─[36mNo Fields[0m
    │ └─[36mSignature[0m
    │   ├─[33mMessage[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle0[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mc[0m
    │     │ │ └─[32mContent[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mm[0m
    │     │ │ └─[32mMessage[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mt[0m
    │     │ │ └─[32mUser[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mf[0m
    │     │   └─[32mUser[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33msend[0m
    │         │ └─[36mArgs[0m
    │         │   ├─[33mf[0m
    │         │   ├─[33mt[0m
    │         │   ├─[33mm[0m
    │         │   └─[33mc[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mBinop[0m
    │             ├─[34mImplication[0m
    │             ├─[36mCall[0m
    │             │ ├─[33mreceive[0m
    │             │ └─[36mArgs[0m
    │             │   ├─[33mt[0m
    │             │   └─[33mm[0m
    │             └─[36mUnop[0m
    │               ├─[34mAfter[0m
    │               └─[36mBinop[0m
    │                 ├─[34mAnd[0m
    │                 ├─[36mBinop[0m
    │                 │ ├─[34mIn[0m
    │                 │ ├─[33mm[0m
    │                 │ └─[36mBinop[0m
    │                 │   ├─[34mJoin[0m
    │                 │   ├─[33mt[0m
    │                 │   └─[33m(State.inbox)[0m
    │                 └─[36mBinop[0m
    │                   ├─[34mEq[0m
    │                   ├─[36mBinop[0m
    │                   │ ├─[34mJoin[0m
    │                   │ ├─[33mm[0m
    │                   │ └─[33m(State.content)[0m
    │                   └─[33mc[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33msend[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mUnop[0m
      │ │   ├─[34mNot[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mIn[0m
      │ │     ├─[33mm[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mJoin[0m
      │ │       ├─[33m_for[0m
      │ │       └─[33m(State.inbox)[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mby[0m
      │ │ │ └─[32mUser[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33m_for[0m
      │ │ │ └─[32mUser[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mm[0m
      │ │ │ └─[32mMessage[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mc[0m
      │ │   └─[32mContent[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.content')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[36mParenthesis[0m
      │   │   │ └─[36mBinop[0m
      │   │   │   ├─[34mDifference[0m
      │   │   │   ├─[33m(State.content)[0m
      │   │   │   └─[36mBinop[0m
      │   │   │     ├─[34mProduct[0m
      │   │   │     ├─[33mm[0m
      │   │   │     └─[33mContent[0m
      │   │   └─[33mm->c[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.to')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[36mParenthesis[0m
      │   │   │ └─[36mBinop[0m
      │   │   │   ├─[34mDifference[0m
      │   │   │   ├─[33m(State.to)[0m
      │   │   │   └─[36mBinop[0m
      │   │   │     ├─[34mProduct[0m
      │   │   │     ├─[33mm[0m
      │   │   │     └─[33mUser[0m
      │   │   └─[33mm->_for[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.from')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[36mParenthesis[0m
      │   │   │ └─[36mBinop[0m
      │   │   │   ├─[34mDifference[0m
      │   │   │   ├─[33m(State.from)[0m
      │   │   │   └─[36mBinop[0m
      │   │   │     ├─[34mProduct[0m
      │   │   │     ├─[33mm[0m
      │   │   │     └─[33mUser[0m
      │   │   └─[33mm->by[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.inbox')[0m
      │     └─[33m(State.inbox)[0m
      ├─[36mPredicate[0m
      │ ├─[33mreceive[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mAnd[0m
      │ │   ├─[36mUnop[0m
      │ │   │ ├─[34mNot[0m
      │ │   │ └─[36mBinop[0m
      │ │   │   ├─[34mIn[0m
      │ │   │   ├─[33mm[0m
      │ │   │   └─[36mBinop[0m
      │ │   │     ├─[34mJoin[0m
      │ │   │     ├─[33mby[0m
      │ │   │     └─[33m(State.inbox)[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mEq[0m
      │ │     ├─[36mBinop[0m
      │ │     │ ├─[34mJoin[0m
      │ │     │ ├─[33mm[0m
      │ │     │ └─[33m(State.to)[0m
      │ │     └─[33mby[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mby[0m
      │ │ │ └─[32mUser[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mm[0m
      │ │   └─[32mMessage[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.inbox')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.inbox)[0m
      │   │   └─[33mby->m[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.from')[0m
      │   │ └─[33m(State.from)[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.to')[0m
      │   │ └─[33m(State.to)[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.content')[0m
      │     └─[33m(State.content)[0m
      └─[36mPredicate[0m
        ├─[33mdelete[0m
        ├─[36mCondition[0m
        │ └─[36mBinop[0m
        │   ├─[34mIn[0m
        │   ├─[33mm[0m
        │   └─[36mBinop[0m
        │     ├─[34mJoin[0m
        │     ├─[33mu[0m
        │     └─[33m(State.inbox)[0m
        ├─[36mParameters[0m
        │ ├─[36mParameter[0m
        │ │ ├─[33mu[0m
        │ │ └─[32mUser[0m
        │ └─[36mParameter[0m
        │   ├─[33mm[0m
        │   └─[32mMessage[0m
        └─[36mBody[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.content')[0m
          │ └─[36mParenthesis[0m
          │   └─[36mBinop[0m
          │     ├─[34mDifference[0m
          │     ├─[33m(State.content)[0m
          │     └─[36mBinop[0m
          │       ├─[34mProduct[0m
          │       ├─[33mm[0m
          │       └─[33mContent[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.to')[0m
          │ └─[36mParenthesis[0m
          │   └─[36mBinop[0m
          │     ├─[34mDifference[0m
          │     ├─[33m(State.to)[0m
          │     └─[36mBinop[0m
          │       ├─[34mProduct[0m
          │       ├─[33mm[0m
          │       └─[33mUser[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.from')[0m
          │ └─[36mParenthesis[0m
          │   └─[36mBinop[0m
          │     ├─[34mDifference[0m
          │     ├─[33m(State.from)[0m
          │     └─[36mBinop[0m
          │       ├─[34mProduct[0m
          │       ├─[33mm[0m
          │       └─[33mUser[0m
          └─[36mAssignment[0m
            ├─[33m(State.inbox')[0m
            └─[36mBinop[0m
              ├─[34mDifference[0m
              ├─[33m(State.inbox)[0m
              └─[33mu->m[0m |}];
  !>> "email";
  [%expect {|
    module email

    /* PURPOSE:	 communicate with private messages  */
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

let %expect_test "Label Concept Alloy AST" =
  !> "label";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mlabel[0m
    │ └─[36mParameters[0m
    │   └─[33mItem[0m
    ├─[36mPurpose[0m
    │ └─[36m organize items into overlapping categories[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mlabels[0m
    │ │     ├─[32mItem -> Set Label[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mLabel[0m
    │ │ ├─[36mNo Fields[0m
    │ └─[36mSignature[0m
    │   ├─[33mItem[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ ├─[36mVar[0m
    │ │   │ │ ├─[33ml[0m
    │ │   │ │ └─[32mLabel[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mi[0m
    │ │   │   └─[32mItem[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33maffix[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   ├─[33mi[0m
    │ │       │   └─[33ml[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mBinop[0m
    │ │           ├─[34mRelease[0m
    │ │           ├─[36mBinop[0m
    │ │           │ ├─[34mIn[0m
    │ │           │ ├─[33mi[0m
    │ │           │ └─[36mCall[0m
    │ │           │   ├─[33mfind[0m
    │ │           │   └─[36mArgs[0m
    │ │           │     └─[33ml[0m
    │ │           └─[36mBinop[0m
    │ │             ├─[34mOr[0m
    │ │             ├─[36mCall[0m
    │ │             │ ├─[33mdetach[0m
    │ │             │ └─[36mArgs[0m
    │ │             │   ├─[33mi[0m
    │ │             │   └─[33ml[0m
    │ │             └─[36mCall[0m
    │ │               ├─[33mclear[0m
    │ │               └─[36mArgs[0m
    │ │                 └─[33mi[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle1[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33ml[0m
    │     │ │ └─[32mLabel[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mi[0m
    │     │   └─[32mItem[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mUnop[0m
    │         │ ├─[34mHistorically[0m
    │         │ └─[36mUnop[0m
    │         │   ├─[34mNot[0m
    │         │   └─[36mBinop[0m
    │         │     ├─[34mOr[0m
    │         │     ├─[36mCall[0m
    │         │     │ ├─[33maffix[0m
    │         │     │ └─[36mArgs[0m
    │         │     │   ├─[33mi[0m
    │         │     │   └─[33ml[0m
    │         │     └─[36mCall[0m
    │         │       ├─[33mdetach[0m
    │         │       └─[36mArgs[0m
    │         │         ├─[33mi[0m
    │         │         └─[33ml[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mUnop[0m
    │             ├─[34mNot[0m
    │             └─[36mBinop[0m
    │               ├─[34mIn[0m
    │               ├─[33mi[0m
    │               └─[36mCall[0m
    │                 ├─[33mfind[0m
    │                 └─[36mArgs[0m
    │                   └─[33ml[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33maffix[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ └─[36mBody[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.labels')[0m
      │     └─[36mBinop[0m
      │       ├─[34mUnion[0m
      │       ├─[33m(State.labels)[0m
      │       └─[33mi->l[0m
      ├─[36mPredicate[0m
      │ ├─[33mdetach[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ └─[36mBody[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.labels')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.labels)[0m
      │       └─[33mi->l[0m
      ├─[36mFunction[0m
      │ ├─[33mfind[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ ├─[32mItem[0m
      │ └─[36mBody[0m
      │   └─[36mBinop[0m
      │     ├─[34mJoin[0m
      │     ├─[33ml[0m
      │     └─[36mUnop[0m
      │       ├─[34mTilde[0m
      │       └─[33m(State.labels)[0m
      └─[36mPredicate[0m
        ├─[33mclear[0m
        ├─[36mCondition[0m
        │ └─[36mNone[0m
        ├─[36mParameters[0m
        │ └─[36mParameter[0m
        │   ├─[33mi[0m
        │   └─[32mItem[0m
        └─[36mBody[0m
          └─[36mAssignment[0m
            ├─[33m(State.labels')[0m
            └─[36mParenthesis[0m
              └─[36mBinop[0m
                ├─[34mDifference[0m
                ├─[33m(State.labels)[0m
                └─[36mBinop[0m
                  ├─[34mProduct[0m
                  ├─[33mi[0m
                  └─[33mLabel[0m |}];
  !>> "label";
  [%expect {|
    module label[Item]

    /* PURPOSE:	 organize items into overlapping categories */
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

let %expect_test "Style Concept Alloy AST" = 
  !> "style";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mstyle[0m
    │ └─[36mParameters[0m
    │   ├─[33mElement[0m
    │   └─[33mFormat[0m
    ├─[36mPurpose[0m
    │ └─[36m   easing consistent formatting of elements [0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33massigned[0m
    │ │   │ ├─[32mElement -> Lone Style[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mdefined[0m
    │ │   │ ├─[32mStyle -> Lone Format[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mformat[0m
    │ │     ├─[32mElement -> Lone Format[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mStyle[0m
    │ │ ├─[36mNo Fields[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mFormat[0m
    │ │ ├─[36mNo Fields[0m
    │ └─[36mSignature[0m
    │   ├─[33mElement[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    │ └─[36mFact[0m
    │   ├─[33mformat[0m
    │   └─[36mUnop[0m
    │     ├─[34mAlways[0m
    │     └─[36mAssignment[0m
    │       ├─[33m(State.format)[0m
    │       └─[36mBinop[0m
    │         ├─[34mJoin[0m
    │         ├─[33m(State.assigned)[0m
    │         └─[33m(State.defined)[0m
    ├─[36mAssertions[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle0[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mf2[0m
    │     │ │ └─[32mFormat[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33me2[0m
    │     │ │ └─[32mElement[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33me1[0m
    │     │ │ └─[32mElement[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mf1[0m
    │     │ │ └─[32mFormat[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33ms[0m
    │     │   └─[32mStyle[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mdefine[0m
    │         │ └─[36mArgs[0m
    │         │   ├─[33ms[0m
    │         │   └─[33mf1[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mBinop[0m
    │             ├─[34mImplication[0m
    │             ├─[36mCall[0m
    │             │ ├─[33massign[0m
    │             │ └─[36mArgs[0m
    │             │   ├─[33me1[0m
    │             │   └─[33ms[0m
    │             └─[36mUnop[0m
    │               ├─[34mAfter[0m
    │               └─[36mBinop[0m
    │                 ├─[34mImplication[0m
    │                 ├─[36mCall[0m
    │                 │ ├─[33massign[0m
    │                 │ └─[36mArgs[0m
    │                 │   ├─[33me2[0m
    │                 │   └─[33ms[0m
    │                 └─[36mUnop[0m
    │                   ├─[34mAfter[0m
    │                   └─[36mBinop[0m
    │                     ├─[34mImplication[0m
    │                     ├─[36mCall[0m
    │                     │ ├─[33mdefine[0m
    │                     │ └─[36mArgs[0m
    │                     │   ├─[33ms[0m
    │                     │   └─[33mf2[0m
    │                     └─[36mUnop[0m
    │                       ├─[34mAfter[0m
    │                       └─[36mBinop[0m
    │                         ├─[34mAnd[0m
    │                         ├─[36mBinop[0m
    │                         │ ├─[34mEq[0m
    │                         │ ├─[36mBinop[0m
    │                         │ │ ├─[34mJoin[0m
    │                         │ │ ├─[33me1[0m
    │                         │ │ └─[33m(State.format)[0m
    │                         │ └─[33mf2[0m
    │                         └─[36mBinop[0m
    │                           ├─[34mEq[0m
    │                           ├─[36mBinop[0m
    │                           │ ├─[34mJoin[0m
    │                           │ ├─[33me2[0m
    │                           │ └─[33m(State.format)[0m
    │                           └─[33mf2[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33massign[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33me[0m
      │ │ │ └─[32mElement[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ms[0m
      │ │   └─[32mStyle[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.assigned')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[36mParenthesis[0m
      │   │   │ └─[36mBinop[0m
      │   │   │   ├─[34mDifference[0m
      │   │   │   ├─[33m(State.assigned)[0m
      │   │   │   └─[36mBinop[0m
      │   │   │     ├─[34mProduct[0m
      │   │   │     ├─[33me[0m
      │   │   │     └─[33mStyle[0m
      │   │   └─[33me->s[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.defined')[0m
      │     └─[33m(State.defined)[0m
      └─[36mPredicate[0m
        ├─[33mdefine[0m
        ├─[36mCondition[0m
        │ └─[36mNone[0m
        ├─[36mParameters[0m
        │ ├─[36mParameter[0m
        │ │ ├─[33ms[0m
        │ │ └─[32mStyle[0m
        │ └─[36mParameter[0m
        │   ├─[33mf[0m
        │   └─[32mFormat[0m
        └─[36mBody[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.defined')[0m
          │ └─[36mBinop[0m
          │   ├─[34mUnion[0m
          │   ├─[36mParenthesis[0m
          │   │ └─[36mBinop[0m
          │   │   ├─[34mDifference[0m
          │   │   ├─[33m(State.defined)[0m
          │   │   └─[36mBinop[0m
          │   │     ├─[34mProduct[0m
          │   │     ├─[33ms[0m
          │   │     └─[33mFormat[0m
          │   └─[33ms->f[0m
          └─[36mAssignment[0m
            ├─[33m(State.assigned')[0m
            └─[33m(State.assigned)[0m |}];
  !>> "style"; 
  [%expect {|
    module style[Element, Format]

    /* PURPOSE:	   easing consistent formatting of elements  */
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
let %expect_test "Todo Concept Alloy AST" = 
  !> "todo";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mtodo[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36m keep track of tasks[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mdone[0m
    │ │   │ ├─[32mSet Task[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mpending[0m
    │ │     ├─[32mSet Task[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ └─[36mSignature[0m
    │   ├─[33mTask[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mt[0m
    │ │   │   └─[32mTask[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33madd[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   └─[33mt[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mBinop[0m
    │ │           ├─[34mRelease[0m
    │ │           ├─[36mBinop[0m
    │ │           │ ├─[34mIn[0m
    │ │           │ ├─[33mt[0m
    │ │           │ └─[33m(State.pending)[0m
    │ │           └─[36mBinop[0m
    │ │             ├─[34mOr[0m
    │ │             ├─[36mCall[0m
    │ │             │ ├─[33mdelete[0m
    │ │             │ └─[36mArgs[0m
    │ │             │   └─[33mt[0m
    │ │             └─[36mCall[0m
    │ │               ├─[33mcomplete[0m
    │ │               └─[36mArgs[0m
    │ │                 └─[33mt[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle1[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mt[0m
    │     │   └─[32mTask[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mcomplete[0m
    │         │ └─[36mArgs[0m
    │         │   └─[33mt[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mBinop[0m
    │             ├─[34mRelease[0m
    │             ├─[36mBinop[0m
    │             │ ├─[34mIn[0m
    │             │ ├─[33mt[0m
    │             │ └─[33m(State.done)[0m
    │             └─[36mCall[0m
    │               ├─[33mdelete[0m
    │               └─[36mArgs[0m
    │                 └─[33mt[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33madd[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mUnop[0m
      │ │   ├─[34mNot[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mIn[0m
      │ │     ├─[33mt[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mUnion[0m
      │ │       ├─[33m(State.done)[0m
      │ │       └─[33m(State.pending)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mt[0m
      │ │   └─[32mTask[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.pending')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.pending)[0m
      │   │   └─[33mt[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.done')[0m
      │     └─[33m(State.done)[0m
      ├─[36mPredicate[0m
      │ ├─[33mdelete[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mIn[0m
      │ │   ├─[33mt[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mUnion[0m
      │ │     ├─[33m(State.done)[0m
      │ │     └─[33m(State.pending)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mt[0m
      │ │   └─[32mTask[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.pending')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mDifference[0m
      │   │   ├─[33m(State.pending)[0m
      │   │   └─[33mt[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.done')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.done)[0m
      │       └─[33mt[0m
      └─[36mPredicate[0m
        ├─[33mcomplete[0m
        ├─[36mCondition[0m
        │ └─[36mBinop[0m
        │   ├─[34mIn[0m
        │   ├─[33mt[0m
        │   └─[33m(State.pending)[0m
        ├─[36mParameters[0m
        │ └─[36mParameter[0m
        │   ├─[33mt[0m
        │   └─[32mTask[0m
        └─[36mBody[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.pending')[0m
          │ └─[36mBinop[0m
          │   ├─[34mDifference[0m
          │   ├─[33m(State.pending)[0m
          │   └─[33mt[0m
          └─[36mAssignment[0m
            ├─[33m(State.done')[0m
            └─[36mBinop[0m
              ├─[34mUnion[0m
              ├─[33m(State.done)[0m
              └─[33mt[0m |}];
  !>> "todo";
  [%expect {|
    module todo

    /* PURPOSE:	 keep track of tasks */
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
let %expect_test "Upvote Concept Alloy AST" = 
  !> "upvote";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mupvote[0m
    │ └─[36mParameters[0m
    │   ├─[33mItem[0m
    │   └─[33mUser[0m
    ├─[36mPurpose[0m
    │ └─[36m track relative popularity of items[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mupvotes[0m
    │ │   │ ├─[32mItem -> Set User[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mdownvotes[0m
    │ │     ├─[32mItem -> Set User[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mUser[0m
    │ │ ├─[36mNo Fields[0m
    │ └─[36mSignature[0m
    │   ├─[33mItem[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ ├─[36mVar[0m
    │ │   │ │ ├─[33mu[0m
    │ │   │ │ └─[32mUser[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mi[0m
    │ │   │   └─[32mItem[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mBinop[0m
    │ │       │ ├─[34mOr[0m
    │ │       │ ├─[36mCall[0m
    │ │       │ │ ├─[33mupvote[0m
    │ │       │ │ └─[36mArgs[0m
    │ │       │ │   ├─[33mi[0m
    │ │       │ │   └─[33mu[0m
    │ │       │ └─[36mCall[0m
    │ │       │   ├─[33mdownvote[0m
    │ │       │   └─[36mArgs[0m
    │ │       │     ├─[33mi[0m
    │ │       │     └─[33mu[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mCall[0m
    │ │           ├─[33m_can_unvote[0m
    │ │           └─[36mArgs[0m
    │ │             ├─[33mi[0m
    │ │             └─[33mu[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle1[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ ├─[36mVar[0m
    │ │   │ │ ├─[33mu[0m
    │ │   │ │ └─[32mUser[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mi[0m
    │ │   │   └─[32mItem[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mupvote[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   ├─[33mi[0m
    │ │       │   └─[33mu[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mUnop[0m
    │ │           ├─[34mNot[0m
    │ │           └─[36mCall[0m
    │ │             ├─[33m_can_upvote[0m
    │ │             └─[36mArgs[0m
    │ │               ├─[33mi[0m
    │ │               └─[33mu[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle2[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mu[0m
    │     │ │ └─[32mUser[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mi[0m
    │     │   └─[32mItem[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mdownvote[0m
    │         │ └─[36mArgs[0m
    │         │   ├─[33mi[0m
    │         │   └─[33mu[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mUnop[0m
    │             ├─[34mNot[0m
    │             └─[36mCall[0m
    │               ├─[33m_can_downvote[0m
    │               └─[36mArgs[0m
    │                 ├─[33mi[0m
    │                 └─[33mu[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33mupvote[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mUnop[0m
      │ │   ├─[34mNot[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mIn[0m
      │ │     ├─[33mu[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mJoin[0m
      │ │       ├─[33mi[0m
      │ │       └─[33m(State.upvotes)[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mu[0m
      │ │   └─[32mUser[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.downvotes')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mDifference[0m
      │   │   ├─[33m(State.downvotes)[0m
      │   │   └─[33mi->u[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.upvotes')[0m
      │     └─[36mBinop[0m
      │       ├─[34mUnion[0m
      │       ├─[33m(State.upvotes)[0m
      │       └─[33mi->u[0m
      ├─[36mPredicate[0m
      │ ├─[33mdownvote[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mUnop[0m
      │ │   ├─[34mNot[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mIn[0m
      │ │     ├─[33mu[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mJoin[0m
      │ │       ├─[33mi[0m
      │ │       └─[33m(State.downvotes)[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mu[0m
      │ │   └─[32mUser[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.downvotes')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.downvotes)[0m
      │   │   └─[33mi->u[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.upvotes')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.upvotes)[0m
      │       └─[33mi->u[0m
      ├─[36mPredicate[0m
      │ ├─[33munvote[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mIn[0m
      │ │   ├─[33mu[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mJoin[0m
      │ │     ├─[33mi[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mUnion[0m
      │ │       ├─[33m(State.upvotes)[0m
      │ │       └─[33m(State.downvotes)[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mu[0m
      │ │   └─[32mUser[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.downvotes')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mDifference[0m
      │   │   ├─[33m(State.downvotes)[0m
      │   │   └─[33mi->u[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.upvotes')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.upvotes)[0m
      │       └─[33mi->u[0m
      └─[36mFunction[0m
        ├─[33mcount[0m
        ├─[36mCondition[0m
        │ └─[36mNone[0m
        ├─[36mParameters[0m
        │ └─[36mParameter[0m
        │   ├─[33mi[0m
        │   └─[32mItem[0m
        ├─[32mInt[0m
        └─[36mBody[0m
          └─[36mBinop[0m
            ├─[34mSub[0m
            ├─[36mUnop[0m
            │ ├─[34mCard[0m
            │ └─[36mBinop[0m
            │   ├─[34mJoin[0m
            │   ├─[33mi[0m
            │   └─[33m(State.upvotes)[0m
            └─[36mUnop[0m
              ├─[34mCard[0m
              └─[36mBinop[0m
                ├─[34mJoin[0m
                ├─[33mi[0m
                └─[33m(State.downvotes)[0m |}];
  !>> "upvote";
  [%expect {|
    module upvote[Item, User]

    /* PURPOSE:	 track relative popularity of items */
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

let %expect_test "Todo-Label App Alloy AST" = 
  !> "todo-label";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mlabel[0m
    │ └─[36mParameters[0m
    │   ├─[33mItem[0m
    │   └─[33mLabel[0m
    ├─[36mPurpose[0m
    │ └─[36m organize items into overlapping categories[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mlabels[0m
    │ │     ├─[32mItem -> Set Label[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mLabel[0m
    │ │ ├─[36mNo Fields[0m
    │ └─[36mSignature[0m
    │   ├─[33mItem[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ ├─[36mVar[0m
    │ │   │ │ ├─[33ml[0m
    │ │   │ │ └─[32mLabel[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mi[0m
    │ │   │   └─[32mItem[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33maffix[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   ├─[33mi[0m
    │ │       │   └─[33ml[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mBinop[0m
    │ │           ├─[34mRelease[0m
    │ │           ├─[36mBinop[0m
    │ │           │ ├─[34mIn[0m
    │ │           │ ├─[33mi[0m
    │ │           │ └─[36mCall[0m
    │ │           │   ├─[33mfind[0m
    │ │           │   └─[36mArgs[0m
    │ │           │     └─[33ml[0m
    │ │           └─[36mBinop[0m
    │ │             ├─[34mOr[0m
    │ │             ├─[36mCall[0m
    │ │             │ ├─[33mdetach[0m
    │ │             │ └─[36mArgs[0m
    │ │             │   ├─[33mi[0m
    │ │             │   └─[33ml[0m
    │ │             └─[36mCall[0m
    │ │               ├─[33mclear[0m
    │ │               └─[36mArgs[0m
    │ │                 └─[33mi[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle1[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33ml[0m
    │     │ │ └─[32mLabel[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mi[0m
    │     │   └─[32mItem[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mUnop[0m
    │         │ ├─[34mHistorically[0m
    │         │ └─[36mUnop[0m
    │         │   ├─[34mNot[0m
    │         │   └─[36mBinop[0m
    │         │     ├─[34mOr[0m
    │         │     ├─[36mCall[0m
    │         │     │ ├─[33maffix[0m
    │         │     │ └─[36mArgs[0m
    │         │     │   ├─[33mi[0m
    │         │     │   └─[33ml[0m
    │         │     └─[36mCall[0m
    │         │       ├─[33mdetach[0m
    │         │       └─[36mArgs[0m
    │         │         ├─[33mi[0m
    │         │         └─[33ml[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mUnop[0m
    │             ├─[34mNot[0m
    │             └─[36mBinop[0m
    │               ├─[34mIn[0m
    │               ├─[33mi[0m
    │               └─[36mCall[0m
    │                 ├─[33mfind[0m
    │                 └─[36mArgs[0m
    │                   └─[33ml[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33maffix[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ └─[36mBody[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.labels')[0m
      │     └─[36mBinop[0m
      │       ├─[34mUnion[0m
      │       ├─[33m(State.labels)[0m
      │       └─[33mi->l[0m
      ├─[36mPredicate[0m
      │ ├─[33mdetach[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ └─[36mBody[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.labels')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.labels)[0m
      │       └─[33mi->l[0m
      ├─[36mFunction[0m
      │ ├─[33mfind[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ ├─[32mItem[0m
      │ └─[36mBody[0m
      │   └─[36mBinop[0m
      │     ├─[34mJoin[0m
      │     ├─[33ml[0m
      │     └─[36mUnop[0m
      │       ├─[34mTilde[0m
      │       └─[33m(State.labels)[0m
      └─[36mPredicate[0m
        ├─[33mclear[0m
        ├─[36mCondition[0m
        │ └─[36mNone[0m
        ├─[36mParameters[0m
        │ └─[36mParameter[0m
        │   ├─[33mi[0m
        │   └─[32mItem[0m
        └─[36mBody[0m
          └─[36mAssignment[0m
            ├─[33m(State.labels')[0m
            └─[36mParenthesis[0m
              └─[36mBinop[0m
                ├─[34mDifference[0m
                ├─[33m(State.labels)[0m
                └─[36mBinop[0m
                  ├─[34mProduct[0m
                  ├─[33mi[0m
                  └─[33mLabel[0m
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mtodo[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36m keep track of tasks[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mdone[0m
    │ │   │ ├─[32mSet Task[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mpending[0m
    │ │     ├─[32mSet Task[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ └─[36mSignature[0m
    │   ├─[33mTask[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mt[0m
    │ │   │   └─[32mTask[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33madd[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   └─[33mt[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mBinop[0m
    │ │           ├─[34mRelease[0m
    │ │           ├─[36mBinop[0m
    │ │           │ ├─[34mIn[0m
    │ │           │ ├─[33mt[0m
    │ │           │ └─[33m(State.pending)[0m
    │ │           └─[36mBinop[0m
    │ │             ├─[34mOr[0m
    │ │             ├─[36mCall[0m
    │ │             │ ├─[33mdelete[0m
    │ │             │ └─[36mArgs[0m
    │ │             │   └─[33mt[0m
    │ │             └─[36mCall[0m
    │ │               ├─[33mcomplete[0m
    │ │               └─[36mArgs[0m
    │ │                 └─[33mt[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle1[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mt[0m
    │     │   └─[32mTask[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mcomplete[0m
    │         │ └─[36mArgs[0m
    │         │   └─[33mt[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mBinop[0m
    │             ├─[34mRelease[0m
    │             ├─[36mBinop[0m
    │             │ ├─[34mIn[0m
    │             │ ├─[33mt[0m
    │             │ └─[33m(State.done)[0m
    │             └─[36mCall[0m
    │               ├─[33mdelete[0m
    │               └─[36mArgs[0m
    │                 └─[33mt[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33madd[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mUnop[0m
      │ │   ├─[34mNot[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mIn[0m
      │ │     ├─[33mt[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mUnion[0m
      │ │       ├─[33m(State.done)[0m
      │ │       └─[33m(State.pending)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mt[0m
      │ │   └─[32mTask[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.pending')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.pending)[0m
      │   │   └─[33mt[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.done')[0m
      │     └─[33m(State.done)[0m
      ├─[36mPredicate[0m
      │ ├─[33mdelete[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mIn[0m
      │ │   ├─[33mt[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mUnion[0m
      │ │     ├─[33m(State.done)[0m
      │ │     └─[33m(State.pending)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mt[0m
      │ │   └─[32mTask[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.pending')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mDifference[0m
      │   │   ├─[33m(State.pending)[0m
      │   │   └─[33mt[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.done')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.done)[0m
      │       └─[33mt[0m
      └─[36mPredicate[0m
        ├─[33mcomplete[0m
        ├─[36mCondition[0m
        │ └─[36mBinop[0m
        │   ├─[34mIn[0m
        │   ├─[33mt[0m
        │   └─[33m(State.pending)[0m
        ├─[36mParameters[0m
        │ └─[36mParameter[0m
        │   ├─[33mt[0m
        │   └─[32mTask[0m
        └─[36mBody[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.pending')[0m
          │ └─[36mBinop[0m
          │   ├─[34mDifference[0m
          │   ├─[33m(State.pending)[0m
          │   └─[33mt[0m
          └─[36mAssignment[0m
            ├─[33m(State.done')[0m
            └─[36mBinop[0m
              ├─[34mUnion[0m
              ├─[33m(State.done)[0m
              └─[33mt[0m
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mtodo_label[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36mNo Purpose[0m
    ├─[36mDependencies[0m
    │ ├─[36mDependency[0m
    │ │ ├─[33mtodo[0m
    │ │ └─[36mGenerics[0m
    │ └─[36mDependency[0m
    │   ├─[33mlabel[0m
    │   └─[36mGenerics[0m
    │     ├─[36mGeneric[0m
    │     │ ├─[33mtodo[0m
    │     │ └─[32mTask[0m
    │     └─[36mGeneric[0m
    │       ├─[33mNo Generic[0m
    │       └─[32mString[0m
    ├─[36mSignatures[0m
    ├─[36mFacts[0m
    │ ├─[36mFact[0m
    │ │ ├─[33m_sync_todo_delete0[0m
    │ │ └─[36mUnop[0m
    │ │   ├─[34mAlways[0m
    │ │   └─[36mQuantifier[0m
    │ │     ├─[34mAll[0m
    │ │     ├─[36mVars[0m
    │ │     │ └─[36mVar[0m
    │ │     │   ├─[33mt[0m
    │ │     │   └─[32mtodo/Task[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mtodo/delete[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   └─[33mt[0m
    │ │       └─[36mCall[0m
    │ │         ├─[33mlabel/clear[0m
    │ │         └─[36mArgs[0m
    │ │           └─[33mt[0m
    │ ├─[36mFact[0m
    │ │ ├─[33m_sync_todo_add1[0m
    │ │ └─[36mUnop[0m
    │ │   ├─[34mAlways[0m
    │ │   └─[36mQuantifier[0m
    │ │     ├─[34mAll[0m
    │ │     ├─[36mVars[0m
    │ │     │ └─[36mVar[0m
    │ │     │   ├─[33mt[0m
    │ │     │   └─[32mtodo/Task[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mtodo/add[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   └─[33mt[0m
    │ │       └─[36mCall[0m
    │ │         ├─[33mlabel/affix[0m
    │ │         └─[36mArgs[0m
    │ │           ├─[33mt[0m
    │ │           └─[36mpending[0m
    │ ├─[36mFact[0m
    │ │ ├─[33m_sync_todo_complete2[0m
    │ │ └─[36mUnop[0m
    │ │   ├─[34mAlways[0m
    │ │   └─[36mQuantifier[0m
    │ │     ├─[34mAll[0m
    │ │     ├─[36mVars[0m
    │ │     │ └─[36mVar[0m
    │ │     │   ├─[33mt[0m
    │ │     │   └─[32mtodo/Task[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mtodo/complete[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   └─[33mt[0m
    │ │       └─[36mCall[0m
    │ │         ├─[33mlabel/detach[0m
    │ │         └─[36mArgs[0m
    │ │           ├─[33mt[0m
    │ │           └─[36mpending[0m
    │ ├─[36mFact[0m
    │ │ ├─[33m_sync_label_detach3[0m
    │ │ └─[36mUnop[0m
    │ │   ├─[34mAlways[0m
    │ │   └─[36mQuantifier[0m
    │ │     ├─[34mAll[0m
    │ │     ├─[36mVars[0m
    │ │     │ └─[36mVar[0m
    │ │     │   ├─[33mt[0m
    │ │     │   └─[32mtodo/Task[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mlabel/detach[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   ├─[33mt[0m
    │ │       │   └─[36mpending[0m
    │ │       └─[36mCall[0m
    │ │         ├─[33mtodo/complete[0m
    │ │         └─[36mArgs[0m
    │ │           └─[33mt[0m
    │ └─[36mFact[0m
    │   ├─[33m_sync_label_affix4[0m
    │   └─[36mUnop[0m
    │     ├─[34mAlways[0m
    │     └─[36mQuantifier[0m
    │       ├─[34mAll[0m
    │       ├─[36mVars[0m
    │       │ └─[36mVar[0m
    │       │   ├─[33mt[0m
    │       │   └─[32mtodo/Task[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mlabel/affix[0m
    │         │ └─[36mArgs[0m
    │         │   ├─[33mt[0m
    │         │   └─[36mpending[0m
    │         └─[36mCall[0m
    │           ├─[33mtodo/add[0m
    │           └─[36mArgs[0m
    │             └─[33mt[0m
    ├─[36mAssertions[0m
    └─[36mPredicates and Functions[0m |}];
  !>> "todo-label";
  [%expect {|
    module label[Item, Label]

    /* PURPOSE:	 organize items into overlapping categories */
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

    /* PURPOSE:	 keep track of tasks */
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

let %expect_test "Todo-Label-Email App Alloy AST" = 
  !> "todo-label-email";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33memail[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36m   communicate with private messages [0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33minbox[0m
    │ │   │ ├─[32mUser -> Set Message[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mfrom[0m
    │ │   │ ├─[32mMessage -> User[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mto[0m
    │ │   │ ├─[32mMessage -> User[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mcontent[0m
    │ │     ├─[32mMessage -> Content[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mUser[0m
    │ │ ├─[36mNo Fields[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mContent[0m
    │ │ ├─[36mNo Fields[0m
    │ └─[36mSignature[0m
    │   ├─[33mMessage[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle0[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mc[0m
    │     │ │ └─[32mContent[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mm[0m
    │     │ │ └─[32mMessage[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mt[0m
    │     │ │ └─[32mUser[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mb[0m
    │     │   └─[32mUser[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33msend[0m
    │         │ └─[36mArgs[0m
    │         │   ├─[33mb[0m
    │         │   ├─[33mt[0m
    │         │   ├─[33mm[0m
    │         │   └─[33mc[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mBinop[0m
    │             ├─[34mImplication[0m
    │             ├─[36mCall[0m
    │             │ ├─[33mreceive[0m
    │             │ └─[36mArgs[0m
    │             │   ├─[33mt[0m
    │             │   └─[33mm[0m
    │             └─[36mUnop[0m
    │               ├─[34mAfter[0m
    │               └─[36mBinop[0m
    │                 ├─[34mAnd[0m
    │                 ├─[36mBinop[0m
    │                 │ ├─[34mIn[0m
    │                 │ ├─[33mm[0m
    │                 │ └─[36mBinop[0m
    │                 │   ├─[34mJoin[0m
    │                 │   ├─[33mt[0m
    │                 │   └─[33m(State.inbox)[0m
    │                 └─[36mBinop[0m
    │                   ├─[34mEq[0m
    │                   ├─[36mBinop[0m
    │                   │ ├─[34mJoin[0m
    │                   │ ├─[33mm[0m
    │                   │ └─[33m(State.content)[0m
    │                   └─[33mc[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33msend[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mUnop[0m
      │ │   ├─[34mNot[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mIn[0m
      │ │     ├─[33mm[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mJoin[0m
      │ │       ├─[33mt[0m
      │ │       └─[33m(State.inbox)[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mby[0m
      │ │ │ └─[32mUser[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mt[0m
      │ │ │ └─[32mUser[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mm[0m
      │ │ │ └─[32mMessage[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mc[0m
      │ │   └─[32mContent[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.content')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[36mParenthesis[0m
      │   │   │ └─[36mBinop[0m
      │   │   │   ├─[34mDifference[0m
      │   │   │   ├─[33m(State.content)[0m
      │   │   │   └─[36mBinop[0m
      │   │   │     ├─[34mProduct[0m
      │   │   │     ├─[33mm[0m
      │   │   │     └─[33mContent[0m
      │   │   └─[33mm->c[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.to')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[36mParenthesis[0m
      │   │   │ └─[36mBinop[0m
      │   │   │   ├─[34mDifference[0m
      │   │   │   ├─[33m(State.to)[0m
      │   │   │   └─[36mBinop[0m
      │   │   │     ├─[34mProduct[0m
      │   │   │     ├─[33mm[0m
      │   │   │     └─[33mUser[0m
      │   │   └─[33mm->t[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.from')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[36mParenthesis[0m
      │   │   │ └─[36mBinop[0m
      │   │   │   ├─[34mDifference[0m
      │   │   │   ├─[33m(State.from)[0m
      │   │   │   └─[36mBinop[0m
      │   │   │     ├─[34mProduct[0m
      │   │   │     ├─[33mm[0m
      │   │   │     └─[33mUser[0m
      │   │   └─[33mm->by[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.inbox')[0m
      │     └─[33m(State.inbox)[0m
      ├─[36mPredicate[0m
      │ ├─[33mreceive[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mAnd[0m
      │ │   ├─[36mUnop[0m
      │ │   │ ├─[34mNot[0m
      │ │   │ └─[36mBinop[0m
      │ │   │   ├─[34mIn[0m
      │ │   │   ├─[33mm[0m
      │ │   │   └─[36mBinop[0m
      │ │   │     ├─[34mJoin[0m
      │ │   │     ├─[33mby[0m
      │ │   │     └─[33m(State.inbox)[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mEq[0m
      │ │     ├─[36mBinop[0m
      │ │     │ ├─[34mJoin[0m
      │ │     │ ├─[33mm[0m
      │ │     │ └─[33m(State.to)[0m
      │ │     └─[33mby[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mby[0m
      │ │ │ └─[32mUser[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mm[0m
      │ │   └─[32mMessage[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.inbox')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.inbox)[0m
      │   │   └─[33mby->m[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.from')[0m
      │   │ └─[33m(State.from)[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.to')[0m
      │   │ └─[33m(State.to)[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.content')[0m
      │     └─[33m(State.content)[0m
      └─[36mPredicate[0m
        ├─[33mdelete[0m
        ├─[36mCondition[0m
        │ └─[36mBinop[0m
        │   ├─[34mIn[0m
        │   ├─[33mm[0m
        │   └─[36mBinop[0m
        │     ├─[34mJoin[0m
        │     ├─[33mu[0m
        │     └─[33m(State.inbox)[0m
        ├─[36mParameters[0m
        │ ├─[36mParameter[0m
        │ │ ├─[33mu[0m
        │ │ └─[32mUser[0m
        │ └─[36mParameter[0m
        │   ├─[33mm[0m
        │   └─[32mMessage[0m
        └─[36mBody[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.content')[0m
          │ └─[36mParenthesis[0m
          │   └─[36mBinop[0m
          │     ├─[34mDifference[0m
          │     ├─[33m(State.content)[0m
          │     └─[36mBinop[0m
          │       ├─[34mProduct[0m
          │       ├─[33mm[0m
          │       └─[33mContent[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.to')[0m
          │ └─[36mParenthesis[0m
          │   └─[36mBinop[0m
          │     ├─[34mDifference[0m
          │     ├─[33m(State.to)[0m
          │     └─[36mBinop[0m
          │       ├─[34mProduct[0m
          │       ├─[33mm[0m
          │       └─[33mUser[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.from')[0m
          │ └─[36mParenthesis[0m
          │   └─[36mBinop[0m
          │     ├─[34mDifference[0m
          │     ├─[33m(State.from)[0m
          │     └─[36mBinop[0m
          │       ├─[34mProduct[0m
          │       ├─[33mm[0m
          │       └─[33mUser[0m
          └─[36mAssignment[0m
            ├─[33m(State.inbox')[0m
            └─[36mBinop[0m
              ├─[34mDifference[0m
              ├─[33m(State.inbox)[0m
              └─[33mu->m[0m
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mtodo[0m
    │ └─[36mParameters[0m
    │   └─[33mTask[0m
    ├─[36mPurpose[0m
    │ └─[36m keep track of tasks[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   ├─[36mField[0m
    │ │   │ ├─[33mdone[0m
    │ │   │ ├─[32mSet Task[0m
    │ │   │ ├─[36mExpression[0m
    │ │   │ │ └─[36mNone[0m
    │ │   │ └─[36mfalse[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mpending[0m
    │ │     ├─[32mSet Task[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ └─[36mSignature[0m
    │   ├─[33mTask[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mt[0m
    │ │   │   └─[32mTask[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33madd[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   └─[33mt[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mBinop[0m
    │ │           ├─[34mRelease[0m
    │ │           ├─[36mBinop[0m
    │ │           │ ├─[34mIn[0m
    │ │           │ ├─[33mt[0m
    │ │           │ └─[33m(State.pending)[0m
    │ │           └─[36mBinop[0m
    │ │             ├─[34mOr[0m
    │ │             ├─[36mCall[0m
    │ │             │ ├─[33mdelete[0m
    │ │             │ └─[36mArgs[0m
    │ │             │   └─[33mt[0m
    │ │             └─[36mCall[0m
    │ │               ├─[33mcomplete[0m
    │ │               └─[36mArgs[0m
    │ │                 └─[33mt[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle1[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mt[0m
    │     │   └─[32mTask[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mcomplete[0m
    │         │ └─[36mArgs[0m
    │         │   └─[33mt[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mBinop[0m
    │             ├─[34mRelease[0m
    │             ├─[36mBinop[0m
    │             │ ├─[34mIn[0m
    │             │ ├─[33mt[0m
    │             │ └─[33m(State.done)[0m
    │             └─[36mCall[0m
    │               ├─[33mdelete[0m
    │               └─[36mArgs[0m
    │                 └─[33mt[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33madd[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mUnop[0m
      │ │   ├─[34mNot[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mIn[0m
      │ │     ├─[33mt[0m
      │ │     └─[36mBinop[0m
      │ │       ├─[34mUnion[0m
      │ │       ├─[33m(State.done)[0m
      │ │       └─[33m(State.pending)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mt[0m
      │ │   └─[32mTask[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.pending')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mUnion[0m
      │   │   ├─[33m(State.pending)[0m
      │   │   └─[33mt[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.done')[0m
      │     └─[33m(State.done)[0m
      ├─[36mPredicate[0m
      │ ├─[33mdelete[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mBinop[0m
      │ │   ├─[34mIn[0m
      │ │   ├─[33mt[0m
      │ │   └─[36mBinop[0m
      │ │     ├─[34mUnion[0m
      │ │     ├─[33m(State.done)[0m
      │ │     └─[33m(State.pending)[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33mt[0m
      │ │   └─[32mTask[0m
      │ └─[36mBody[0m
      │   ├─[36mAssignment[0m
      │   │ ├─[33m(State.pending')[0m
      │   │ └─[36mBinop[0m
      │   │   ├─[34mDifference[0m
      │   │   ├─[33m(State.pending)[0m
      │   │   └─[33mt[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.done')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.done)[0m
      │       └─[33mt[0m
      └─[36mPredicate[0m
        ├─[33mcomplete[0m
        ├─[36mCondition[0m
        │ └─[36mBinop[0m
        │   ├─[34mIn[0m
        │   ├─[33mt[0m
        │   └─[33m(State.pending)[0m
        ├─[36mParameters[0m
        │ └─[36mParameter[0m
        │   ├─[33mt[0m
        │   └─[32mTask[0m
        └─[36mBody[0m
          ├─[36mAssignment[0m
          │ ├─[33m(State.pending')[0m
          │ └─[36mBinop[0m
          │   ├─[34mDifference[0m
          │   ├─[33m(State.pending)[0m
          │   └─[33mt[0m
          └─[36mAssignment[0m
            ├─[33m(State.done')[0m
            └─[36mBinop[0m
              ├─[34mUnion[0m
              ├─[33m(State.done)[0m
              └─[33mt[0m
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mlabel[0m
    │ └─[36mParameters[0m
    │   └─[33mItem[0m
    ├─[36mPurpose[0m
    │ └─[36m organize items into overlapping categories[0m
    ├─[36mDependencies[0m
    ├─[36mSignatures[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mState[0m
    │ │ ├─[34mOne [0m
    │ │ └─[36mFields[0m
    │ │   └─[36mField[0m
    │ │     ├─[33mlabels[0m
    │ │     ├─[32mItem -> Set Label[0m
    │ │     ├─[36mExpression[0m
    │ │     │ └─[36mNone[0m
    │ │     └─[36mfalse[0m
    │ ├─[36mSignature[0m
    │ │ ├─[33mLabel[0m
    │ │ ├─[36mNo Fields[0m
    │ └─[36mSignature[0m
    │   ├─[33mItem[0m
    │   ├─[36mNo Fields[0m
    ├─[36mFacts[0m
    ├─[36mAssertions[0m
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ ├─[36mVar[0m
    │ │   │ │ ├─[33ml[0m
    │ │   │ │ └─[32mLabel[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mi[0m
    │ │   │   └─[32mItem[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33maffix[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   ├─[33mi[0m
    │ │       │   └─[33ml[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mBinop[0m
    │ │           ├─[34mRelease[0m
    │ │           ├─[36mBinop[0m
    │ │           │ ├─[34mIn[0m
    │ │           │ ├─[33mi[0m
    │ │           │ └─[36mCall[0m
    │ │           │   ├─[33mfind[0m
    │ │           │   └─[36mArgs[0m
    │ │           │     └─[33ml[0m
    │ │           └─[36mBinop[0m
    │ │             ├─[34mOr[0m
    │ │             ├─[36mCall[0m
    │ │             │ ├─[33mdetach[0m
    │ │             │ └─[36mArgs[0m
    │ │             │   ├─[33mi[0m
    │ │             │   └─[33ml[0m
    │ │             └─[36mCall[0m
    │ │               ├─[33mclear[0m
    │ │               └─[36mArgs[0m
    │ │                 └─[33mi[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle1[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33ml[0m
    │     │ │ └─[32mLabel[0m
    │     │ └─[36mVar[0m
    │     │   ├─[33mi[0m
    │     │   └─[32mItem[0m
    │     └─[36mUnop[0m
    │       ├─[34mAlways[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mUnop[0m
    │         │ ├─[34mHistorically[0m
    │         │ └─[36mUnop[0m
    │         │   ├─[34mNot[0m
    │         │   └─[36mBinop[0m
    │         │     ├─[34mOr[0m
    │         │     ├─[36mCall[0m
    │         │     │ ├─[33maffix[0m
    │         │     │ └─[36mArgs[0m
    │         │     │   ├─[33mi[0m
    │         │     │   └─[33ml[0m
    │         │     └─[36mCall[0m
    │         │       ├─[33mdetach[0m
    │         │       └─[36mArgs[0m
    │         │         ├─[33mi[0m
    │         │         └─[33ml[0m
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mUnop[0m
    │             ├─[34mNot[0m
    │             └─[36mBinop[0m
    │               ├─[34mIn[0m
    │               ├─[33mi[0m
    │               └─[36mCall[0m
    │                 ├─[33mfind[0m
    │                 └─[36mArgs[0m
    │                   └─[33ml[0m
    └─[36mPredicates and Functions[0m
      ├─[36mPredicate[0m
      │ ├─[33maffix[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ └─[36mBody[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.labels')[0m
      │     └─[36mBinop[0m
      │       ├─[34mUnion[0m
      │       ├─[33m(State.labels)[0m
      │       └─[33mi->l[0m
      ├─[36mPredicate[0m
      │ ├─[33mdetach[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ ├─[36mParameter[0m
      │ │ │ ├─[33mi[0m
      │ │ │ └─[32mItem[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ └─[36mBody[0m
      │   └─[36mAssignment[0m
      │     ├─[33m(State.labels')[0m
      │     └─[36mBinop[0m
      │       ├─[34mDifference[0m
      │       ├─[33m(State.labels)[0m
      │       └─[33mi->l[0m
      ├─[36mFunction[0m
      │ ├─[33mfind[0m
      │ ├─[36mCondition[0m
      │ │ └─[36mNone[0m
      │ ├─[36mParameters[0m
      │ │ └─[36mParameter[0m
      │ │   ├─[33ml[0m
      │ │   └─[32mLabel[0m
      │ ├─[32mItem[0m
      │ └─[36mBody[0m
      │   └─[36mBinop[0m
      │     ├─[34mJoin[0m
      │     ├─[33ml[0m
      │     └─[36mUnop[0m
      │       ├─[34mTilde[0m
      │       └─[33m(State.labels)[0m
      └─[36mPredicate[0m
        ├─[33mclear[0m
        ├─[36mCondition[0m
        │ └─[36mNone[0m
        ├─[36mParameters[0m
        │ └─[36mParameter[0m
        │   ├─[33mi[0m
        │   └─[32mItem[0m
        └─[36mBody[0m
          └─[36mAssignment[0m
            ├─[33m(State.labels')[0m
            └─[36mParenthesis[0m
              └─[36mBinop[0m
                ├─[34mDifference[0m
                ├─[33m(State.labels)[0m
                └─[36mBinop[0m
                  ├─[34mProduct[0m
                  ├─[33mi[0m
                  └─[33mLabel[0m
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mtodo_label_mail[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36mNo Purpose[0m
    ├─[36mDependencies[0m
    │ ├─[36mDependency[0m
    │ │ ├─[33mtodo[0m
    │ │ └─[36mGenerics[0m
    │ │   └─[36mGeneric[0m
    │ │     ├─[33memail[0m
    │ │     └─[32mContent[0m
    │ ├─[36mDependency[0m
    │ │ ├─[33mlabel[0m
    │ │ └─[36mGenerics[0m
    │ │   └─[36mGeneric[0m
    │ │     ├─[33memail[0m
    │ │     └─[32mContent[0m
    │ └─[36mDependency[0m
    │   ├─[33memail[0m
    │   └─[36mGenerics[0m
    ├─[36mSignatures[0m
    ├─[36mFacts[0m
    │ ├─[36mFact[0m
    │ │ ├─[33m_sync_todo_delete0[0m
    │ │ └─[36mUnop[0m
    │ │   ├─[34mAlways[0m
    │ │   └─[36mQuantifier[0m
    │ │     ├─[34mAll[0m
    │ │     ├─[36mVars[0m
    │ │     │ └─[36mVar[0m
    │ │     │   ├─[33mt[0m
    │ │     │   └─[32memail/Content[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mtodo/delete[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   └─[33mt[0m
    │ │       └─[36mCall[0m
    │ │         ├─[33mlabel/clear[0m
    │ │         └─[36mArgs[0m
    │ │           └─[33mt[0m
    │ └─[36mFact[0m
    │   ├─[33m_sync_email_receive1[0m
    │   └─[36mUnop[0m
    │     ├─[34mAlways[0m
    │     └─[36mQuantifier[0m
    │       ├─[34mAll[0m
    │       ├─[36mVars[0m
    │       │ ├─[36mVar[0m
    │       │ │ ├─[33mm[0m
    │       │ │ └─[32memail/Message[0m
    │       │ └─[36mVar[0m
    │       │   ├─[33mu[0m
    │       │   └─[32memail/User[0m
    │       └─[36mBinop[0m
    │         ├─[34mImplication[0m
    │         ├─[36mCall[0m
    │         │ ├─[33memail/receive[0m
    │         │ └─[36mArgs[0m
    │         │   ├─[33mu[0m
    │         │   └─[33mm[0m
    │         └─[36mCall[0m
    │           ├─[33mtodo/add[0m
    │           └─[36mArgs[0m
    │             └─[36mBinop[0m
    │               ├─[34mJoin[0m
    │               ├─[33mm[0m
    │               └─[33m(email/State.content)[0m
    ├─[36mAssertions[0m
    └─[36mPredicates and Functions[0m |}];
  !>> "todo-label-email";
  [%expect {|
    module email

    /* PURPOSE:	   communicate with private messages  */
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

    /* PURPOSE:	 keep track of tasks */
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

    /* PURPOSE:	 organize items into overlapping categories */
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
    		all u : email/User, m : email/Message | { email/receive[u, m] => todo/add[m.(email/State.content)] }
    	)
    } |}]
  