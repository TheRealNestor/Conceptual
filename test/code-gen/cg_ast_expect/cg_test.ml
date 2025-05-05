let pretty_ir_ast p = 
  Conceptual.AstCompiler.compile_program_to_ast p 
  |> Option.map (Conceptual.Semant.typecheck_prog p)
  |> Option.map snd
  |> Option.map (Conceptual.CodeGen.translate_program_to_ir)
  |> Option.map (List.map Conceptual.AlloyPretty.program_to_tree) 
  |> Option.map (List.map PrintBox_text.to_string)
  |> Option.value ~default: ["Error"]
  
let prog_dir = "../../progs/" 
let (~/) file = prog_dir ^ file ^ ".con" 

let (!>) p =  ~/p |> pretty_ir_ast |> List.fold_left (fun _ s -> print_endline s) ()

let %test "Program found" = Sys.file_exists (~/"reservation")

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
    │ └─[36mmanage efficient use of resources[0m
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
    │ ├─[36mAssertion[0m
    │ │ ├─[33m_principle0[0m
    │ │ └─[36mQuantifier[0m
    │ │   ├─[34mAll[0m
    │ │   ├─[36mVars[0m
    │ │   │ ├─[36mVar[0m
    │ │   │ │ ├─[33mr[0m
    │ │   │ │ └─[32mResource[0m
    │ │   │ └─[36mVar[0m
    │ │   │   ├─[33mu[0m
    │ │   │   └─[32mUser[0m
    │ │   └─[36mUnop[0m
    │ │     ├─[34mAlways[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mImplication[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mreserve[0m
    │ │       │ └─[36mArgs[0m
    │ │       │   ├─[33mu[0m
    │ │       │   └─[33mr[0m
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mBinop[0m
    │ │           ├─[34mRelease[0m
    │ │           ├─[36mCall[0m
    │ │           │ ├─[33m_can_use[0m
    │ │           │ └─[36mArgs[0m
    │ │           │   ├─[33mu[0m
    │ │           │   └─[33mr[0m
    │ │           └─[36mCall[0m
    │ │             ├─[33mcancel[0m
    │ │             └─[36mArgs[0m
    │ │               ├─[33mu[0m
    │ │               └─[33mr[0m
    │ └─[36mAssertion[0m
    │   ├─[33m_principle1[0m
    │   └─[36mQuantifier[0m
    │     ├─[34mAll[0m
    │     ├─[36mVars[0m
    │     │ ├─[36mVar[0m
    │     │ │ ├─[33mu2[0m
    │     │ │ └─[32mUser[0m
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
    │             ├─[36mUnop[0m
    │             │ ├─[34mNot[0m
    │             │ └─[36mCall[0m
    │             │   ├─[33m_can_reserve[0m
    │             │   └─[36mArgs[0m
    │             │     ├─[33mu2[0m
    │             │     └─[33mr[0m
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
      │ │ └─[36mNone[0m
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
            └─[33m(State.reservations)[0m |}]

let %expect_test "Trash Concept Alloy AST" = 
  !> "trash";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mtrash[0m
    │ └─[36mParameters[0m
    │   └─[33mItem[0m
    ├─[36mPurpose[0m
    │ └─[36mto allow undoing of deletions[0m
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
    │             │ ├─[33mclear[0m
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
        ├─[33mclear[0m
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
            └─[33m(State.accessible)[0m |}]

let %expect_test "Email Concept Alloy AST" = 
  !> "email";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33memail[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36mcommunicate with private messages[0m
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
              └─[33mu->m[0m |}]
  
let %expect_test "Label Concept Alloy AST" =
  !> "label";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mlabel[0m
    │ └─[36mParameters[0m
    │   └─[33mItem[0m
    ├─[36mPurpose[0m
    │ └─[36morganize items into overlapping categories[0m
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
                  └─[33mLabel[0m |}]

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
    │ └─[36measing consistent formatting of elements[0m
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
            └─[33m(State.assigned)[0m |}]

let %expect_test "Todo Concept Alloy AST" = 
  !> "todo";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33mtodo[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36mkeep track of tasks[0m
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
              └─[33mt[0m |}]
  
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
    │ └─[36mgauge user sentiment of items[0m
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
    │ │         └─[36mBinop[0m
    │ │           ├─[34mRelease[0m
    │ │           ├─[36mUnop[0m
    │ │           │ ├─[34mNot[0m
    │ │           │ └─[36mCall[0m
    │ │           │   ├─[33m_can_upvote[0m
    │ │           │   └─[36mArgs[0m
    │ │           │     ├─[33mi[0m
    │ │           │     └─[33mu[0m
    │ │           └─[36mBinop[0m
    │ │             ├─[34mOr[0m
    │ │             ├─[36mCall[0m
    │ │             │ ├─[33munvote[0m
    │ │             │ └─[36mArgs[0m
    │ │             │   ├─[33mi[0m
    │ │             │   └─[33mu[0m
    │ │             └─[36mCall[0m
    │ │               ├─[33mdownvote[0m
    │ │               └─[36mArgs[0m
    │ │                 ├─[33mi[0m
    │ │                 └─[33mu[0m
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
    │           └─[36mBinop[0m
    │             ├─[34mRelease[0m
    │             ├─[36mUnop[0m
    │             │ ├─[34mNot[0m
    │             │ └─[36mCall[0m
    │             │   ├─[33m_can_downvote[0m
    │             │   └─[36mArgs[0m
    │             │     ├─[33mi[0m
    │             │     └─[33mu[0m
    │             └─[36mBinop[0m
    │               ├─[34mOr[0m
    │               ├─[36mCall[0m
    │               │ ├─[33munvote[0m
    │               │ └─[36mArgs[0m
    │               │   ├─[33mi[0m
    │               │   └─[33mu[0m
    │               └─[36mCall[0m
    │                 ├─[33mupvote[0m
    │                 └─[36mArgs[0m
    │                   ├─[33mi[0m
    │                   └─[33mu[0m
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
                └─[33m(State.downvotes)[0m |}]
  
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
    │ └─[36morganize items into overlapping categories[0m
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
    │ └─[36mkeep track of tasks[0m
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
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mCall[0m
    │ │           ├─[33mlabel/clear[0m
    │ │           └─[36mArgs[0m
    │ │             └─[33mt[0m
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
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mCall[0m
    │ │           ├─[33mlabel/affix[0m
    │ │           └─[36mArgs[0m
    │ │             ├─[33mt[0m
    │ │             └─[36mpending[0m
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
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mCall[0m
    │ │           ├─[33mlabel/detach[0m
    │ │           └─[36mArgs[0m
    │ │             ├─[33mt[0m
    │ │             └─[36mpending[0m
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
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mCall[0m
    │ │           ├─[33mtodo/complete[0m
    │ │           └─[36mArgs[0m
    │ │             └─[33mt[0m
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
    │         └─[36mUnop[0m
    │           ├─[34mAfter[0m
    │           └─[36mCall[0m
    │             ├─[33mtodo/add[0m
    │             └─[36mArgs[0m
    │               └─[33mt[0m
    ├─[36mAssertions[0m
    └─[36mPredicates and Functions[0m |}]
  
let %expect_test "Todo-Label-Email App Alloy AST" = 
  !> "todo-label-email";
  [%expect {|
    [36mProgram[0m
    ├─[36mModule[0m
    │ ├─[33memail[0m
    │ └─[36mNo Parameters[0m
    ├─[36mPurpose[0m
    │ └─[36mcommunicate with private messages[0m
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
    │ └─[36mkeep track of tasks[0m
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
    │ └─[36morganize items into overlapping categories[0m
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
    │ │       └─[36mUnop[0m
    │ │         ├─[34mAfter[0m
    │ │         └─[36mCall[0m
    │ │           ├─[33mlabel/clear[0m
    │ │           └─[36mArgs[0m
    │ │             └─[33mt[0m
    │ └─[36mFact[0m
    │   ├─[33m_sync_email_receive1[0m
    │   └─[36mUnop[0m
    │     ├─[34mAlways[0m
    │     └─[36mQuantifier[0m
    │       ├─[34mSome[0m
    │       ├─[36mVars[0m
    │       │ └─[36mVar[0m
    │       │   ├─[33mtodo_user[0m
    │       │   └─[32memail/User[0m
    │       └─[36mQuantifier[0m
    │         ├─[34mAll[0m
    │         ├─[36mVars[0m
    │         │ └─[36mVar[0m
    │         │   ├─[33mm[0m
    │         │   └─[32memail/Message[0m
    │         └─[36mBinop[0m
    │           ├─[34mImplication[0m
    │           ├─[36mCall[0m
    │           │ ├─[33memail/receive[0m
    │           │ └─[36mArgs[0m
    │           │   ├─[33mtodo_user[0m
    │           │   └─[33mm[0m
    │           └─[36mUnop[0m
    │             ├─[34mAfter[0m
    │             └─[36mCall[0m
    │               ├─[33mtodo/add[0m
    │               └─[36mArgs[0m
    │                 └─[36mBinop[0m
    │                   ├─[34mJoin[0m
    │                   ├─[33mm[0m
    │                   └─[33m(email/State.content)[0m
    ├─[36mAssertions[0m
    └─[36mPredicates and Functions[0m |}]