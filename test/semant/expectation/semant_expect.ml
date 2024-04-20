(* Make sure IDE supports ANSI coloring for the expectance test to look pretty *)

let prog_dir = "../../progs/" 
let (~/) file = prog_dir ^ file ^ if Filename.check_suffix file ".con" then "" else ".con" 

(* pretty print the typed program p *)
let pretty_program p = 
   Conceptual.AstCompiler.compile_program_to_ast p 
  |> Option.map (Conceptual.Semant.typecheck_prog p)
  |> Option.map snd
  |> Option.map (Conceptual.TypedPretty.program_to_tree)
  |> Option.map PrintBox_text.to_string
  |> Option.value ~default:""

let error_counter = ref 0 (*Errors are propagated for some reason. This is a work-around. TODO: Fix this*)
let prog_has_errors p = 
  let prog = Conceptual.AstCompiler.compile_program_to_ast p  in 
  match prog with 
  | None -> true (*prog not found *)
  | Some prog -> 
    let errors,_ = Conceptual.Semant.typecheck_prog p prog in
    let has_error = !error_counter <> List.length errors in
    error_counter := List.length errors;
    has_error


(* check that the environment is free of errors. *)
let (??) p = Sys.file_exists (~/p) && not @@ prog_has_errors (~/p)

(* pretty print the typed program p and print *)
let (~>) p =  if Sys.file_exists ~/p then ~/p |> pretty_program |> print_endline else print_endline "File does not exist"


let %test "Testing Util Works" = Sys.file_exists (~/"reservation") 
let %test "Reservation Concept No Errors" = ?? "reservation"
let %expect_test "Reservation Concept" =
  ~> "reservation";
  [%expect {| 
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33mreservation[0m
    │   │ └─[36mParameterList[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[32mUser[0m
    │   │   └─[36mParameter[0m
    │   │     └─[32mResource[0m
    │   ├─[36mPurpose[0m
    │   │ └─ manage efficient use of resources
    │   ├─[36mStates[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mreservations[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mUser[0m
    │   │ │ │           └─[32mResource[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mavailable[0m
    │   │   │   └─[36mType: [0m[32mSet of Resource[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mprovide[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mr[0m
    │   │ │ │   └─[36mType: [0m[32mResource[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mNotIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mr[0m
    │   │ │ │   │ └─[32mResource[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mreservations[0m
    │   │ │ │   │ └─[32mMap[0m
    │   │ │ │   │   ├─[32mUser[0m
    │   │ │ │   │   └─[32mResource[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mavailable[0m
    │   │ │     │ └─[32mSet of Resource[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mavailable[0m
    │   │ │     │ │ └─[32mSet of Resource[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mr[0m
    │   │ │     │ │ └─[32mResource[0m
    │   │ │     │ └─[32mSet of Resource[0m
    │   │ │     └─[32mSet of Resource[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mretract[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mr[0m
    │   │ │ │   └─[36mType: [0m[32mResource[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mNone[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mavailable[0m
    │   │ │     │ └─[32mSet of Resource[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mMinus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mavailable[0m
    │   │ │     │ │ └─[32mSet of Resource[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mr[0m
    │   │ │     │ │ └─[32mResource[0m
    │   │ │     │ └─[32mSet of Resource[0m
    │   │ │     └─[32mSet of Resource[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mreserve[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mu[0m
    │   │ │ │ │ └─[36mType: [0m[32mUser[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mr[0m
    │   │ │ │   └─[36mType: [0m[32mResource[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mr[0m
    │   │ │ │   │ └─[32mResource[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mavailable[0m
    │   │ │ │   │ └─[32mSet of Resource[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mu[0m
    │   │ │   │ │ │ └─[32mUser[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mreservations[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mUser[0m
    │   │ │   │ │ │   └─[32mResource[0m
    │   │ │   │ │ └─[32mResource[0m
    │   │ │   │ ├─[36mBinop[0m
    │   │ │   │ │ ├─[34mPlus[0m
    │   │ │   │ │ ├─[36mRelation:[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mu[0m
    │   │ │   │ │ │ │ └─[32mUser[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mreservations[0m
    │   │ │   │ │ │ │ └─[32mMap[0m
    │   │ │   │ │ │ │   ├─[32mUser[0m
    │   │ │   │ │ │ │   └─[32mResource[0m
    │   │ │   │ │ │ └─[32mResource[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mr[0m
    │   │ │   │ │ │ └─[32mResource[0m
    │   │ │   │ │ └─[32mResource[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mUser[0m
    │   │ │   │   └─[32mResource[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mavailable[0m
    │   │ │     │ └─[32mSet of Resource[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mMinus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mavailable[0m
    │   │ │     │ │ └─[32mSet of Resource[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mr[0m
    │   │ │     │ │ └─[32mResource[0m
    │   │ │     │ └─[32mSet of Resource[0m
    │   │ │     └─[32mSet of Resource[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mcancel[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mu[0m
    │   │ │ │ │ └─[36mType: [0m[32mUser[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mr[0m
    │   │ │ │   └─[36mType: [0m[32mResource[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mr[0m
    │   │ │ │   │ └─[32mResource[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mJoin[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mu[0m
    │   │ │ │   │ │ └─[32mUser[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mreservations[0m
    │   │ │ │   │ │ └─[32mMap[0m
    │   │ │ │   │ │   ├─[32mUser[0m
    │   │ │ │   │ │   └─[32mResource[0m
    │   │ │ │   │ └─[32mResource[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mu[0m
    │   │ │   │ │ │ └─[32mUser[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mreservations[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mUser[0m
    │   │ │   │ │ │   └─[32mResource[0m
    │   │ │   │ │ └─[32mResource[0m
    │   │ │   │ ├─[36mBinop[0m
    │   │ │   │ │ ├─[34mMinus[0m
    │   │ │   │ │ ├─[36mRelation:[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mu[0m
    │   │ │   │ │ │ │ └─[32mUser[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mreservations[0m
    │   │ │   │ │ │ │ └─[32mMap[0m
    │   │ │   │ │ │ │   ├─[32mUser[0m
    │   │ │   │ │ │ │   └─[32mResource[0m
    │   │ │   │ │ │ └─[32mResource[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mr[0m
    │   │ │   │ │ │ └─[32mResource[0m
    │   │ │   │ │ └─[32mResource[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mUser[0m
    │   │ │   │   └─[32mResource[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mavailable[0m
    │   │ │     │ └─[32mSet of Resource[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mavailable[0m
    │   │ │     │ │ └─[32mSet of Resource[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mr[0m
    │   │ │     │ │ └─[32mResource[0m
    │   │ │     │ └─[32mSet of Resource[0m
    │   │ │     └─[32mSet of Resource[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33muse[0m
    │   │   ├─[36mReturn Type: [0m[36mNone[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ ├─[36mDecl[0m
    │   │   │ │ ├─[36mName: [0m[33mu[0m
    │   │   │ │ └─[36mType: [0m[32mUser[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mr[0m
    │   │   │   └─[36mType: [0m[32mResource[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mBinop[0m
    │   │   │   ├─[34mIn[0m
    │   │   │   ├─[36mVar:[0m
    │   │   │   │ ├─[33mr[0m
    │   │   │   │ └─[32mResource[0m
    │   │   │   ├─[36mBinop[0m
    │   │   │   │ ├─[34mJoin[0m
    │   │   │   │ ├─[36mVar:[0m
    │   │   │   │ │ ├─[33mu[0m
    │   │   │   │ │ └─[32mUser[0m
    │   │   │   │ ├─[36mVar:[0m
    │   │   │   │ │ ├─[33mreservations[0m
    │   │   │   │ │ └─[32mMap[0m
    │   │   │   │ │   ├─[32mUser[0m
    │   │   │   │ │   └─[32mResource[0m
    │   │   │   │ └─[32mResource[0m
    │   │   │   └─[32mBool[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mStatements[0m
    │   │     └─[36mEmpty[0m
    │   └─[36mOP[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mreserve[0m
    │         │ ├─[36mArgs[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33mu[0m
    │         │ │ │   └─[32mUser[0m
    │         │ │ └─[36mArg[0m
    │         │ │   ├─[36mVar:[0m
    │         │ │     ├─[33mr[0m
    │         │ │     └─[32mResource[0m
    │         │ └─[32mBool[0m
    │         ├─[36mBinop[0m
    │         │ ├─[34mUntil[0m
    │         │ ├─[36mCan[0m
    │         │ │ └─[36mCall[0m
    │         │ │   ├─[33muse[0m
    │         │ │   ├─[36mArgs[0m
    │         │ │   │ ├─[36mArg[0m
    │         │ │   │ │ ├─[36mVar:[0m
    │         │ │   │ │   ├─[33mu[0m
    │         │ │   │ │   └─[32mUser[0m
    │         │ │   │ └─[36mArg[0m
    │         │ │   │   ├─[36mVar:[0m
    │         │ │   │     ├─[33mr[0m
    │         │ │   │     └─[32mResource[0m
    │         │ │   └─[32mBool[0m
    │         │ ├─[36mCall[0m
    │         │ │ ├─[33mcancel[0m
    │         │ │ ├─[36mArgs[0m
    │         │ │ │ ├─[36mArg[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │   ├─[33mu[0m
    │         │ │ │ │   └─[32mUser[0m
    │         │ │ │ └─[36mArg[0m
    │         │ │ │   ├─[36mVar:[0m
    │         │ │ │     ├─[33mr[0m
    │         │ │ │     └─[32mResource[0m
    │         │ │ └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mEmpty[0m |}] 

(* let %test "Trash Concept No Errors" = ?? "trash" *)
let %expect_test "Trash Concept" = 
  ~> "trash";
  [%expect {|
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33mtrash[0m
    │   │ └─[36mParameterList[0m
    │   │   └─[36mParameter[0m
    │   │     └─[32mItem[0m
    │   ├─[36mPurpose[0m
    │   │ └─  to allow undoing of deletions
    │   ├─[36mStates[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mtrashed[0m
    │   │ │ │   └─[36mType: [0m[32mSet of Item[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33maccessible[0m
    │   │   │   └─[36mType: [0m[32mSet of Item[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mcreate[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mx[0m
    │   │ │ │   └─[36mType: [0m[32mItem[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mNotIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mx[0m
    │   │ │ │   │ └─[32mItem[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mPlus[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33maccessible[0m
    │   │ │ │   │ │ └─[32mSet of Item[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mtrashed[0m
    │   │ │ │   │ │ └─[32mSet of Item[0m
    │   │ │ │   │ └─[32mSet of Item[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33maccessible[0m
    │   │ │     │ └─[32mSet of Item[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33maccessible[0m
    │   │ │     │ │ └─[32mSet of Item[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mx[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ └─[32mSet of Item[0m
    │   │ │     └─[32mSet of Item[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mdelete[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mx[0m
    │   │ │ │   └─[36mType: [0m[32mItem[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mLand[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mIn[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mx[0m
    │   │ │ │   │ │ └─[32mItem[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33maccessible[0m
    │   │ │ │   │ │ └─[32mSet of Item[0m
    │   │ │ │   │ └─[32mBool[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mNotIn[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mx[0m
    │   │ │ │   │ │ └─[32mItem[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mtrashed[0m
    │   │ │ │   │ │ └─[32mSet of Item[0m
    │   │ │ │   │ └─[32mBool[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mVar:[0m
    │   │ │   │ │ ├─[33maccessible[0m
    │   │ │   │ │ └─[32mSet of Item[0m
    │   │ │   │ ├─[36mBinop[0m
    │   │ │   │ │ ├─[34mMinus[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33maccessible[0m
    │   │ │   │ │ │ └─[32mSet of Item[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mx[0m
    │   │ │   │ │ │ └─[32mItem[0m
    │   │ │   │ │ └─[32mSet of Item[0m
    │   │ │   │ └─[32mSet of Item[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mtrashed[0m
    │   │ │     │ └─[32mSet of Item[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mtrashed[0m
    │   │ │     │ │ └─[32mSet of Item[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mx[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ └─[32mSet of Item[0m
    │   │ │     └─[32mSet of Item[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mrestore[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mx[0m
    │   │ │ │   └─[36mType: [0m[32mItem[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mx[0m
    │   │ │ │   │ └─[32mItem[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mtrashed[0m
    │   │ │ │   │ └─[32mSet of Item[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mVar:[0m
    │   │ │   │ │ ├─[33mtrashed[0m
    │   │ │   │ │ └─[32mSet of Item[0m
    │   │ │   │ ├─[36mBinop[0m
    │   │ │   │ │ ├─[34mMinus[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mtrashed[0m
    │   │ │   │ │ │ └─[32mSet of Item[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mx[0m
    │   │ │   │ │ │ └─[32mItem[0m
    │   │ │   │ │ └─[32mSet of Item[0m
    │   │ │   │ └─[32mSet of Item[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33maccessible[0m
    │   │ │     │ └─[32mSet of Item[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33maccessible[0m
    │   │ │     │ │ └─[32mSet of Item[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mx[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ └─[32mSet of Item[0m
    │   │ │     └─[32mSet of Item[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33mempty[0m
    │   │   ├─[36mReturn Type: [0m[36mNone[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ └─[36mEmpty[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mBinop[0m
    │   │   │   ├─[34mNeq[0m
    │   │   │   ├─[36mVar:[0m
    │   │   │   │ ├─[33mtrashed[0m
    │   │   │   │ └─[32mSet of Item[0m
    │   │   │   ├─[36mEmptySet[0m
    │   │   │   │ └─[32mSet of Item[0m
    │   │   │   └─[32mBool[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mStatements[0m
    │   │     └─[36mAssignment[0m
    │   │       ├─[36mVar:[0m
    │   │       │ ├─[33mtrashed[0m
    │   │       │ └─[32mSet of Item[0m
    │   │       ├─[36mEmptySet[0m
    │   │       │ └─[32mNull[0m
    │   │       └─[32mSet of Item[0m
    │   └─[36mOP[0m
    │     ├─[36mPrinciple[0m
    │     │ └─[36mBinop[0m
    │     │   ├─[34mThen[0m
    │     │   ├─[36mCall[0m
    │     │   │ ├─[33mdelete[0m
    │     │   │ ├─[36mArgs[0m
    │     │   │ │ └─[36mArg[0m
    │     │   │ │   ├─[36mVar:[0m
    │     │   │ │     ├─[33mx[0m
    │     │   │ │     └─[32mItem[0m
    │     │   │ └─[32mBool[0m
    │     │   ├─[36mBinop[0m
    │     │   │ ├─[34mThen[0m
    │     │   │ ├─[36mCall[0m
    │     │   │ │ ├─[33mrestore[0m
    │     │   │ │ ├─[36mArgs[0m
    │     │   │ │ │ └─[36mArg[0m
    │     │   │ │ │   ├─[36mVar:[0m
    │     │   │ │ │     ├─[33mx[0m
    │     │   │ │ │     └─[32mItem[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ ├─[36mBinop[0m
    │     │   │ │ ├─[34mIn[0m
    │     │   │ │ ├─[36mVar:[0m
    │     │   │ │ │ ├─[33mx[0m
    │     │   │ │ │ └─[32mItem[0m
    │     │   │ │ ├─[36mVar:[0m
    │     │   │ │ │ ├─[33maccessible[0m
    │     │   │ │ │ └─[32mSet of Item[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ └─[32mBool[0m
    │     │   └─[32mBool[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mdelete[0m
    │         │ ├─[36mArgs[0m
    │         │ │ └─[36mArg[0m
    │         │ │   ├─[36mVar:[0m
    │         │ │     ├─[33mx[0m
    │         │ │     └─[32mItem[0m
    │         │ └─[32mBool[0m
    │         ├─[36mBinop[0m
    │         │ ├─[34mThen[0m
    │         │ ├─[36mCall[0m
    │         │ │ ├─[33mempty[0m
    │         │ │ ├─[36mArgs[0m
    │         │ │ └─[32mBool[0m
    │         │ ├─[36mBinop[0m
    │         │ │ ├─[34mNotIn[0m
    │         │ │ ├─[36mVar:[0m
    │         │ │ │ ├─[33mx[0m
    │         │ │ │ └─[32mItem[0m
    │         │ │ ├─[36mBinop[0m
    │         │ │ │ ├─[34mPlus[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ ├─[33maccessible[0m
    │         │ │ │ │ └─[32mSet of Item[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ ├─[33mtrashed[0m
    │         │ │ │ │ └─[32mSet of Item[0m
    │         │ │ │ └─[32mSet of Item[0m
    │         │ │ └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mEmpty[0m |}]

(* let %test "Email Concept No Errors" = ?? "email" *)
let %expect_test "Email Concept" = 
  ~> "email";
  [%expect {|
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33memail[0m
    │   │ └─[36mParameterList[0m
    │   │   └─[36mEmpty[0m
    │   ├─[36mPurpose[0m
    │   │ └─ communicate with private messages
    │   ├─[36mStates[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mcontent[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mMessage[0m
    │   │ │ │           └─[32mContent[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mto[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mMessage[0m
    │   │ │ │           └─[32mUser[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mfrom[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mMessage[0m
    │   │ │ │           └─[32mUser[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33minbox[0m
    │   │   │   └─[36mType: [0m[32mMap[0m
    │   │   │           ├─[32mUser[0m
    │   │   │           └─[32mSet of Message[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33msend[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mby[0m
    │   │ │ │ │ └─[36mType: [0m[32mUser[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33m_for[0m
    │   │ │ │ │ └─[36mType: [0m[32mUser[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mm[0m
    │   │ │ │ │ └─[36mType: [0m[32mMessage[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mc[0m
    │   │ │ │   └─[36mType: [0m[32mContent[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mNotIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mm[0m
    │   │ │ │   │ └─[32mMessage[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mJoin[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33m_for[0m
    │   │ │ │   │ │ └─[32mUser[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33minbox[0m
    │   │ │ │   │ │ └─[32mMap[0m
    │   │ │ │   │ │   ├─[32mUser[0m
    │   │ │ │   │ │   └─[32mSet of Message[0m
    │   │ │ │   │ └─[32mSet of Message[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mm[0m
    │   │ │   │ │ │ └─[32mMessage[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mcontent[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mMessage[0m
    │   │ │   │ │ │   └─[32mContent[0m
    │   │ │   │ │ └─[32mContent[0m
    │   │ │   │ ├─[36mVar:[0m
    │   │ │   │ │ ├─[33mc[0m
    │   │ │   │ │ └─[32mContent[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mMessage[0m
    │   │ │   │   └─[32mContent[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mm[0m
    │   │ │   │ │ │ └─[32mMessage[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mfrom[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mMessage[0m
    │   │ │   │ │ │   └─[32mUser[0m
    │   │ │   │ │ └─[32mUser[0m
    │   │ │   │ ├─[36mVar:[0m
    │   │ │   │ │ ├─[33mby[0m
    │   │ │   │ │ └─[32mUser[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mMessage[0m
    │   │ │   │   └─[32mUser[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mm[0m
    │   │ │     │ │ └─[32mMessage[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mto[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mMessage[0m
    │   │ │     │ │   └─[32mUser[0m
    │   │ │     │ └─[32mUser[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33m_for[0m
    │   │ │     │ └─[32mUser[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mMessage[0m
    │   │ │       └─[32mUser[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mreceive[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mby[0m
    │   │ │ │ │ └─[36mType: [0m[32mUser[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mm[0m
    │   │ │ │   └─[36mType: [0m[32mMessage[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mLand[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mNotIn[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mm[0m
    │   │ │ │   │ │ └─[32mMessage[0m
    │   │ │ │   │ ├─[36mBinop[0m
    │   │ │ │   │ │ ├─[34mJoin[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33mby[0m
    │   │ │ │   │ │ │ └─[32mUser[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33minbox[0m
    │   │ │ │   │ │ │ └─[32mMap[0m
    │   │ │ │   │ │ │   ├─[32mUser[0m
    │   │ │ │   │ │ │   └─[32mSet of Message[0m
    │   │ │ │   │ │ └─[32mSet of Message[0m
    │   │ │ │   │ └─[32mBool[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mEq[0m
    │   │ │ │   │ ├─[36mBinop[0m
    │   │ │ │   │ │ ├─[34mJoin[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33mm[0m
    │   │ │ │   │ │ │ └─[32mMessage[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33mto[0m
    │   │ │ │   │ │ │ └─[32mMap[0m
    │   │ │ │   │ │ │   ├─[32mMessage[0m
    │   │ │ │   │ │ │   └─[32mUser[0m
    │   │ │ │   │ │ └─[32mUser[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mby[0m
    │   │ │ │   │ │ └─[32mUser[0m
    │   │ │ │   │ └─[32mBool[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mby[0m
    │   │ │     │ │ └─[32mUser[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33minbox[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mUser[0m
    │   │ │     │ │   └─[32mSet of Message[0m
    │   │ │     │ └─[32mSet of Message[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mby[0m
    │   │ │     │ │ │ └─[32mUser[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33minbox[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mUser[0m
    │   │ │     │ │ │   └─[32mSet of Message[0m
    │   │ │     │ │ └─[32mSet of Message[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mm[0m
    │   │ │     │ │ └─[32mMessage[0m
    │   │ │     │ └─[32mSet of Message[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mUser[0m
    │   │ │       └─[32mSet of Message[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33mdelete[0m
    │   │   ├─[36mReturn Type: [0m[36mNone[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ ├─[36mDecl[0m
    │   │   │ │ ├─[36mName: [0m[33mu[0m
    │   │   │ │ └─[36mType: [0m[32mUser[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mm[0m
    │   │   │   └─[36mType: [0m[32mMessage[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mBinop[0m
    │   │   │   ├─[34mIn[0m
    │   │   │   ├─[36mVar:[0m
    │   │   │   │ ├─[33mm[0m
    │   │   │   │ └─[32mMessage[0m
    │   │   │   ├─[36mBinop[0m
    │   │   │   │ ├─[34mJoin[0m
    │   │   │   │ ├─[36mVar:[0m
    │   │   │   │ │ ├─[33mu[0m
    │   │   │   │ │ └─[32mUser[0m
    │   │   │   │ ├─[36mVar:[0m
    │   │   │   │ │ ├─[33minbox[0m
    │   │   │   │ │ └─[32mMap[0m
    │   │   │   │ │   ├─[32mUser[0m
    │   │   │   │ │   └─[32mSet of Message[0m
    │   │   │   │ └─[32mSet of Message[0m
    │   │   │   └─[32mBool[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mStatements[0m
    │   │     ├─[36mAssignment[0m
    │   │     │ ├─[36mRelation:[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mu[0m
    │   │     │ │ │ └─[32mUser[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33minbox[0m
    │   │     │ │ │ └─[32mMap[0m
    │   │     │ │ │   ├─[32mUser[0m
    │   │     │ │ │   └─[32mSet of Message[0m
    │   │     │ │ └─[32mSet of Message[0m
    │   │     │ ├─[36mBinop[0m
    │   │     │ │ ├─[34mMinus[0m
    │   │     │ │ ├─[36mRelation:[0m
    │   │     │ │ │ ├─[36mVar:[0m
    │   │     │ │ │ │ ├─[33mu[0m
    │   │     │ │ │ │ └─[32mUser[0m
    │   │     │ │ │ ├─[36mVar:[0m
    │   │     │ │ │ │ ├─[33minbox[0m
    │   │     │ │ │ │ └─[32mMap[0m
    │   │     │ │ │ │   ├─[32mUser[0m
    │   │     │ │ │ │   └─[32mSet of Message[0m
    │   │     │ │ │ └─[32mSet of Message[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mm[0m
    │   │     │ │ │ └─[32mMessage[0m
    │   │     │ │ └─[32mSet of Message[0m
    │   │     │ └─[32mMap[0m
    │   │     │   ├─[32mUser[0m
    │   │     │   └─[32mSet of Message[0m
    │   │     ├─[36mAssignment[0m
    │   │     │ ├─[36mRelation:[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mm[0m
    │   │     │ │ │ └─[32mMessage[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mfrom[0m
    │   │     │ │ │ └─[32mMap[0m
    │   │     │ │ │   ├─[32mMessage[0m
    │   │     │ │ │   └─[32mUser[0m
    │   │     │ │ └─[32mUser[0m
    │   │     │ ├─[36mEmptySet[0m
    │   │     │ │ └─[32mNull[0m
    │   │     │ └─[32mMap[0m
    │   │     │   ├─[32mMessage[0m
    │   │     │   └─[32mUser[0m
    │   │     ├─[36mAssignment[0m
    │   │     │ ├─[36mRelation:[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mm[0m
    │   │     │ │ │ └─[32mMessage[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mto[0m
    │   │     │ │ │ └─[32mMap[0m
    │   │     │ │ │   ├─[32mMessage[0m
    │   │     │ │ │   └─[32mUser[0m
    │   │     │ │ └─[32mUser[0m
    │   │     │ ├─[36mEmptySet[0m
    │   │     │ │ └─[32mNull[0m
    │   │     │ └─[32mMap[0m
    │   │     │   ├─[32mMessage[0m
    │   │     │   └─[32mUser[0m
    │   │     └─[36mAssignment[0m
    │   │       ├─[36mRelation:[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mm[0m
    │   │       │ │ └─[32mMessage[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mcontent[0m
    │   │       │ │ └─[32mMap[0m
    │   │       │ │   ├─[32mMessage[0m
    │   │       │ │   └─[32mContent[0m
    │   │       │ └─[32mContent[0m
    │   │       ├─[36mEmptySet[0m
    │   │       │ └─[32mNull[0m
    │   │       └─[32mMap[0m
    │   │         ├─[32mMessage[0m
    │   │         └─[32mContent[0m
    │   └─[36mOP[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mCall[0m
    │         │ ├─[33msend[0m
    │         │ ├─[36mArgs[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33mf[0m
    │         │ │ │   └─[32mUser[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33mt[0m
    │         │ │ │   └─[32mUser[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33mm[0m
    │         │ │ │   └─[32mMessage[0m
    │         │ │ └─[36mArg[0m
    │         │ │   ├─[36mVar:[0m
    │         │ │     ├─[33mc[0m
    │         │ │     └─[32mContent[0m
    │         │ └─[32mBool[0m
    │         ├─[36mBinop[0m
    │         │ ├─[34mThen[0m
    │         │ ├─[36mCall[0m
    │         │ │ ├─[33mreceive[0m
    │         │ │ ├─[36mArgs[0m
    │         │ │ │ ├─[36mArg[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │   ├─[33mt[0m
    │         │ │ │ │   └─[32mUser[0m
    │         │ │ │ └─[36mArg[0m
    │         │ │ │   ├─[36mVar:[0m
    │         │ │ │     ├─[33mm[0m
    │         │ │ │     └─[32mMessage[0m
    │         │ │ └─[32mBool[0m
    │         │ ├─[36mBinop[0m
    │         │ │ ├─[34mLand[0m
    │         │ │ ├─[36mBinop[0m
    │         │ │ │ ├─[34mIn[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ ├─[33mm[0m
    │         │ │ │ │ └─[32mMessage[0m
    │         │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ ├─[34mJoin[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ ├─[33mt[0m
    │         │ │ │ │ │ └─[32mUser[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ ├─[33minbox[0m
    │         │ │ │ │ │ └─[32mMap[0m
    │         │ │ │ │ │   ├─[32mUser[0m
    │         │ │ │ │ │   └─[32mSet of Message[0m
    │         │ │ │ │ └─[32mSet of Message[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ ├─[36mBinop[0m
    │         │ │ │ ├─[34mEq[0m
    │         │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ ├─[34mJoin[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ ├─[33mm[0m
    │         │ │ │ │ │ └─[32mMessage[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ ├─[33mcontent[0m
    │         │ │ │ │ │ └─[32mMap[0m
    │         │ │ │ │ │   ├─[32mMessage[0m
    │         │ │ │ │ │   └─[32mContent[0m
    │         │ │ │ │ └─[32mContent[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ ├─[33mc[0m
    │         │ │ │ │ └─[32mContent[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mEmpty[0m |}]  

(* let %test "Label Concept No Errors" = ?? "label" *)
let %expect_test "Label Concept" =
  ~> "label";
  [%expect {|
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33mlabel[0m
    │   │ └─[36mParameterList[0m
    │   │   └─[36mParameter[0m
    │   │     └─[32mItem[0m
    │   ├─[36mPurpose[0m
    │   │ └─ organize items into overlapping categories
    │   ├─[36mStates[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mlabels[0m
    │   │   │   └─[36mType: [0m[32mMap[0m
    │   │   │           ├─[32mItem[0m
    │   │   │           └─[32mSet of Label[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33maffix[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mi[0m
    │   │ │ │ │ └─[36mType: [0m[32mItem[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33ml[0m
    │   │ │ │   └─[36mType: [0m[32mLabel[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mNone[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mi[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mlabels[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of Label[0m
    │   │ │     │ └─[32mSet of Label[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mi[0m
    │   │ │     │ │ │ └─[32mItem[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mlabels[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mItem[0m
    │   │ │     │ │ │   └─[32mSet of Label[0m
    │   │ │     │ │ └─[32mSet of Label[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33ml[0m
    │   │ │     │ │ └─[32mLabel[0m
    │   │ │     │ └─[32mSet of Label[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mItem[0m
    │   │ │       └─[32mSet of Label[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mdetach[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mi[0m
    │   │ │ │ │ └─[36mType: [0m[32mItem[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33ml[0m
    │   │ │ │   └─[36mType: [0m[32mLabel[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mNone[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mi[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mlabels[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of Label[0m
    │   │ │     │ └─[32mSet of Label[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mMinus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mi[0m
    │   │ │     │ │ │ └─[32mItem[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mlabels[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mItem[0m
    │   │ │     │ │ │   └─[32mSet of Label[0m
    │   │ │     │ │ └─[32mSet of Label[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33ml[0m
    │   │ │     │ │ └─[32mLabel[0m
    │   │ │     │ └─[32mSet of Label[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mItem[0m
    │   │ │       └─[32mSet of Label[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mfind[0m
    │   │ │ ├─[36mReturn Type: [0m[32mItem[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33ml[0m
    │   │ │ │   └─[36mType: [0m[32mLabel[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mNone[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mExpression[0m
    │   │ │   └─[36mBinop[0m
    │   │ │     ├─[34mJoin[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33ml[0m
    │   │ │     │ └─[32mLabel[0m
    │   │ │     ├─[36mUnop[0m
    │   │ │     │ ├─[34mTilde[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mlabels[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of Label[0m
    │   │ │     │ └─[32mMap[0m
    │   │ │     │   ├─[32mSet of Label[0m
    │   │ │     │   └─[32mItem[0m
    │   │ │     └─[32mItem[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33mclear[0m
    │   │   ├─[36mReturn Type: [0m[36mNone[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mi[0m
    │   │   │   └─[36mType: [0m[32mItem[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mNone[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mStatements[0m
    │   │     └─[36mAssignment[0m
    │   │       ├─[36mRelation:[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mi[0m
    │   │       │ │ └─[32mItem[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mlabels[0m
    │   │       │ │ └─[32mMap[0m
    │   │       │ │   ├─[32mItem[0m
    │   │       │ │   └─[32mSet of Label[0m
    │   │       │ └─[32mSet of Label[0m
    │   │       ├─[36mEmptySet[0m
    │   │       │ └─[32mNull[0m
    │   │       └─[32mMap[0m
    │   │         ├─[32mItem[0m
    │   │         └─[32mSet of Label[0m
    │   └─[36mOP[0m
    │     ├─[36mPrinciple[0m
    │     │ └─[36mBinop[0m
    │     │   ├─[34mThen[0m
    │     │   ├─[36mCall[0m
    │     │   │ ├─[33maffix[0m
    │     │   │ ├─[36mArgs[0m
    │     │   │ │ ├─[36mArg[0m
    │     │   │ │ │ ├─[36mVar:[0m
    │     │   │ │ │   ├─[33mi[0m
    │     │   │ │ │   └─[32mItem[0m
    │     │   │ │ └─[36mArg[0m
    │     │   │ │   ├─[36mVar:[0m
    │     │   │ │     ├─[33ml[0m
    │     │   │ │     └─[32mLabel[0m
    │     │   │ └─[32mBool[0m
    │     │   ├─[36mBinop[0m
    │     │   │ ├─[34mUntil[0m
    │     │   │ ├─[36mBinop[0m
    │     │   │ │ ├─[34mIn[0m
    │     │   │ │ ├─[36mVar:[0m
    │     │   │ │ │ ├─[33mi[0m
    │     │   │ │ │ └─[32mItem[0m
    │     │   │ │ ├─[36mCall[0m
    │     │   │ │ │ ├─[33mfind[0m
    │     │   │ │ │ ├─[36mArgs[0m
    │     │   │ │ │ │ └─[36mArg[0m
    │     │   │ │ │ │   ├─[36mVar:[0m
    │     │   │ │ │ │     ├─[33ml[0m
    │     │   │ │ │ │     └─[32mLabel[0m
    │     │   │ │ │ └─[32mItem[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ ├─[36mBinop[0m
    │     │   │ │ ├─[34mLor[0m
    │     │   │ │ ├─[36mCall[0m
    │     │   │ │ │ ├─[33mdetach[0m
    │     │   │ │ │ ├─[36mArgs[0m
    │     │   │ │ │ │ ├─[36mArg[0m
    │     │   │ │ │ │ │ ├─[36mVar:[0m
    │     │   │ │ │ │ │   ├─[33mi[0m
    │     │   │ │ │ │ │   └─[32mItem[0m
    │     │   │ │ │ │ └─[36mArg[0m
    │     │   │ │ │ │   ├─[36mVar:[0m
    │     │   │ │ │ │     ├─[33ml[0m
    │     │   │ │ │ │     └─[32mLabel[0m
    │     │   │ │ │ └─[32mBool[0m
    │     │   │ │ ├─[36mCall[0m
    │     │   │ │ │ ├─[33mclear[0m
    │     │   │ │ │ ├─[36mArgs[0m
    │     │   │ │ │ │ └─[36mArg[0m
    │     │   │ │ │ │   ├─[36mVar:[0m
    │     │   │ │ │ │     ├─[33mi[0m
    │     │   │ │ │ │     └─[32mItem[0m
    │     │   │ │ │ └─[32mBool[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ └─[32mBool[0m
    │     │   └─[32mBool[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mUnop[0m
    │         │ ├─[34mNo[0m
    │         │ ├─[36mBinop[0m
    │         │ │ ├─[34mLor[0m
    │         │ │ ├─[36mCall[0m
    │         │ │ │ ├─[33maffix[0m
    │         │ │ │ ├─[36mArgs[0m
    │         │ │ │ │ ├─[36mArg[0m
    │         │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │   ├─[33mi[0m
    │         │ │ │ │ │   └─[32mItem[0m
    │         │ │ │ │ └─[36mArg[0m
    │         │ │ │ │   ├─[36mVar:[0m
    │         │ │ │ │     ├─[33ml[0m
    │         │ │ │ │     └─[32mLabel[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ ├─[36mCall[0m
    │         │ │ │ ├─[33mdetach[0m
    │         │ │ │ ├─[36mArgs[0m
    │         │ │ │ │ ├─[36mArg[0m
    │         │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │   ├─[33mi[0m
    │         │ │ │ │ │   └─[32mItem[0m
    │         │ │ │ │ └─[36mArg[0m
    │         │ │ │ │   ├─[36mVar:[0m
    │         │ │ │ │     ├─[33ml[0m
    │         │ │ │ │     └─[32mLabel[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         ├─[36mBinop[0m
    │         │ ├─[34mNotIn[0m
    │         │ ├─[36mVar:[0m
    │         │ │ ├─[33mi[0m
    │         │ │ └─[32mItem[0m
    │         │ ├─[36mCall[0m
    │         │ │ ├─[33mfind[0m
    │         │ │ ├─[36mArgs[0m
    │         │ │ │ └─[36mArg[0m
    │         │ │ │   ├─[36mVar:[0m
    │         │ │ │     ├─[33ml[0m
    │         │ │ │     └─[32mLabel[0m
    │         │ │ └─[32mItem[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mEmpty[0m |}]

(* let %test "Style Concept No Errors" = ?? "style" *)
let %expect_test "Style Concept" = 
  ~> "style";
  [%expect {|
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33mstyle[0m
    │   │ └─[36mParameterList[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[32mElement[0m
    │   │   └─[36mParameter[0m
    │   │     └─[32mFormat[0m
    │   ├─[36mPurpose[0m
    │   │ └─   easing consistent formatting of elements
    │   ├─[36mStates[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mformat[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mElement[0m
    │   │ │ │           └─[32mOne of Format[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mBinop[0m
    │   │ │     ├─[34mJoin[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33massigned[0m
    │   │ │     │ └─[32mMap[0m
    │   │ │     │   ├─[32mElement[0m
    │   │ │     │   └─[32mOne of Style[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mdefined[0m
    │   │ │     │ └─[32mMap[0m
    │   │ │     │   ├─[32mStyle[0m
    │   │ │     │   └─[32mOne of Format[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mElement[0m
    │   │ │       └─[32mOne of Format[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mdefined[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mStyle[0m
    │   │ │ │           └─[32mOne of Format[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33massigned[0m
    │   │   │   └─[36mType: [0m[32mMap[0m
    │   │   │           ├─[32mElement[0m
    │   │   │           └─[32mOne of Style[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33massign[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33me[0m
    │   │ │ │ │ └─[36mType: [0m[32mElement[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33ms[0m
    │   │ │ │   └─[36mType: [0m[32mStyle[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mNone[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33me[0m
    │   │ │     │ │ └─[32mElement[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33massigned[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mElement[0m
    │   │ │     │ │   └─[32mOne of Style[0m
    │   │ │     │ └─[32mOne of Style[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33ms[0m
    │   │ │     │ └─[32mStyle[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mElement[0m
    │   │ │       └─[32mOne of Style[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33mdefine[0m
    │   │   ├─[36mReturn Type: [0m[36mNone[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ ├─[36mDecl[0m
    │   │   │ │ ├─[36mName: [0m[33ms[0m
    │   │   │ │ └─[36mType: [0m[32mStyle[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mf[0m
    │   │   │   └─[36mType: [0m[32mFormat[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mNone[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mStatements[0m
    │   │     └─[36mAssignment[0m
    │   │       ├─[36mRelation:[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33ms[0m
    │   │       │ │ └─[32mStyle[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mdefined[0m
    │   │       │ │ └─[32mMap[0m
    │   │       │ │   ├─[32mStyle[0m
    │   │       │ │   └─[32mOne of Format[0m
    │   │       │ └─[32mOne of Format[0m
    │   │       ├─[36mVar:[0m
    │   │       │ ├─[33mf[0m
    │   │       │ └─[32mFormat[0m
    │   │       └─[32mMap[0m
    │   │         ├─[32mStyle[0m
    │   │         └─[32mOne of Format[0m
    │   └─[36mOP[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mdefine[0m
    │         │ ├─[36mArgs[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33ms[0m
    │         │ │ │   └─[32mStyle[0m
    │         │ │ └─[36mArg[0m
    │         │ │   ├─[36mVar:[0m
    │         │ │     ├─[33mf1[0m
    │         │ │     └─[32mFormat[0m
    │         │ └─[32mBool[0m
    │         ├─[36mBinop[0m
    │         │ ├─[34mThen[0m
    │         │ ├─[36mCall[0m
    │         │ │ ├─[33massign[0m
    │         │ │ ├─[36mArgs[0m
    │         │ │ │ ├─[36mArg[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │   ├─[33me1[0m
    │         │ │ │ │   └─[32mElement[0m
    │         │ │ │ └─[36mArg[0m
    │         │ │ │   ├─[36mVar:[0m
    │         │ │ │     ├─[33ms[0m
    │         │ │ │     └─[32mStyle[0m
    │         │ │ └─[32mBool[0m
    │         │ ├─[36mBinop[0m
    │         │ │ ├─[34mThen[0m
    │         │ │ ├─[36mCall[0m
    │         │ │ │ ├─[33massign[0m
    │         │ │ │ ├─[36mArgs[0m
    │         │ │ │ │ ├─[36mArg[0m
    │         │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │   ├─[33me2[0m
    │         │ │ │ │ │   └─[32mElement[0m
    │         │ │ │ │ └─[36mArg[0m
    │         │ │ │ │   ├─[36mVar:[0m
    │         │ │ │ │     ├─[33ms[0m
    │         │ │ │ │     └─[32mStyle[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ ├─[36mBinop[0m
    │         │ │ │ ├─[34mThen[0m
    │         │ │ │ ├─[36mCall[0m
    │         │ │ │ │ ├─[33mdefine[0m
    │         │ │ │ │ ├─[36mArgs[0m
    │         │ │ │ │ │ ├─[36mArg[0m
    │         │ │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ │   ├─[33ms[0m
    │         │ │ │ │ │ │   └─[32mStyle[0m
    │         │ │ │ │ │ └─[36mArg[0m
    │         │ │ │ │ │   ├─[36mVar:[0m
    │         │ │ │ │ │     ├─[33mf2[0m
    │         │ │ │ │ │     └─[32mFormat[0m
    │         │ │ │ │ └─[32mBool[0m
    │         │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ ├─[34mLand[0m
    │         │ │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ │ ├─[34mEq[0m
    │         │ │ │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ │ │ ├─[34mJoin[0m
    │         │ │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ │ │ ├─[33me1[0m
    │         │ │ │ │ │ │ │ └─[32mElement[0m
    │         │ │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ │ │ ├─[33mformat[0m
    │         │ │ │ │ │ │ │ └─[32mMap[0m
    │         │ │ │ │ │ │ │   ├─[32mElement[0m
    │         │ │ │ │ │ │ │   └─[32mOne of Format[0m
    │         │ │ │ │ │ │ └─[32mOne of Format[0m
    │         │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ │ ├─[33mf2[0m
    │         │ │ │ │ │ │ └─[32mFormat[0m
    │         │ │ │ │ │ └─[32mBool[0m
    │         │ │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ │ ├─[34mEq[0m
    │         │ │ │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ │ │ ├─[34mJoin[0m
    │         │ │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ │ │ ├─[33me2[0m
    │         │ │ │ │ │ │ │ └─[32mElement[0m
    │         │ │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ │ │ ├─[33mformat[0m
    │         │ │ │ │ │ │ │ └─[32mMap[0m
    │         │ │ │ │ │ │ │   ├─[32mElement[0m
    │         │ │ │ │ │ │ │   └─[32mOne of Format[0m
    │         │ │ │ │ │ │ └─[32mOne of Format[0m
    │         │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ │ ├─[33mf2[0m
    │         │ │ │ │ │ │ └─[32mFormat[0m
    │         │ │ │ │ │ └─[32mBool[0m
    │         │ │ │ │ └─[32mBool[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mEmpty[0m |}]

(* let %test "Todo Concept No Errors" = ?? "todo" *)
let %expect_test "Todo Concept" = 
  ~> "todo";
  [%expect {|
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33mtodo[0m
    │   │ └─[36mParameterList[0m
    │   │   └─[36mEmpty[0m
    │   ├─[36mPurpose[0m
    │   │ └─ keep track of tasks
    │   ├─[36mStates[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mpending[0m
    │   │ │ │   └─[36mType: [0m[32mSet of Task[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mdone[0m
    │   │   │   └─[36mType: [0m[32mSet of Task[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33madd[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mt[0m
    │   │ │ │   └─[36mType: [0m[32mTask[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mNotIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mt[0m
    │   │ │ │   │ └─[32mTask[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mPlus[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mdone[0m
    │   │ │ │   │ │ └─[32mSet of Task[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mpending[0m
    │   │ │ │   │ │ └─[32mSet of Task[0m
    │   │ │ │   │ └─[32mSet of Task[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mpending[0m
    │   │ │     │ └─[32mSet of Task[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mpending[0m
    │   │ │     │ │ └─[32mSet of Task[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mt[0m
    │   │ │     │ │ └─[32mTask[0m
    │   │ │     │ └─[32mSet of Task[0m
    │   │ │     └─[32mSet of Task[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mdelete[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mt[0m
    │   │ │ │   └─[36mType: [0m[32mTask[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mt[0m
    │   │ │ │   │ └─[32mTask[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mPlus[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mdone[0m
    │   │ │ │   │ │ └─[32mSet of Task[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mpending[0m
    │   │ │ │   │ │ └─[32mSet of Task[0m
    │   │ │ │   │ └─[32mSet of Task[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mVar:[0m
    │   │ │   │ │ ├─[33mdone[0m
    │   │ │   │ │ └─[32mSet of Task[0m
    │   │ │   │ ├─[36mBinop[0m
    │   │ │   │ │ ├─[34mMinus[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mdone[0m
    │   │ │   │ │ │ └─[32mSet of Task[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mt[0m
    │   │ │   │ │ │ └─[32mTask[0m
    │   │ │   │ │ └─[32mSet of Task[0m
    │   │ │   │ └─[32mSet of Task[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mpending[0m
    │   │ │     │ └─[32mSet of Task[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mMinus[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mpending[0m
    │   │ │     │ │ └─[32mSet of Task[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mt[0m
    │   │ │     │ │ └─[32mTask[0m
    │   │ │     │ └─[32mSet of Task[0m
    │   │ │     └─[32mSet of Task[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33mcomplete[0m
    │   │   ├─[36mReturn Type: [0m[36mNone[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mt[0m
    │   │   │   └─[36mType: [0m[32mTask[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mBinop[0m
    │   │   │   ├─[34mIn[0m
    │   │   │   ├─[36mVar:[0m
    │   │   │   │ ├─[33mt[0m
    │   │   │   │ └─[32mTask[0m
    │   │   │   ├─[36mVar:[0m
    │   │   │   │ ├─[33mpending[0m
    │   │   │   │ └─[32mSet of Task[0m
    │   │   │   └─[32mBool[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mStatements[0m
    │   │     ├─[36mAssignment[0m
    │   │     │ ├─[36mVar:[0m
    │   │     │ │ ├─[33mpending[0m
    │   │     │ │ └─[32mSet of Task[0m
    │   │     │ ├─[36mBinop[0m
    │   │     │ │ ├─[34mMinus[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mpending[0m
    │   │     │ │ │ └─[32mSet of Task[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mt[0m
    │   │     │ │ │ └─[32mTask[0m
    │   │     │ │ └─[32mSet of Task[0m
    │   │     │ └─[32mSet of Task[0m
    │   │     └─[36mAssignment[0m
    │   │       ├─[36mVar:[0m
    │   │       │ ├─[33mdone[0m
    │   │       │ └─[32mSet of Task[0m
    │   │       ├─[36mBinop[0m
    │   │       │ ├─[34mPlus[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mdone[0m
    │   │       │ │ └─[32mSet of Task[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mt[0m
    │   │       │ │ └─[32mTask[0m
    │   │       │ └─[32mSet of Task[0m
    │   │       └─[32mSet of Task[0m
    │   └─[36mOP[0m
    │     ├─[36mPrinciple[0m
    │     │ └─[36mBinop[0m
    │     │   ├─[34mThen[0m
    │     │   ├─[36mCall[0m
    │     │   │ ├─[33madd[0m
    │     │   │ ├─[36mArgs[0m
    │     │   │ │ └─[36mArg[0m
    │     │   │ │   ├─[36mVar:[0m
    │     │   │ │     ├─[33mt[0m
    │     │   │ │     └─[32mTask[0m
    │     │   │ └─[32mBool[0m
    │     │   ├─[36mBinop[0m
    │     │   │ ├─[34mUntil[0m
    │     │   │ ├─[36mBinop[0m
    │     │   │ │ ├─[34mIn[0m
    │     │   │ │ ├─[36mVar:[0m
    │     │   │ │ │ ├─[33mt[0m
    │     │   │ │ │ └─[32mTask[0m
    │     │   │ │ ├─[36mVar:[0m
    │     │   │ │ │ ├─[33mpending[0m
    │     │   │ │ │ └─[32mSet of Task[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ ├─[36mBinop[0m
    │     │   │ │ ├─[34mLor[0m
    │     │   │ │ ├─[36mCall[0m
    │     │   │ │ │ ├─[33mdelete[0m
    │     │   │ │ │ ├─[36mArgs[0m
    │     │   │ │ │ │ └─[36mArg[0m
    │     │   │ │ │ │   ├─[36mVar:[0m
    │     │   │ │ │ │     ├─[33mt[0m
    │     │   │ │ │ │     └─[32mTask[0m
    │     │   │ │ │ └─[32mBool[0m
    │     │   │ │ ├─[36mCall[0m
    │     │   │ │ │ ├─[33mcomplete[0m
    │     │   │ │ │ ├─[36mArgs[0m
    │     │   │ │ │ │ └─[36mArg[0m
    │     │   │ │ │ │   ├─[36mVar:[0m
    │     │   │ │ │ │     ├─[33mt[0m
    │     │   │ │ │ │     └─[32mTask[0m
    │     │   │ │ │ └─[32mBool[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ └─[32mBool[0m
    │     │   └─[32mBool[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mcomplete[0m
    │         │ ├─[36mArgs[0m
    │         │ │ └─[36mArg[0m
    │         │ │   ├─[36mVar:[0m
    │         │ │     ├─[33mt[0m
    │         │ │     └─[32mTask[0m
    │         │ └─[32mBool[0m
    │         ├─[36mBinop[0m
    │         │ ├─[34mUntil[0m
    │         │ ├─[36mBinop[0m
    │         │ │ ├─[34mIn[0m
    │         │ │ ├─[36mVar:[0m
    │         │ │ │ ├─[33mt[0m
    │         │ │ │ └─[32mTask[0m
    │         │ │ ├─[36mVar:[0m
    │         │ │ │ ├─[33mdone[0m
    │         │ │ │ └─[32mSet of Task[0m
    │         │ │ └─[32mBool[0m
    │         │ ├─[36mCall[0m
    │         │ │ ├─[33mdelete[0m
    │         │ │ ├─[36mArgs[0m
    │         │ │ │ └─[36mArg[0m
    │         │ │ │   ├─[36mVar:[0m
    │         │ │ │     ├─[33mt[0m
    │         │ │ │     └─[32mTask[0m
    │         │ │ └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mEmpty[0m |}]

(* let %test "Upvote Concept No Errors" = ?? "upvote" *)
let %expect_test "Upvote Concept" = 
  ~> "upvote";
  [%expect {|
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33mupvote[0m
    │   │ └─[36mParameterList[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[32mItem[0m
    │   │   └─[36mParameter[0m
    │   │     └─[32mUser[0m
    │   ├─[36mPurpose[0m
    │   │ └─ track relative popularity of items
    │   ├─[36mStates[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mdownvotes[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mItem[0m
    │   │ │ │           └─[32mSet of User[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mupvotes[0m
    │   │   │   └─[36mType: [0m[32mMap[0m
    │   │   │           ├─[32mItem[0m
    │   │   │           └─[32mSet of User[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mupvote[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mi[0m
    │   │ │ │ │ └─[36mType: [0m[32mItem[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mu[0m
    │   │ │ │   └─[36mType: [0m[32mUser[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mNotIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mu[0m
    │   │ │ │   │ └─[32mUser[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mJoin[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mi[0m
    │   │ │ │   │ │ └─[32mItem[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mupvotes[0m
    │   │ │ │   │ │ └─[32mMap[0m
    │   │ │ │   │ │   ├─[32mItem[0m
    │   │ │ │   │ │   └─[32mSet of User[0m
    │   │ │ │   │ └─[32mSet of User[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mi[0m
    │   │ │   │ │ │ └─[32mItem[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mupvotes[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mItem[0m
    │   │ │   │ │ │   └─[32mSet of User[0m
    │   │ │   │ │ └─[32mSet of User[0m
    │   │ │   │ ├─[36mBinop[0m
    │   │ │   │ │ ├─[34mPlus[0m
    │   │ │   │ │ ├─[36mRelation:[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mi[0m
    │   │ │   │ │ │ │ └─[32mItem[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mupvotes[0m
    │   │ │   │ │ │ │ └─[32mMap[0m
    │   │ │   │ │ │ │   ├─[32mItem[0m
    │   │ │   │ │ │ │   └─[32mSet of User[0m
    │   │ │   │ │ │ └─[32mSet of User[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mu[0m
    │   │ │   │ │ │ └─[32mUser[0m
    │   │ │   │ │ └─[32mSet of User[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mItem[0m
    │   │ │   │   └─[32mSet of User[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mi[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mdownvotes[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of User[0m
    │   │ │     │ └─[32mSet of User[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mMinus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mi[0m
    │   │ │     │ │ │ └─[32mItem[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mdownvotes[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mItem[0m
    │   │ │     │ │ │   └─[32mSet of User[0m
    │   │ │     │ │ └─[32mSet of User[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mu[0m
    │   │ │     │ │ └─[32mUser[0m
    │   │ │     │ └─[32mSet of User[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mItem[0m
    │   │ │       └─[32mSet of User[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mdownvote[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mi[0m
    │   │ │ │ │ └─[36mType: [0m[32mItem[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mu[0m
    │   │ │ │   └─[36mType: [0m[32mUser[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mNotIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mu[0m
    │   │ │ │   │ └─[32mUser[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mJoin[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mi[0m
    │   │ │ │   │ │ └─[32mItem[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mdownvotes[0m
    │   │ │ │   │ │ └─[32mMap[0m
    │   │ │ │   │ │   ├─[32mItem[0m
    │   │ │ │   │ │   └─[32mSet of User[0m
    │   │ │ │   │ └─[32mSet of User[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mi[0m
    │   │ │   │ │ │ └─[32mItem[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mdownvotes[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mItem[0m
    │   │ │   │ │ │   └─[32mSet of User[0m
    │   │ │   │ │ └─[32mSet of User[0m
    │   │ │   │ ├─[36mBinop[0m
    │   │ │   │ │ ├─[34mPlus[0m
    │   │ │   │ │ ├─[36mRelation:[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mi[0m
    │   │ │   │ │ │ │ └─[32mItem[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mdownvotes[0m
    │   │ │   │ │ │ │ └─[32mMap[0m
    │   │ │   │ │ │ │   ├─[32mItem[0m
    │   │ │   │ │ │ │   └─[32mSet of User[0m
    │   │ │   │ │ │ └─[32mSet of User[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mu[0m
    │   │ │   │ │ │ └─[32mUser[0m
    │   │ │   │ │ └─[32mSet of User[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mItem[0m
    │   │ │   │   └─[32mSet of User[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mi[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mupvotes[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of User[0m
    │   │ │     │ └─[32mSet of User[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mMinus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mi[0m
    │   │ │     │ │ │ └─[32mItem[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mupvotes[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mItem[0m
    │   │ │     │ │ │   └─[32mSet of User[0m
    │   │ │     │ │ └─[32mSet of User[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mu[0m
    │   │ │     │ │ └─[32mUser[0m
    │   │ │     │ └─[32mSet of User[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mItem[0m
    │   │ │       └─[32mSet of User[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33munvote[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mi[0m
    │   │ │ │ │ └─[36mType: [0m[32mItem[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mu[0m
    │   │ │ │   └─[36mType: [0m[32mUser[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mu[0m
    │   │ │ │   │ └─[32mUser[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mJoin[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mi[0m
    │   │ │ │   │ │ └─[32mItem[0m
    │   │ │ │   │ ├─[36mBinop[0m
    │   │ │ │   │ │ ├─[34mPlus[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33mupvotes[0m
    │   │ │ │   │ │ │ └─[32mMap[0m
    │   │ │ │   │ │ │   ├─[32mItem[0m
    │   │ │ │   │ │ │   └─[32mSet of User[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33mdownvotes[0m
    │   │ │ │   │ │ │ └─[32mMap[0m
    │   │ │ │   │ │ │   ├─[32mItem[0m
    │   │ │ │   │ │ │   └─[32mSet of User[0m
    │   │ │ │   │ │ └─[32mMap[0m
    │   │ │ │   │ │   ├─[32mItem[0m
    │   │ │ │   │ │   └─[32mSet of User[0m
    │   │ │ │   │ └─[32mSet of User[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mi[0m
    │   │ │   │ │ │ └─[32mItem[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mupvotes[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mItem[0m
    │   │ │   │ │ │   └─[32mSet of User[0m
    │   │ │   │ │ └─[32mSet of User[0m
    │   │ │   │ ├─[36mBinop[0m
    │   │ │   │ │ ├─[34mMinus[0m
    │   │ │   │ │ ├─[36mRelation:[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mi[0m
    │   │ │   │ │ │ │ └─[32mItem[0m
    │   │ │   │ │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ │ ├─[33mupvotes[0m
    │   │ │   │ │ │ │ └─[32mMap[0m
    │   │ │   │ │ │ │   ├─[32mItem[0m
    │   │ │   │ │ │ │   └─[32mSet of User[0m
    │   │ │   │ │ │ └─[32mSet of User[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mu[0m
    │   │ │   │ │ │ └─[32mUser[0m
    │   │ │   │ │ └─[32mSet of User[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mItem[0m
    │   │ │   │   └─[32mSet of User[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mi[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mdownvotes[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of User[0m
    │   │ │     │ └─[32mSet of User[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mMinus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mi[0m
    │   │ │     │ │ │ └─[32mItem[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mdownvotes[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mItem[0m
    │   │ │     │ │ │   └─[32mSet of User[0m
    │   │ │     │ │ └─[32mSet of User[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mu[0m
    │   │ │     │ │ └─[32mUser[0m
    │   │ │     │ └─[32mSet of User[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mItem[0m
    │   │ │       └─[32mSet of User[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33mcount[0m
    │   │   ├─[36mReturn Type: [0m[32mInt[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mi[0m
    │   │   │   └─[36mType: [0m[32mItem[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mNone[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mExpression[0m
    │   │     └─[36mBinop[0m
    │   │       ├─[34mMinus[0m
    │   │       ├─[36mUnop[0m
    │   │       │ ├─[34mCard[0m
    │   │       │ ├─[36mBinop[0m
    │   │       │ │ ├─[34mJoin[0m
    │   │       │ │ ├─[36mVar:[0m
    │   │       │ │ │ ├─[33mi[0m
    │   │       │ │ │ └─[32mItem[0m
    │   │       │ │ ├─[36mVar:[0m
    │   │       │ │ │ ├─[33mupvotes[0m
    │   │       │ │ │ └─[32mMap[0m
    │   │       │ │ │   ├─[32mItem[0m
    │   │       │ │ │   └─[32mSet of User[0m
    │   │       │ │ └─[32mSet of User[0m
    │   │       │ └─[32mInt[0m
    │   │       ├─[36mUnop[0m
    │   │       │ ├─[34mCard[0m
    │   │       │ ├─[36mBinop[0m
    │   │       │ │ ├─[34mJoin[0m
    │   │       │ │ ├─[36mVar:[0m
    │   │       │ │ │ ├─[33mi[0m
    │   │       │ │ │ └─[32mItem[0m
    │   │       │ │ ├─[36mVar:[0m
    │   │       │ │ │ ├─[33mdownvotes[0m
    │   │       │ │ │ └─[32mMap[0m
    │   │       │ │ │   ├─[32mItem[0m
    │   │       │ │ │   └─[32mSet of User[0m
    │   │       │ │ └─[32mSet of User[0m
    │   │       │ └─[32mInt[0m
    │   │       └─[32mInt[0m
    │   └─[36mOP[0m
    │     ├─[36mPrinciple[0m
    │     │ └─[36mBinop[0m
    │     │   ├─[34mThen[0m
    │     │   ├─[36mBinop[0m
    │     │   │ ├─[34mLor[0m
    │     │   │ ├─[36mCall[0m
    │     │   │ │ ├─[33mupvote[0m
    │     │   │ │ ├─[36mArgs[0m
    │     │   │ │ │ ├─[36mArg[0m
    │     │   │ │ │ │ ├─[36mVar:[0m
    │     │   │ │ │ │   ├─[33mi[0m
    │     │   │ │ │ │   └─[32mItem[0m
    │     │   │ │ │ └─[36mArg[0m
    │     │   │ │ │   ├─[36mVar:[0m
    │     │   │ │ │     ├─[33mu[0m
    │     │   │ │ │     └─[32mUser[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ ├─[36mCall[0m
    │     │   │ │ ├─[33mdownvote[0m
    │     │   │ │ ├─[36mArgs[0m
    │     │   │ │ │ ├─[36mArg[0m
    │     │   │ │ │ │ ├─[36mVar:[0m
    │     │   │ │ │ │   ├─[33mi[0m
    │     │   │ │ │ │   └─[32mItem[0m
    │     │   │ │ │ └─[36mArg[0m
    │     │   │ │ │   ├─[36mVar:[0m
    │     │   │ │ │     ├─[33mu[0m
    │     │   │ │ │     └─[32mUser[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ └─[32mBool[0m
    │     │   ├─[36mCan[0m
    │     │   │ └─[36mCall[0m
    │     │   │   ├─[33munvote[0m
    │     │   │   ├─[36mArgs[0m
    │     │   │   │ ├─[36mArg[0m
    │     │   │   │ │ ├─[36mVar:[0m
    │     │   │   │ │   ├─[33mi[0m
    │     │   │   │ │   └─[32mItem[0m
    │     │   │   │ └─[36mArg[0m
    │     │   │   │   ├─[36mVar:[0m
    │     │   │   │     ├─[33mu[0m
    │     │   │   │     └─[32mUser[0m
    │     │   │   └─[32mBool[0m
    │     │   └─[32mBool[0m
    │     ├─[36mPrinciple[0m
    │     │ └─[36mBinop[0m
    │     │   ├─[34mThen[0m
    │     │   ├─[36mCall[0m
    │     │   │ ├─[33mupvote[0m
    │     │   │ ├─[36mArgs[0m
    │     │   │ │ ├─[36mArg[0m
    │     │   │ │ │ ├─[36mVar:[0m
    │     │   │ │ │   ├─[33mi[0m
    │     │   │ │ │   └─[32mItem[0m
    │     │   │ │ └─[36mArg[0m
    │     │   │ │   ├─[36mVar:[0m
    │     │   │ │     ├─[33mu[0m
    │     │   │ │     └─[32mUser[0m
    │     │   │ └─[32mBool[0m
    │     │   ├─[36mUnop[0m
    │     │   │ ├─[34mNot[0m
    │     │   │ ├─[36mCan[0m
    │     │   │ │ └─[36mCall[0m
    │     │   │ │   ├─[33mupvote[0m
    │     │   │ │   ├─[36mArgs[0m
    │     │   │ │   │ ├─[36mArg[0m
    │     │   │ │   │ │ ├─[36mVar:[0m
    │     │   │ │   │ │   ├─[33mi[0m
    │     │   │ │   │ │   └─[32mItem[0m
    │     │   │ │   │ └─[36mArg[0m
    │     │   │ │   │   ├─[36mVar:[0m
    │     │   │ │   │     ├─[33mu[0m
    │     │   │ │   │     └─[32mUser[0m
    │     │   │ │   └─[32mBool[0m
    │     │   │ └─[32mBool[0m
    │     │   └─[32mBool[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mCall[0m
    │         │ ├─[33mdownvote[0m
    │         │ ├─[36mArgs[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33mi[0m
    │         │ │ │   └─[32mItem[0m
    │         │ │ └─[36mArg[0m
    │         │ │   ├─[36mVar:[0m
    │         │ │     ├─[33mu[0m
    │         │ │     └─[32mUser[0m
    │         │ └─[32mBool[0m
    │         ├─[36mUnop[0m
    │         │ ├─[34mNot[0m
    │         │ ├─[36mCan[0m
    │         │ │ └─[36mCall[0m
    │         │ │   ├─[33mdownvote[0m
    │         │ │   ├─[36mArgs[0m
    │         │ │   │ ├─[36mArg[0m
    │         │ │   │ │ ├─[36mVar:[0m
    │         │ │   │ │   ├─[33mi[0m
    │         │ │   │ │   └─[32mItem[0m
    │         │ │   │ └─[36mArg[0m
    │         │ │   │   ├─[36mVar:[0m
    │         │ │   │     ├─[33mu[0m
    │         │ │   │     └─[32mUser[0m
    │         │ │   └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mEmpty[0m |}]

(* let %test "Todo-Label App No Errors" = ?? "todo-label" *)
let %expect_test "Todo-Label App" = 
  ~> "todo-label";
  [%expect {|
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ ├─[36mConcept[0m
    │ │ ├─[36mSignature[0m
    │ │ │ ├─[33mtodo[0m
    │ │ │ └─[36mParameterList[0m
    │ │ │   └─[36mEmpty[0m
    │ │ ├─[36mPurpose[0m
    │ │ │ └─ keep track of tasks
    │ │ ├─[36mStates[0m
    │ │ │ ├─[36mState[0m
    │ │ │ │ ├─[36mParameter[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33mpending[0m
    │ │ │ │ │   └─[36mType: [0m[32mSet of Task[0m
    │ │ │ │ ├─[36mConst[0m
    │ │ │ │ │ └─[36mfalse[0m
    │ │ │ │ └─[36mExpression[0m
    │ │ │ │   └─[36mNone[0m
    │ │ │ └─[36mState[0m
    │ │ │   ├─[36mParameter[0m
    │ │ │   │ └─[36mDecl[0m
    │ │ │   │   ├─[36mName: [0m[33mdone[0m
    │ │ │   │   └─[36mType: [0m[32mSet of Task[0m
    │ │ │   ├─[36mConst[0m
    │ │ │   │ └─[36mfalse[0m
    │ │ │   └─[36mExpression[0m
    │ │ │     └─[36mNone[0m
    │ │ ├─[36mActions[0m
    │ │ │ ├─[36mAction[0m
    │ │ │ │ ├─[36mName: [0m[33madd[0m
    │ │ │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │ │ ├─[36mDeclList[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33mt[0m
    │ │ │ │ │   └─[36mType: [0m[32mTask[0m
    │ │ │ │ ├─[36mFiring Condition[0m
    │ │ │ │ │ └─[36mBinop[0m
    │ │ │ │ │   ├─[34mNotIn[0m
    │ │ │ │ │   ├─[36mVar:[0m
    │ │ │ │ │   │ ├─[33mt[0m
    │ │ │ │ │   │ └─[32mTask[0m
    │ │ │ │ │   ├─[36mBinop[0m
    │ │ │ │ │   │ ├─[34mPlus[0m
    │ │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │ │   │ │ ├─[33mdone[0m
    │ │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │ │   │ │ ├─[33mpending[0m
    │ │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │ │   │ └─[32mSet of Task[0m
    │ │ │ │ │   └─[32mBool[0m
    │ │ │ │ └─[36mBody[0m
    │ │ │ │   ├─[36mStatements[0m
    │ │ │ │   └─[36mAssignment[0m
    │ │ │ │     ├─[36mVar:[0m
    │ │ │ │     │ ├─[33mpending[0m
    │ │ │ │     │ └─[32mSet of Task[0m
    │ │ │ │     ├─[36mBinop[0m
    │ │ │ │     │ ├─[34mPlus[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mpending[0m
    │ │ │ │     │ │ └─[32mSet of Task[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mt[0m
    │ │ │ │     │ │ └─[32mTask[0m
    │ │ │ │     │ └─[32mSet of Task[0m
    │ │ │ │     └─[32mSet of Task[0m
    │ │ │ ├─[36mAction[0m
    │ │ │ │ ├─[36mName: [0m[33mdelete[0m
    │ │ │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │ │ ├─[36mDeclList[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33mt[0m
    │ │ │ │ │   └─[36mType: [0m[32mTask[0m
    │ │ │ │ ├─[36mFiring Condition[0m
    │ │ │ │ │ └─[36mBinop[0m
    │ │ │ │ │   ├─[34mIn[0m
    │ │ │ │ │   ├─[36mVar:[0m
    │ │ │ │ │   │ ├─[33mt[0m
    │ │ │ │ │   │ └─[32mTask[0m
    │ │ │ │ │   ├─[36mBinop[0m
    │ │ │ │ │   │ ├─[34mPlus[0m
    │ │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │ │   │ │ ├─[33mdone[0m
    │ │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │ │   │ │ ├─[33mpending[0m
    │ │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │ │   │ └─[32mSet of Task[0m
    │ │ │ │ │   └─[32mBool[0m
    │ │ │ │ └─[36mBody[0m
    │ │ │ │   ├─[36mStatements[0m
    │ │ │ │   ├─[36mAssignment[0m
    │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │   │ │ ├─[33mdone[0m
    │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │   │ ├─[36mBinop[0m
    │ │ │ │   │ │ ├─[34mMinus[0m
    │ │ │ │   │ │ ├─[36mVar:[0m
    │ │ │ │   │ │ │ ├─[33mdone[0m
    │ │ │ │   │ │ │ └─[32mSet of Task[0m
    │ │ │ │   │ │ ├─[36mVar:[0m
    │ │ │ │   │ │ │ ├─[33mt[0m
    │ │ │ │   │ │ │ └─[32mTask[0m
    │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │   │ └─[32mSet of Task[0m
    │ │ │ │   └─[36mAssignment[0m
    │ │ │ │     ├─[36mVar:[0m
    │ │ │ │     │ ├─[33mpending[0m
    │ │ │ │     │ └─[32mSet of Task[0m
    │ │ │ │     ├─[36mBinop[0m
    │ │ │ │     │ ├─[34mMinus[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mpending[0m
    │ │ │ │     │ │ └─[32mSet of Task[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mt[0m
    │ │ │ │     │ │ └─[32mTask[0m
    │ │ │ │     │ └─[32mSet of Task[0m
    │ │ │ │     └─[32mSet of Task[0m
    │ │ │ └─[36mAction[0m
    │ │ │   ├─[36mName: [0m[33mcomplete[0m
    │ │ │   ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │   ├─[36mDeclList[0m
    │ │ │   │ └─[36mDecl[0m
    │ │ │   │   ├─[36mName: [0m[33mt[0m
    │ │ │   │   └─[36mType: [0m[32mTask[0m
    │ │ │   ├─[36mFiring Condition[0m
    │ │ │   │ └─[36mBinop[0m
    │ │ │   │   ├─[34mIn[0m
    │ │ │   │   ├─[36mVar:[0m
    │ │ │   │   │ ├─[33mt[0m
    │ │ │   │   │ └─[32mTask[0m
    │ │ │   │   ├─[36mVar:[0m
    │ │ │   │   │ ├─[33mpending[0m
    │ │ │   │   │ └─[32mSet of Task[0m
    │ │ │   │   └─[32mBool[0m
    │ │ │   └─[36mBody[0m
    │ │ │     ├─[36mStatements[0m
    │ │ │     ├─[36mAssignment[0m
    │ │ │     │ ├─[36mVar:[0m
    │ │ │     │ │ ├─[33mpending[0m
    │ │ │     │ │ └─[32mSet of Task[0m
    │ │ │     │ ├─[36mBinop[0m
    │ │ │     │ │ ├─[34mMinus[0m
    │ │ │     │ │ ├─[36mVar:[0m
    │ │ │     │ │ │ ├─[33mpending[0m
    │ │ │     │ │ │ └─[32mSet of Task[0m
    │ │ │     │ │ ├─[36mVar:[0m
    │ │ │     │ │ │ ├─[33mt[0m
    │ │ │     │ │ │ └─[32mTask[0m
    │ │ │     │ │ └─[32mSet of Task[0m
    │ │ │     │ └─[32mSet of Task[0m
    │ │ │     └─[36mAssignment[0m
    │ │ │       ├─[36mVar:[0m
    │ │ │       │ ├─[33mdone[0m
    │ │ │       │ └─[32mSet of Task[0m
    │ │ │       ├─[36mBinop[0m
    │ │ │       │ ├─[34mPlus[0m
    │ │ │       │ ├─[36mVar:[0m
    │ │ │       │ │ ├─[33mdone[0m
    │ │ │       │ │ └─[32mSet of Task[0m
    │ │ │       │ ├─[36mVar:[0m
    │ │ │       │ │ ├─[33mt[0m
    │ │ │       │ │ └─[32mTask[0m
    │ │ │       │ └─[32mSet of Task[0m
    │ │ │       └─[32mSet of Task[0m
    │ │ └─[36mOP[0m
    │ │   ├─[36mPrinciple[0m
    │ │   │ └─[36mBinop[0m
    │ │   │   ├─[34mThen[0m
    │ │   │   ├─[36mCall[0m
    │ │   │   │ ├─[33madd[0m
    │ │   │   │ ├─[36mArgs[0m
    │ │   │   │ │ └─[36mArg[0m
    │ │   │   │ │   ├─[36mVar:[0m
    │ │   │   │ │     ├─[33mt[0m
    │ │   │   │ │     └─[32mTask[0m
    │ │   │   │ └─[32mBool[0m
    │ │   │   ├─[36mBinop[0m
    │ │   │   │ ├─[34mUntil[0m
    │ │   │   │ ├─[36mBinop[0m
    │ │   │   │ │ ├─[34mIn[0m
    │ │   │   │ │ ├─[36mVar:[0m
    │ │   │   │ │ │ ├─[33mt[0m
    │ │   │   │ │ │ └─[32mTask[0m
    │ │   │   │ │ ├─[36mVar:[0m
    │ │   │   │ │ │ ├─[33mpending[0m
    │ │   │   │ │ │ └─[32mSet of Task[0m
    │ │   │   │ │ └─[32mBool[0m
    │ │   │   │ ├─[36mBinop[0m
    │ │   │   │ │ ├─[34mLor[0m
    │ │   │   │ │ ├─[36mCall[0m
    │ │   │   │ │ │ ├─[33mdelete[0m
    │ │   │   │ │ │ ├─[36mArgs[0m
    │ │   │   │ │ │ │ └─[36mArg[0m
    │ │   │   │ │ │ │   ├─[36mVar:[0m
    │ │   │   │ │ │ │     ├─[33mt[0m
    │ │   │   │ │ │ │     └─[32mTask[0m
    │ │   │   │ │ │ └─[32mBool[0m
    │ │   │   │ │ ├─[36mCall[0m
    │ │   │   │ │ │ ├─[33mcomplete[0m
    │ │   │   │ │ │ ├─[36mArgs[0m
    │ │   │   │ │ │ │ └─[36mArg[0m
    │ │   │   │ │ │ │   ├─[36mVar:[0m
    │ │   │   │ │ │ │     ├─[33mt[0m
    │ │   │   │ │ │ │     └─[32mTask[0m
    │ │   │   │ │ │ └─[32mBool[0m
    │ │   │   │ │ └─[32mBool[0m
    │ │   │   │ └─[32mBool[0m
    │ │   │   └─[32mBool[0m
    │ │   └─[36mPrinciple[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mThen[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mcomplete[0m
    │ │       │ ├─[36mArgs[0m
    │ │       │ │ └─[36mArg[0m
    │ │       │ │   ├─[36mVar:[0m
    │ │       │ │     ├─[33mt[0m
    │ │       │ │     └─[32mTask[0m
    │ │       │ └─[32mBool[0m
    │ │       ├─[36mBinop[0m
    │ │       │ ├─[34mUntil[0m
    │ │       │ ├─[36mBinop[0m
    │ │       │ │ ├─[34mIn[0m
    │ │       │ │ ├─[36mVar:[0m
    │ │       │ │ │ ├─[33mt[0m
    │ │       │ │ │ └─[32mTask[0m
    │ │       │ │ ├─[36mVar:[0m
    │ │       │ │ │ ├─[33mdone[0m
    │ │       │ │ │ └─[32mSet of Task[0m
    │ │       │ │ └─[32mBool[0m
    │ │       │ ├─[36mCall[0m
    │ │       │ │ ├─[33mdelete[0m
    │ │       │ │ ├─[36mArgs[0m
    │ │       │ │ │ └─[36mArg[0m
    │ │       │ │ │   ├─[36mVar:[0m
    │ │       │ │ │     ├─[33mt[0m
    │ │       │ │ │     └─[32mTask[0m
    │ │       │ │ └─[32mBool[0m
    │ │       │ └─[32mBool[0m
    │ │       └─[32mBool[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33mlabel[0m
    │   │ └─[36mParameterList[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[32mItem[0m
    │   │   └─[36mParameter[0m
    │   │     └─[32mLabel[0m
    │   ├─[36mPurpose[0m
    │   │ └─ organize items into overlapping categories
    │   ├─[36mStates[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mlabels[0m
    │   │   │   └─[36mType: [0m[32mMap[0m
    │   │   │           ├─[32mItem[0m
    │   │   │           └─[32mSet of Label[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33maffix[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mi[0m
    │   │ │ │ │ └─[36mType: [0m[32mItem[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33ml[0m
    │   │ │ │   └─[36mType: [0m[32mLabel[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mNone[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mi[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mlabels[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of Label[0m
    │   │ │     │ └─[32mSet of Label[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mi[0m
    │   │ │     │ │ │ └─[32mItem[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mlabels[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mItem[0m
    │   │ │     │ │ │   └─[32mSet of Label[0m
    │   │ │     │ │ └─[32mSet of Label[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33ml[0m
    │   │ │     │ │ └─[32mLabel[0m
    │   │ │     │ └─[32mSet of Label[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mItem[0m
    │   │ │       └─[32mSet of Label[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mdetach[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mi[0m
    │   │ │ │ │ └─[36mType: [0m[32mItem[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33ml[0m
    │   │ │ │   └─[36mType: [0m[32mLabel[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mNone[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mi[0m
    │   │ │     │ │ └─[32mItem[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mlabels[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of Label[0m
    │   │ │     │ └─[32mSet of Label[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mMinus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mi[0m
    │   │ │     │ │ │ └─[32mItem[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mlabels[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mItem[0m
    │   │ │     │ │ │   └─[32mSet of Label[0m
    │   │ │     │ │ └─[32mSet of Label[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33ml[0m
    │   │ │     │ │ └─[32mLabel[0m
    │   │ │     │ └─[32mSet of Label[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mItem[0m
    │   │ │       └─[32mSet of Label[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mfind[0m
    │   │ │ ├─[36mReturn Type: [0m[32mItem[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33ml[0m
    │   │ │ │   └─[36mType: [0m[32mLabel[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mNone[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mExpression[0m
    │   │ │   └─[36mBinop[0m
    │   │ │     ├─[34mJoin[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33ml[0m
    │   │ │     │ └─[32mLabel[0m
    │   │ │     ├─[36mUnop[0m
    │   │ │     │ ├─[34mTilde[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mlabels[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mItem[0m
    │   │ │     │ │   └─[32mSet of Label[0m
    │   │ │     │ └─[32mMap[0m
    │   │ │     │   ├─[32mSet of Label[0m
    │   │ │     │   └─[32mItem[0m
    │   │ │     └─[32mItem[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33mclear[0m
    │   │   ├─[36mReturn Type: [0m[36mNone[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mi[0m
    │   │   │   └─[36mType: [0m[32mItem[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mNone[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mStatements[0m
    │   │     └─[36mAssignment[0m
    │   │       ├─[36mRelation:[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mi[0m
    │   │       │ │ └─[32mItem[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mlabels[0m
    │   │       │ │ └─[32mMap[0m
    │   │       │ │   ├─[32mItem[0m
    │   │       │ │   └─[32mSet of Label[0m
    │   │       │ └─[32mSet of Label[0m
    │   │       ├─[36mEmptySet[0m
    │   │       │ └─[32mNull[0m
    │   │       └─[32mMap[0m
    │   │         ├─[32mItem[0m
    │   │         └─[32mSet of Label[0m
    │   └─[36mOP[0m
    │     ├─[36mPrinciple[0m
    │     │ └─[36mBinop[0m
    │     │   ├─[34mThen[0m
    │     │   ├─[36mCall[0m
    │     │   │ ├─[33maffix[0m
    │     │   │ ├─[36mArgs[0m
    │     │   │ │ ├─[36mArg[0m
    │     │   │ │ │ ├─[36mVar:[0m
    │     │   │ │ │   ├─[33mi[0m
    │     │   │ │ │   └─[32mItem[0m
    │     │   │ │ └─[36mArg[0m
    │     │   │ │   ├─[36mVar:[0m
    │     │   │ │     ├─[33ml[0m
    │     │   │ │     └─[32mLabel[0m
    │     │   │ └─[32mBool[0m
    │     │   ├─[36mBinop[0m
    │     │   │ ├─[34mUntil[0m
    │     │   │ ├─[36mBinop[0m
    │     │   │ │ ├─[34mIn[0m
    │     │   │ │ ├─[36mVar:[0m
    │     │   │ │ │ ├─[33mi[0m
    │     │   │ │ │ └─[32mItem[0m
    │     │   │ │ ├─[36mCall[0m
    │     │   │ │ │ ├─[33mfind[0m
    │     │   │ │ │ ├─[36mArgs[0m
    │     │   │ │ │ │ └─[36mArg[0m
    │     │   │ │ │ │   ├─[36mVar:[0m
    │     │   │ │ │ │     ├─[33ml[0m
    │     │   │ │ │ │     └─[32mLabel[0m
    │     │   │ │ │ └─[32mItem[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ ├─[36mBinop[0m
    │     │   │ │ ├─[34mLor[0m
    │     │   │ │ ├─[36mCall[0m
    │     │   │ │ │ ├─[33mdetach[0m
    │     │   │ │ │ ├─[36mArgs[0m
    │     │   │ │ │ │ ├─[36mArg[0m
    │     │   │ │ │ │ │ ├─[36mVar:[0m
    │     │   │ │ │ │ │   ├─[33mi[0m
    │     │   │ │ │ │ │   └─[32mItem[0m
    │     │   │ │ │ │ └─[36mArg[0m
    │     │   │ │ │ │   ├─[36mVar:[0m
    │     │   │ │ │ │     ├─[33ml[0m
    │     │   │ │ │ │     └─[32mLabel[0m
    │     │   │ │ │ └─[32mBool[0m
    │     │   │ │ ├─[36mCall[0m
    │     │   │ │ │ ├─[33mclear[0m
    │     │   │ │ │ ├─[36mArgs[0m
    │     │   │ │ │ │ └─[36mArg[0m
    │     │   │ │ │ │   ├─[36mVar:[0m
    │     │   │ │ │ │     ├─[33mi[0m
    │     │   │ │ │ │     └─[32mItem[0m
    │     │   │ │ │ └─[32mBool[0m
    │     │   │ │ └─[32mBool[0m
    │     │   │ └─[32mBool[0m
    │     │   └─[32mBool[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mUnop[0m
    │         │ ├─[34mNo[0m
    │         │ ├─[36mBinop[0m
    │         │ │ ├─[34mLor[0m
    │         │ │ ├─[36mCall[0m
    │         │ │ │ ├─[33maffix[0m
    │         │ │ │ ├─[36mArgs[0m
    │         │ │ │ │ ├─[36mArg[0m
    │         │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │   ├─[33mi[0m
    │         │ │ │ │ │   └─[32mItem[0m
    │         │ │ │ │ └─[36mArg[0m
    │         │ │ │ │   ├─[36mVar:[0m
    │         │ │ │ │     ├─[33ml[0m
    │         │ │ │ │     └─[32mLabel[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ ├─[36mCall[0m
    │         │ │ │ ├─[33mdetach[0m
    │         │ │ │ ├─[36mArgs[0m
    │         │ │ │ │ ├─[36mArg[0m
    │         │ │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │   ├─[33mi[0m
    │         │ │ │ │ │   └─[32mItem[0m
    │         │ │ │ │ └─[36mArg[0m
    │         │ │ │ │   ├─[36mVar:[0m
    │         │ │ │ │     ├─[33ml[0m
    │         │ │ │ │     └─[32mLabel[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         ├─[36mBinop[0m
    │         │ ├─[34mNotIn[0m
    │         │ ├─[36mVar:[0m
    │         │ │ ├─[33mi[0m
    │         │ │ └─[32mItem[0m
    │         │ ├─[36mCall[0m
    │         │ │ ├─[33mfind[0m
    │         │ │ ├─[36mArgs[0m
    │         │ │ │ └─[36mArg[0m
    │         │ │ │   ├─[36mVar:[0m
    │         │ │ │     ├─[33ml[0m
    │         │ │ │     └─[32mLabel[0m
    │         │ │ └─[32mItem[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mApp[0m
        ├─[33mtodo_label[0m
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
        │       └─[32mString[0m
        └─[36mSyncs[0m
          ├─[36mSync[0m
          │ ├─[36mSyncCall[0m
          │ │ ├─[33mtodo[0m
          │ │ └─[36mCall[0m
          │ │   ├─[33mdelete[0m
          │ │   ├─[36mArgs[0m
          │ │   │ └─[36mArg[0m
          │ │   │   ├─[36mVar:[0m
          │ │   │     ├─[33mt[0m
          │ │   │     └─[32mTask[0m
          │ │   └─[32mBool[0m
          │ └─[36mBody[0m
          │   └─[36mSyncCall[0m
          │     ├─[33mlabel[0m
          │     └─[36mCall[0m
          │       ├─[33mclear[0m
          │       ├─[36mArgs[0m
          │       │ └─[36mArg[0m
          │       │   ├─[36mVar:[0m
          │       │     ├─[33mt[0m
          │       │     └─[32mTask[0m
          │       └─[32mBool[0m
          ├─[36mSync[0m
          │ ├─[36mSyncCall[0m
          │ │ ├─[33mtodo[0m
          │ │ └─[36mCall[0m
          │ │   ├─[33madd[0m
          │ │   ├─[36mArgs[0m
          │ │   │ └─[36mArg[0m
          │ │   │   ├─[36mVar:[0m
          │ │   │     ├─[33mt[0m
          │ │   │     └─[32mTask[0m
          │ │   └─[32mBool[0m
          │ └─[36mBody[0m
          │   └─[36mSyncCall[0m
          │     ├─[33mlabel[0m
          │     └─[36mCall[0m
          │       ├─[33maffix[0m
          │       ├─[36mArgs[0m
          │       │ ├─[36mArg[0m
          │       │ │ ├─[36mVar:[0m
          │       │ │   ├─[33mt[0m
          │       │ │   └─[32mTask[0m
          │       │ └─[36mArg[0m
          │       │   ├─[36mStringLit([0mpending[36m)[0m
          │       └─[32mBool[0m
          ├─[36mSync[0m
          │ ├─[36mSyncCall[0m
          │ │ ├─[33mtodo[0m
          │ │ └─[36mCall[0m
          │ │   ├─[33mcomplete[0m
          │ │   ├─[36mArgs[0m
          │ │   │ └─[36mArg[0m
          │ │   │   ├─[36mVar:[0m
          │ │   │     ├─[33mt[0m
          │ │   │     └─[32mTask[0m
          │ │   └─[32mBool[0m
          │ └─[36mBody[0m
          │   └─[36mSyncCall[0m
          │     ├─[33mlabel[0m
          │     └─[36mCall[0m
          │       ├─[33mdetach[0m
          │       ├─[36mArgs[0m
          │       │ ├─[36mArg[0m
          │       │ │ ├─[36mVar:[0m
          │       │ │   ├─[33mt[0m
          │       │ │   └─[32mTask[0m
          │       │ └─[36mArg[0m
          │       │   ├─[36mStringLit([0mpending[36m)[0m
          │       └─[32mBool[0m
          ├─[36mSync[0m
          │ ├─[36mSyncCall[0m
          │ │ ├─[33mlabel[0m
          │ │ └─[36mCall[0m
          │ │   ├─[33mdetach[0m
          │ │   ├─[36mArgs[0m
          │ │   │ ├─[36mArg[0m
          │ │   │ │ ├─[36mVar:[0m
          │ │   │ │   ├─[33mt[0m
          │ │   │ │   └─[32mTask[0m
          │ │   │ └─[36mArg[0m
          │ │   │   ├─[36mStringLit([0mpending[36m)[0m
          │ │   └─[32mBool[0m
          │ └─[36mBody[0m
          │   └─[36mSyncCall[0m
          │     ├─[33mtodo[0m
          │     └─[36mCall[0m
          │       ├─[33mcomplete[0m
          │       ├─[36mArgs[0m
          │       │ └─[36mArg[0m
          │       │   ├─[36mVar:[0m
          │       │     ├─[33mt[0m
          │       │     └─[32mTask[0m
          │       └─[32mBool[0m
          └─[36mSync[0m
            ├─[36mSyncCall[0m
            │ ├─[33mlabel[0m
            │ └─[36mCall[0m
            │   ├─[33maffix[0m
            │   ├─[36mArgs[0m
            │   │ ├─[36mArg[0m
            │   │ │ ├─[36mVar:[0m
            │   │ │   ├─[33mt[0m
            │   │ │   └─[32mTask[0m
            │   │ └─[36mArg[0m
            │   │   ├─[36mStringLit([0mpending[36m)[0m
            │   └─[32mBool[0m
            └─[36mBody[0m
              └─[36mSyncCall[0m
                ├─[33mtodo[0m
                └─[36mCall[0m
                  ├─[33madd[0m
                  ├─[36mArgs[0m
                  │ └─[36mArg[0m
                  │   ├─[36mVar:[0m
                  │     ├─[33mt[0m
                  │     └─[32mTask[0m
                  └─[32mBool[0m |}]

(* let %test "Todo-Label-Email App No Errors" = ?? "todo-label-email" *)
let %expect_test "Todo-Label-Email App" = 
  ~> "todo-label-email";
  [%expect {|
    [36mProgram[0m
    ├─[36mConcepts[0m
    │ ├─[36mConcept[0m
    │ │ ├─[36mSignature[0m
    │ │ │ ├─[33mlabel[0m
    │ │ │ └─[36mParameterList[0m
    │ │ │   └─[36mParameter[0m
    │ │ │     └─[32mItem[0m
    │ │ ├─[36mPurpose[0m
    │ │ │ └─ organize items into overlapping categories
    │ │ ├─[36mStates[0m
    │ │ │ └─[36mState[0m
    │ │ │   ├─[36mParameter[0m
    │ │ │   │ └─[36mDecl[0m
    │ │ │   │   ├─[36mName: [0m[33mlabels[0m
    │ │ │   │   └─[36mType: [0m[32mMap[0m
    │ │ │   │           ├─[32mItem[0m
    │ │ │   │           └─[32mSet of Label[0m
    │ │ │   ├─[36mConst[0m
    │ │ │   │ └─[36mfalse[0m
    │ │ │   └─[36mExpression[0m
    │ │ │     └─[36mNone[0m
    │ │ ├─[36mActions[0m
    │ │ │ ├─[36mAction[0m
    │ │ │ │ ├─[36mName: [0m[33maffix[0m
    │ │ │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │ │ ├─[36mDeclList[0m
    │ │ │ │ │ ├─[36mDecl[0m
    │ │ │ │ │ │ ├─[36mName: [0m[33mi[0m
    │ │ │ │ │ │ └─[36mType: [0m[32mItem[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33ml[0m
    │ │ │ │ │   └─[36mType: [0m[32mLabel[0m
    │ │ │ │ ├─[36mFiring Condition[0m
    │ │ │ │ │ └─[36mNone[0m
    │ │ │ │ └─[36mBody[0m
    │ │ │ │   ├─[36mStatements[0m
    │ │ │ │   └─[36mAssignment[0m
    │ │ │ │     ├─[36mRelation:[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mi[0m
    │ │ │ │     │ │ └─[32mItem[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mlabels[0m
    │ │ │ │     │ │ └─[32mMap[0m
    │ │ │ │     │ │   ├─[32mItem[0m
    │ │ │ │     │ │   └─[32mSet of Label[0m
    │ │ │ │     │ └─[32mSet of Label[0m
    │ │ │ │     ├─[36mBinop[0m
    │ │ │ │     │ ├─[34mPlus[0m
    │ │ │ │     │ ├─[36mRelation:[0m
    │ │ │ │     │ │ ├─[36mVar:[0m
    │ │ │ │     │ │ │ ├─[33mi[0m
    │ │ │ │     │ │ │ └─[32mItem[0m
    │ │ │ │     │ │ ├─[36mVar:[0m
    │ │ │ │     │ │ │ ├─[33mlabels[0m
    │ │ │ │     │ │ │ └─[32mMap[0m
    │ │ │ │     │ │ │   ├─[32mItem[0m
    │ │ │ │     │ │ │   └─[32mSet of Label[0m
    │ │ │ │     │ │ └─[32mSet of Label[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33ml[0m
    │ │ │ │     │ │ └─[32mLabel[0m
    │ │ │ │     │ └─[32mSet of Label[0m
    │ │ │ │     └─[32mMap[0m
    │ │ │ │       ├─[32mItem[0m
    │ │ │ │       └─[32mSet of Label[0m
    │ │ │ ├─[36mAction[0m
    │ │ │ │ ├─[36mName: [0m[33mdetach[0m
    │ │ │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │ │ ├─[36mDeclList[0m
    │ │ │ │ │ ├─[36mDecl[0m
    │ │ │ │ │ │ ├─[36mName: [0m[33mi[0m
    │ │ │ │ │ │ └─[36mType: [0m[32mItem[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33ml[0m
    │ │ │ │ │   └─[36mType: [0m[32mLabel[0m
    │ │ │ │ ├─[36mFiring Condition[0m
    │ │ │ │ │ └─[36mNone[0m
    │ │ │ │ └─[36mBody[0m
    │ │ │ │   ├─[36mStatements[0m
    │ │ │ │   └─[36mAssignment[0m
    │ │ │ │     ├─[36mRelation:[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mi[0m
    │ │ │ │     │ │ └─[32mItem[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mlabels[0m
    │ │ │ │     │ │ └─[32mMap[0m
    │ │ │ │     │ │   ├─[32mItem[0m
    │ │ │ │     │ │   └─[32mSet of Label[0m
    │ │ │ │     │ └─[32mSet of Label[0m
    │ │ │ │     ├─[36mBinop[0m
    │ │ │ │     │ ├─[34mMinus[0m
    │ │ │ │     │ ├─[36mRelation:[0m
    │ │ │ │     │ │ ├─[36mVar:[0m
    │ │ │ │     │ │ │ ├─[33mi[0m
    │ │ │ │     │ │ │ └─[32mItem[0m
    │ │ │ │     │ │ ├─[36mVar:[0m
    │ │ │ │     │ │ │ ├─[33mlabels[0m
    │ │ │ │     │ │ │ └─[32mMap[0m
    │ │ │ │     │ │ │   ├─[32mItem[0m
    │ │ │ │     │ │ │   └─[32mSet of Label[0m
    │ │ │ │     │ │ └─[32mSet of Label[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33ml[0m
    │ │ │ │     │ │ └─[32mLabel[0m
    │ │ │ │     │ └─[32mSet of Label[0m
    │ │ │ │     └─[32mMap[0m
    │ │ │ │       ├─[32mItem[0m
    │ │ │ │       └─[32mSet of Label[0m
    │ │ │ ├─[36mAction[0m
    │ │ │ │ ├─[36mName: [0m[33mfind[0m
    │ │ │ │ ├─[36mReturn Type: [0m[32mItem[0m
    │ │ │ │ ├─[36mDeclList[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33ml[0m
    │ │ │ │ │   └─[36mType: [0m[32mLabel[0m
    │ │ │ │ ├─[36mFiring Condition[0m
    │ │ │ │ │ └─[36mNone[0m
    │ │ │ │ └─[36mBody[0m
    │ │ │ │   ├─[36mExpression[0m
    │ │ │ │   └─[36mBinop[0m
    │ │ │ │     ├─[34mJoin[0m
    │ │ │ │     ├─[36mVar:[0m
    │ │ │ │     │ ├─[33ml[0m
    │ │ │ │     │ └─[32mLabel[0m
    │ │ │ │     ├─[36mUnop[0m
    │ │ │ │     │ ├─[34mTilde[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mlabels[0m
    │ │ │ │     │ │ └─[32mMap[0m
    │ │ │ │     │ │   ├─[32mItem[0m
    │ │ │ │     │ │   └─[32mSet of Label[0m
    │ │ │ │     │ └─[32mMap[0m
    │ │ │ │     │   ├─[32mSet of Label[0m
    │ │ │ │     │   └─[32mItem[0m
    │ │ │ │     └─[32mItem[0m
    │ │ │ └─[36mAction[0m
    │ │ │   ├─[36mName: [0m[33mclear[0m
    │ │ │   ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │   ├─[36mDeclList[0m
    │ │ │   │ └─[36mDecl[0m
    │ │ │   │   ├─[36mName: [0m[33mi[0m
    │ │ │   │   └─[36mType: [0m[32mItem[0m
    │ │ │   ├─[36mFiring Condition[0m
    │ │ │   │ └─[36mNone[0m
    │ │ │   └─[36mBody[0m
    │ │ │     ├─[36mStatements[0m
    │ │ │     └─[36mAssignment[0m
    │ │ │       ├─[36mRelation:[0m
    │ │ │       │ ├─[36mVar:[0m
    │ │ │       │ │ ├─[33mi[0m
    │ │ │       │ │ └─[32mItem[0m
    │ │ │       │ ├─[36mVar:[0m
    │ │ │       │ │ ├─[33mlabels[0m
    │ │ │       │ │ └─[32mMap[0m
    │ │ │       │ │   ├─[32mItem[0m
    │ │ │       │ │   └─[32mSet of Label[0m
    │ │ │       │ └─[32mSet of Label[0m
    │ │ │       ├─[36mEmptySet[0m
    │ │ │       │ └─[32mNull[0m
    │ │ │       └─[32mMap[0m
    │ │ │         ├─[32mItem[0m
    │ │ │         └─[32mSet of Label[0m
    │ │ └─[36mOP[0m
    │ │   ├─[36mPrinciple[0m
    │ │   │ └─[36mBinop[0m
    │ │   │   ├─[34mThen[0m
    │ │   │   ├─[36mCall[0m
    │ │   │   │ ├─[33maffix[0m
    │ │   │   │ ├─[36mArgs[0m
    │ │   │   │ │ ├─[36mArg[0m
    │ │   │   │ │ │ ├─[36mVar:[0m
    │ │   │   │ │ │   ├─[33mi[0m
    │ │   │   │ │ │   └─[32mItem[0m
    │ │   │   │ │ └─[36mArg[0m
    │ │   │   │ │   ├─[36mVar:[0m
    │ │   │   │ │     ├─[33ml[0m
    │ │   │   │ │     └─[32mLabel[0m
    │ │   │   │ └─[32mBool[0m
    │ │   │   ├─[36mBinop[0m
    │ │   │   │ ├─[34mUntil[0m
    │ │   │   │ ├─[36mBinop[0m
    │ │   │   │ │ ├─[34mIn[0m
    │ │   │   │ │ ├─[36mVar:[0m
    │ │   │   │ │ │ ├─[33mi[0m
    │ │   │   │ │ │ └─[32mItem[0m
    │ │   │   │ │ ├─[36mCall[0m
    │ │   │   │ │ │ ├─[33mfind[0m
    │ │   │   │ │ │ ├─[36mArgs[0m
    │ │   │   │ │ │ │ └─[36mArg[0m
    │ │   │   │ │ │ │   ├─[36mVar:[0m
    │ │   │   │ │ │ │     ├─[33ml[0m
    │ │   │   │ │ │ │     └─[32mLabel[0m
    │ │   │   │ │ │ └─[32mItem[0m
    │ │   │   │ │ └─[32mBool[0m
    │ │   │   │ ├─[36mBinop[0m
    │ │   │   │ │ ├─[34mLor[0m
    │ │   │   │ │ ├─[36mCall[0m
    │ │   │   │ │ │ ├─[33mdetach[0m
    │ │   │   │ │ │ ├─[36mArgs[0m
    │ │   │   │ │ │ │ ├─[36mArg[0m
    │ │   │   │ │ │ │ │ ├─[36mVar:[0m
    │ │   │   │ │ │ │ │   ├─[33mi[0m
    │ │   │   │ │ │ │ │   └─[32mItem[0m
    │ │   │   │ │ │ │ └─[36mArg[0m
    │ │   │   │ │ │ │   ├─[36mVar:[0m
    │ │   │   │ │ │ │     ├─[33ml[0m
    │ │   │   │ │ │ │     └─[32mLabel[0m
    │ │   │   │ │ │ └─[32mBool[0m
    │ │   │   │ │ ├─[36mCall[0m
    │ │   │   │ │ │ ├─[33mclear[0m
    │ │   │   │ │ │ ├─[36mArgs[0m
    │ │   │   │ │ │ │ └─[36mArg[0m
    │ │   │   │ │ │ │   ├─[36mVar:[0m
    │ │   │   │ │ │ │     ├─[33mi[0m
    │ │   │   │ │ │ │     └─[32mItem[0m
    │ │   │   │ │ │ └─[32mBool[0m
    │ │   │   │ │ └─[32mBool[0m
    │ │   │   │ └─[32mBool[0m
    │ │   │   └─[32mBool[0m
    │ │   └─[36mPrinciple[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mThen[0m
    │ │       ├─[36mUnop[0m
    │ │       │ ├─[34mNo[0m
    │ │       │ ├─[36mBinop[0m
    │ │       │ │ ├─[34mLor[0m
    │ │       │ │ ├─[36mCall[0m
    │ │       │ │ │ ├─[33maffix[0m
    │ │       │ │ │ ├─[36mArgs[0m
    │ │       │ │ │ │ ├─[36mArg[0m
    │ │       │ │ │ │ │ ├─[36mVar:[0m
    │ │       │ │ │ │ │   ├─[33mi[0m
    │ │       │ │ │ │ │   └─[32mItem[0m
    │ │       │ │ │ │ └─[36mArg[0m
    │ │       │ │ │ │   ├─[36mVar:[0m
    │ │       │ │ │ │     ├─[33ml[0m
    │ │       │ │ │ │     └─[32mLabel[0m
    │ │       │ │ │ └─[32mBool[0m
    │ │       │ │ ├─[36mCall[0m
    │ │       │ │ │ ├─[33mdetach[0m
    │ │       │ │ │ ├─[36mArgs[0m
    │ │       │ │ │ │ ├─[36mArg[0m
    │ │       │ │ │ │ │ ├─[36mVar:[0m
    │ │       │ │ │ │ │   ├─[33mi[0m
    │ │       │ │ │ │ │   └─[32mItem[0m
    │ │       │ │ │ │ └─[36mArg[0m
    │ │       │ │ │ │   ├─[36mVar:[0m
    │ │       │ │ │ │     ├─[33ml[0m
    │ │       │ │ │ │     └─[32mLabel[0m
    │ │       │ │ │ └─[32mBool[0m
    │ │       │ │ └─[32mBool[0m
    │ │       │ └─[32mBool[0m
    │ │       ├─[36mBinop[0m
    │ │       │ ├─[34mNotIn[0m
    │ │       │ ├─[36mVar:[0m
    │ │       │ │ ├─[33mi[0m
    │ │       │ │ └─[32mItem[0m
    │ │       │ ├─[36mCall[0m
    │ │       │ │ ├─[33mfind[0m
    │ │       │ │ ├─[36mArgs[0m
    │ │       │ │ │ └─[36mArg[0m
    │ │       │ │ │   ├─[36mVar:[0m
    │ │       │ │ │     ├─[33ml[0m
    │ │       │ │ │     └─[32mLabel[0m
    │ │       │ │ └─[32mItem[0m
    │ │       │ └─[32mBool[0m
    │ │       └─[32mBool[0m
    │ ├─[36mConcept[0m
    │ │ ├─[36mSignature[0m
    │ │ │ ├─[33mtodo[0m
    │ │ │ └─[36mParameterList[0m
    │ │ │   └─[36mParameter[0m
    │ │ │     └─[32mTask[0m
    │ │ ├─[36mPurpose[0m
    │ │ │ └─ keep track of tasks
    │ │ ├─[36mStates[0m
    │ │ │ ├─[36mState[0m
    │ │ │ │ ├─[36mParameter[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33mpending[0m
    │ │ │ │ │   └─[36mType: [0m[32mSet of Task[0m
    │ │ │ │ ├─[36mConst[0m
    │ │ │ │ │ └─[36mfalse[0m
    │ │ │ │ └─[36mExpression[0m
    │ │ │ │   └─[36mNone[0m
    │ │ │ └─[36mState[0m
    │ │ │   ├─[36mParameter[0m
    │ │ │   │ └─[36mDecl[0m
    │ │ │   │   ├─[36mName: [0m[33mdone[0m
    │ │ │   │   └─[36mType: [0m[32mSet of Task[0m
    │ │ │   ├─[36mConst[0m
    │ │ │   │ └─[36mfalse[0m
    │ │ │   └─[36mExpression[0m
    │ │ │     └─[36mNone[0m
    │ │ ├─[36mActions[0m
    │ │ │ ├─[36mAction[0m
    │ │ │ │ ├─[36mName: [0m[33madd[0m
    │ │ │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │ │ ├─[36mDeclList[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33mt[0m
    │ │ │ │ │   └─[36mType: [0m[32mTask[0m
    │ │ │ │ ├─[36mFiring Condition[0m
    │ │ │ │ │ └─[36mBinop[0m
    │ │ │ │ │   ├─[34mNotIn[0m
    │ │ │ │ │   ├─[36mVar:[0m
    │ │ │ │ │   │ ├─[33mt[0m
    │ │ │ │ │   │ └─[32mTask[0m
    │ │ │ │ │   ├─[36mBinop[0m
    │ │ │ │ │   │ ├─[34mPlus[0m
    │ │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │ │   │ │ ├─[33mdone[0m
    │ │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │ │   │ │ ├─[33mpending[0m
    │ │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │ │   │ └─[32mSet of Task[0m
    │ │ │ │ │   └─[32mBool[0m
    │ │ │ │ └─[36mBody[0m
    │ │ │ │   ├─[36mStatements[0m
    │ │ │ │   └─[36mAssignment[0m
    │ │ │ │     ├─[36mVar:[0m
    │ │ │ │     │ ├─[33mpending[0m
    │ │ │ │     │ └─[32mSet of Task[0m
    │ │ │ │     ├─[36mBinop[0m
    │ │ │ │     │ ├─[34mPlus[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mpending[0m
    │ │ │ │     │ │ └─[32mSet of Task[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mt[0m
    │ │ │ │     │ │ └─[32mTask[0m
    │ │ │ │     │ └─[32mSet of Task[0m
    │ │ │ │     └─[32mSet of Task[0m
    │ │ │ ├─[36mAction[0m
    │ │ │ │ ├─[36mName: [0m[33mdelete[0m
    │ │ │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │ │ ├─[36mDeclList[0m
    │ │ │ │ │ └─[36mDecl[0m
    │ │ │ │ │   ├─[36mName: [0m[33mt[0m
    │ │ │ │ │   └─[36mType: [0m[32mTask[0m
    │ │ │ │ ├─[36mFiring Condition[0m
    │ │ │ │ │ └─[36mBinop[0m
    │ │ │ │ │   ├─[34mIn[0m
    │ │ │ │ │   ├─[36mVar:[0m
    │ │ │ │ │   │ ├─[33mt[0m
    │ │ │ │ │   │ └─[32mTask[0m
    │ │ │ │ │   ├─[36mBinop[0m
    │ │ │ │ │   │ ├─[34mPlus[0m
    │ │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │ │   │ │ ├─[33mdone[0m
    │ │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │ │   │ │ ├─[33mpending[0m
    │ │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │ │   │ └─[32mSet of Task[0m
    │ │ │ │ │   └─[32mBool[0m
    │ │ │ │ └─[36mBody[0m
    │ │ │ │   ├─[36mStatements[0m
    │ │ │ │   ├─[36mAssignment[0m
    │ │ │ │   │ ├─[36mVar:[0m
    │ │ │ │   │ │ ├─[33mdone[0m
    │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │   │ ├─[36mBinop[0m
    │ │ │ │   │ │ ├─[34mMinus[0m
    │ │ │ │   │ │ ├─[36mVar:[0m
    │ │ │ │   │ │ │ ├─[33mdone[0m
    │ │ │ │   │ │ │ └─[32mSet of Task[0m
    │ │ │ │   │ │ ├─[36mVar:[0m
    │ │ │ │   │ │ │ ├─[33mt[0m
    │ │ │ │   │ │ │ └─[32mTask[0m
    │ │ │ │   │ │ └─[32mSet of Task[0m
    │ │ │ │   │ └─[32mSet of Task[0m
    │ │ │ │   └─[36mAssignment[0m
    │ │ │ │     ├─[36mVar:[0m
    │ │ │ │     │ ├─[33mpending[0m
    │ │ │ │     │ └─[32mSet of Task[0m
    │ │ │ │     ├─[36mBinop[0m
    │ │ │ │     │ ├─[34mMinus[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mpending[0m
    │ │ │ │     │ │ └─[32mSet of Task[0m
    │ │ │ │     │ ├─[36mVar:[0m
    │ │ │ │     │ │ ├─[33mt[0m
    │ │ │ │     │ │ └─[32mTask[0m
    │ │ │ │     │ └─[32mSet of Task[0m
    │ │ │ │     └─[32mSet of Task[0m
    │ │ │ └─[36mAction[0m
    │ │ │   ├─[36mName: [0m[33mcomplete[0m
    │ │ │   ├─[36mReturn Type: [0m[36mNone[0m
    │ │ │   ├─[36mDeclList[0m
    │ │ │   │ └─[36mDecl[0m
    │ │ │   │   ├─[36mName: [0m[33mt[0m
    │ │ │   │   └─[36mType: [0m[32mTask[0m
    │ │ │   ├─[36mFiring Condition[0m
    │ │ │   │ └─[36mBinop[0m
    │ │ │   │   ├─[34mIn[0m
    │ │ │   │   ├─[36mVar:[0m
    │ │ │   │   │ ├─[33mt[0m
    │ │ │   │   │ └─[32mTask[0m
    │ │ │   │   ├─[36mVar:[0m
    │ │ │   │   │ ├─[33mpending[0m
    │ │ │   │   │ └─[32mSet of Task[0m
    │ │ │   │   └─[32mBool[0m
    │ │ │   └─[36mBody[0m
    │ │ │     ├─[36mStatements[0m
    │ │ │     ├─[36mAssignment[0m
    │ │ │     │ ├─[36mVar:[0m
    │ │ │     │ │ ├─[33mpending[0m
    │ │ │     │ │ └─[32mSet of Task[0m
    │ │ │     │ ├─[36mBinop[0m
    │ │ │     │ │ ├─[34mMinus[0m
    │ │ │     │ │ ├─[36mVar:[0m
    │ │ │     │ │ │ ├─[33mpending[0m
    │ │ │     │ │ │ └─[32mSet of Task[0m
    │ │ │     │ │ ├─[36mVar:[0m
    │ │ │     │ │ │ ├─[33mt[0m
    │ │ │     │ │ │ └─[32mTask[0m
    │ │ │     │ │ └─[32mSet of Task[0m
    │ │ │     │ └─[32mSet of Task[0m
    │ │ │     └─[36mAssignment[0m
    │ │ │       ├─[36mVar:[0m
    │ │ │       │ ├─[33mdone[0m
    │ │ │       │ └─[32mSet of Task[0m
    │ │ │       ├─[36mBinop[0m
    │ │ │       │ ├─[34mPlus[0m
    │ │ │       │ ├─[36mVar:[0m
    │ │ │       │ │ ├─[33mdone[0m
    │ │ │       │ │ └─[32mSet of Task[0m
    │ │ │       │ ├─[36mVar:[0m
    │ │ │       │ │ ├─[33mt[0m
    │ │ │       │ │ └─[32mTask[0m
    │ │ │       │ └─[32mSet of Task[0m
    │ │ │       └─[32mSet of Task[0m
    │ │ └─[36mOP[0m
    │ │   ├─[36mPrinciple[0m
    │ │   │ └─[36mBinop[0m
    │ │   │   ├─[34mThen[0m
    │ │   │   ├─[36mCall[0m
    │ │   │   │ ├─[33madd[0m
    │ │   │   │ ├─[36mArgs[0m
    │ │   │   │ │ └─[36mArg[0m
    │ │   │   │ │   ├─[36mVar:[0m
    │ │   │   │ │     ├─[33mt[0m
    │ │   │   │ │     └─[32mTask[0m
    │ │   │   │ └─[32mBool[0m
    │ │   │   ├─[36mBinop[0m
    │ │   │   │ ├─[34mUntil[0m
    │ │   │   │ ├─[36mBinop[0m
    │ │   │   │ │ ├─[34mIn[0m
    │ │   │   │ │ ├─[36mVar:[0m
    │ │   │   │ │ │ ├─[33mt[0m
    │ │   │   │ │ │ └─[32mTask[0m
    │ │   │   │ │ ├─[36mVar:[0m
    │ │   │   │ │ │ ├─[33mpending[0m
    │ │   │   │ │ │ └─[32mSet of Task[0m
    │ │   │   │ │ └─[32mBool[0m
    │ │   │   │ ├─[36mBinop[0m
    │ │   │   │ │ ├─[34mLor[0m
    │ │   │   │ │ ├─[36mCall[0m
    │ │   │   │ │ │ ├─[33mdelete[0m
    │ │   │   │ │ │ ├─[36mArgs[0m
    │ │   │   │ │ │ │ └─[36mArg[0m
    │ │   │   │ │ │ │   ├─[36mVar:[0m
    │ │   │   │ │ │ │     ├─[33mt[0m
    │ │   │   │ │ │ │     └─[32mTask[0m
    │ │   │   │ │ │ └─[32mBool[0m
    │ │   │   │ │ ├─[36mCall[0m
    │ │   │   │ │ │ ├─[33mcomplete[0m
    │ │   │   │ │ │ ├─[36mArgs[0m
    │ │   │   │ │ │ │ └─[36mArg[0m
    │ │   │   │ │ │ │   ├─[36mVar:[0m
    │ │   │   │ │ │ │     ├─[33mt[0m
    │ │   │   │ │ │ │     └─[32mTask[0m
    │ │   │   │ │ │ └─[32mBool[0m
    │ │   │   │ │ └─[32mBool[0m
    │ │   │   │ └─[32mBool[0m
    │ │   │   └─[32mBool[0m
    │ │   └─[36mPrinciple[0m
    │ │     └─[36mBinop[0m
    │ │       ├─[34mThen[0m
    │ │       ├─[36mCall[0m
    │ │       │ ├─[33mcomplete[0m
    │ │       │ ├─[36mArgs[0m
    │ │       │ │ └─[36mArg[0m
    │ │       │ │   ├─[36mVar:[0m
    │ │       │ │     ├─[33mt[0m
    │ │       │ │     └─[32mTask[0m
    │ │       │ └─[32mBool[0m
    │ │       ├─[36mBinop[0m
    │ │       │ ├─[34mUntil[0m
    │ │       │ ├─[36mBinop[0m
    │ │       │ │ ├─[34mIn[0m
    │ │       │ │ ├─[36mVar:[0m
    │ │       │ │ │ ├─[33mt[0m
    │ │       │ │ │ └─[32mTask[0m
    │ │       │ │ ├─[36mVar:[0m
    │ │       │ │ │ ├─[33mdone[0m
    │ │       │ │ │ └─[32mSet of Task[0m
    │ │       │ │ └─[32mBool[0m
    │ │       │ ├─[36mCall[0m
    │ │       │ │ ├─[33mdelete[0m
    │ │       │ │ ├─[36mArgs[0m
    │ │       │ │ │ └─[36mArg[0m
    │ │       │ │ │   ├─[36mVar:[0m
    │ │       │ │ │     ├─[33mt[0m
    │ │       │ │ │     └─[32mTask[0m
    │ │       │ │ └─[32mBool[0m
    │ │       │ └─[32mBool[0m
    │ │       └─[32mBool[0m
    │ └─[36mConcept[0m
    │   ├─[36mSignature[0m
    │   │ ├─[33memail[0m
    │   │ └─[36mParameterList[0m
    │   │   └─[36mEmpty[0m
    │   ├─[36mPurpose[0m
    │   │ └─   communicate with private messages
    │   ├─[36mStates[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mcontent[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mMessage[0m
    │   │ │ │           └─[32mContent[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mto[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mMessage[0m
    │   │ │ │           └─[32mUser[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ ├─[36mState[0m
    │   │ │ ├─[36mParameter[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mfrom[0m
    │   │ │ │   └─[36mType: [0m[32mMap[0m
    │   │ │ │           ├─[32mMessage[0m
    │   │ │ │           └─[32mUser[0m
    │   │ │ ├─[36mConst[0m
    │   │ │ │ └─[36mfalse[0m
    │   │ │ └─[36mExpression[0m
    │   │ │   └─[36mNone[0m
    │   │ └─[36mState[0m
    │   │   ├─[36mParameter[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33minbox[0m
    │   │   │   └─[36mType: [0m[32mMap[0m
    │   │   │           ├─[32mUser[0m
    │   │   │           └─[32mSet of Message[0m
    │   │   ├─[36mConst[0m
    │   │   │ └─[36mfalse[0m
    │   │   └─[36mExpression[0m
    │   │     └─[36mNone[0m
    │   ├─[36mActions[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33msend[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mby[0m
    │   │ │ │ │ └─[36mType: [0m[32mUser[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mt[0m
    │   │ │ │ │ └─[36mType: [0m[32mUser[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mm[0m
    │   │ │ │ │ └─[36mType: [0m[32mMessage[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mc[0m
    │   │ │ │   └─[36mType: [0m[32mContent[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mNotIn[0m
    │   │ │ │   ├─[36mVar:[0m
    │   │ │ │   │ ├─[33mm[0m
    │   │ │ │   │ └─[32mMessage[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mJoin[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mt[0m
    │   │ │ │   │ │ └─[32mUser[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33minbox[0m
    │   │ │ │   │ │ └─[32mMap[0m
    │   │ │ │   │ │   ├─[32mUser[0m
    │   │ │ │   │ │   └─[32mSet of Message[0m
    │   │ │ │   │ └─[32mSet of Message[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mm[0m
    │   │ │   │ │ │ └─[32mMessage[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mcontent[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mMessage[0m
    │   │ │   │ │ │   └─[32mContent[0m
    │   │ │   │ │ └─[32mContent[0m
    │   │ │   │ ├─[36mVar:[0m
    │   │ │   │ │ ├─[33mc[0m
    │   │ │   │ │ └─[32mContent[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mMessage[0m
    │   │ │   │   └─[32mContent[0m
    │   │ │   ├─[36mAssignment[0m
    │   │ │   │ ├─[36mRelation:[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mm[0m
    │   │ │   │ │ │ └─[32mMessage[0m
    │   │ │   │ │ ├─[36mVar:[0m
    │   │ │   │ │ │ ├─[33mfrom[0m
    │   │ │   │ │ │ └─[32mMap[0m
    │   │ │   │ │ │   ├─[32mMessage[0m
    │   │ │   │ │ │   └─[32mUser[0m
    │   │ │   │ │ └─[32mUser[0m
    │   │ │   │ ├─[36mVar:[0m
    │   │ │   │ │ ├─[33mby[0m
    │   │ │   │ │ └─[32mUser[0m
    │   │ │   │ └─[32mMap[0m
    │   │ │   │   ├─[32mMessage[0m
    │   │ │   │   └─[32mUser[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mm[0m
    │   │ │     │ │ └─[32mMessage[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mto[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mMessage[0m
    │   │ │     │ │   └─[32mUser[0m
    │   │ │     │ └─[32mUser[0m
    │   │ │     ├─[36mVar:[0m
    │   │ │     │ ├─[33mt[0m
    │   │ │     │ └─[32mUser[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mMessage[0m
    │   │ │       └─[32mUser[0m
    │   │ ├─[36mAction[0m
    │   │ │ ├─[36mName: [0m[33mreceive[0m
    │   │ │ ├─[36mReturn Type: [0m[36mNone[0m
    │   │ │ ├─[36mDeclList[0m
    │   │ │ │ ├─[36mDecl[0m
    │   │ │ │ │ ├─[36mName: [0m[33mby[0m
    │   │ │ │ │ └─[36mType: [0m[32mUser[0m
    │   │ │ │ └─[36mDecl[0m
    │   │ │ │   ├─[36mName: [0m[33mm[0m
    │   │ │ │   └─[36mType: [0m[32mMessage[0m
    │   │ │ ├─[36mFiring Condition[0m
    │   │ │ │ └─[36mBinop[0m
    │   │ │ │   ├─[34mLand[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mNotIn[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mm[0m
    │   │ │ │   │ │ └─[32mMessage[0m
    │   │ │ │   │ ├─[36mBinop[0m
    │   │ │ │   │ │ ├─[34mJoin[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33mby[0m
    │   │ │ │   │ │ │ └─[32mUser[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33minbox[0m
    │   │ │ │   │ │ │ └─[32mMap[0m
    │   │ │ │   │ │ │   ├─[32mUser[0m
    │   │ │ │   │ │ │   └─[32mSet of Message[0m
    │   │ │ │   │ │ └─[32mSet of Message[0m
    │   │ │ │   │ └─[32mBool[0m
    │   │ │ │   ├─[36mBinop[0m
    │   │ │ │   │ ├─[34mEq[0m
    │   │ │ │   │ ├─[36mBinop[0m
    │   │ │ │   │ │ ├─[34mJoin[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33mm[0m
    │   │ │ │   │ │ │ └─[32mMessage[0m
    │   │ │ │   │ │ ├─[36mVar:[0m
    │   │ │ │   │ │ │ ├─[33mto[0m
    │   │ │ │   │ │ │ └─[32mMap[0m
    │   │ │ │   │ │ │   ├─[32mMessage[0m
    │   │ │ │   │ │ │   └─[32mUser[0m
    │   │ │ │   │ │ └─[32mUser[0m
    │   │ │ │   │ ├─[36mVar:[0m
    │   │ │ │   │ │ ├─[33mby[0m
    │   │ │ │   │ │ └─[32mUser[0m
    │   │ │ │   │ └─[32mBool[0m
    │   │ │ │   └─[32mBool[0m
    │   │ │ └─[36mBody[0m
    │   │ │   ├─[36mStatements[0m
    │   │ │   └─[36mAssignment[0m
    │   │ │     ├─[36mRelation:[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mby[0m
    │   │ │     │ │ └─[32mUser[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33minbox[0m
    │   │ │     │ │ └─[32mMap[0m
    │   │ │     │ │   ├─[32mUser[0m
    │   │ │     │ │   └─[32mSet of Message[0m
    │   │ │     │ └─[32mSet of Message[0m
    │   │ │     ├─[36mBinop[0m
    │   │ │     │ ├─[34mPlus[0m
    │   │ │     │ ├─[36mRelation:[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33mby[0m
    │   │ │     │ │ │ └─[32mUser[0m
    │   │ │     │ │ ├─[36mVar:[0m
    │   │ │     │ │ │ ├─[33minbox[0m
    │   │ │     │ │ │ └─[32mMap[0m
    │   │ │     │ │ │   ├─[32mUser[0m
    │   │ │     │ │ │   └─[32mSet of Message[0m
    │   │ │     │ │ └─[32mSet of Message[0m
    │   │ │     │ ├─[36mVar:[0m
    │   │ │     │ │ ├─[33mm[0m
    │   │ │     │ │ └─[32mMessage[0m
    │   │ │     │ └─[32mSet of Message[0m
    │   │ │     └─[32mMap[0m
    │   │ │       ├─[32mUser[0m
    │   │ │       └─[32mSet of Message[0m
    │   │ └─[36mAction[0m
    │   │   ├─[36mName: [0m[33mdelete[0m
    │   │   ├─[36mReturn Type: [0m[36mNone[0m
    │   │   ├─[36mDeclList[0m
    │   │   │ ├─[36mDecl[0m
    │   │   │ │ ├─[36mName: [0m[33mu[0m
    │   │   │ │ └─[36mType: [0m[32mUser[0m
    │   │   │ └─[36mDecl[0m
    │   │   │   ├─[36mName: [0m[33mm[0m
    │   │   │   └─[36mType: [0m[32mMessage[0m
    │   │   ├─[36mFiring Condition[0m
    │   │   │ └─[36mBinop[0m
    │   │   │   ├─[34mIn[0m
    │   │   │   ├─[36mVar:[0m
    │   │   │   │ ├─[33mm[0m
    │   │   │   │ └─[32mMessage[0m
    │   │   │   ├─[36mBinop[0m
    │   │   │   │ ├─[34mJoin[0m
    │   │   │   │ ├─[36mVar:[0m
    │   │   │   │ │ ├─[33mu[0m
    │   │   │   │ │ └─[32mUser[0m
    │   │   │   │ ├─[36mVar:[0m
    │   │   │   │ │ ├─[33minbox[0m
    │   │   │   │ │ └─[32mMap[0m
    │   │   │   │ │   ├─[32mUser[0m
    │   │   │   │ │   └─[32mSet of Message[0m
    │   │   │   │ └─[32mSet of Message[0m
    │   │   │   └─[32mBool[0m
    │   │   └─[36mBody[0m
    │   │     ├─[36mStatements[0m
    │   │     ├─[36mAssignment[0m
    │   │     │ ├─[36mRelation:[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mu[0m
    │   │     │ │ │ └─[32mUser[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33minbox[0m
    │   │     │ │ │ └─[32mMap[0m
    │   │     │ │ │   ├─[32mUser[0m
    │   │     │ │ │   └─[32mSet of Message[0m
    │   │     │ │ └─[32mSet of Message[0m
    │   │     │ ├─[36mBinop[0m
    │   │     │ │ ├─[34mMinus[0m
    │   │     │ │ ├─[36mRelation:[0m
    │   │     │ │ │ ├─[36mVar:[0m
    │   │     │ │ │ │ ├─[33mu[0m
    │   │     │ │ │ │ └─[32mUser[0m
    │   │     │ │ │ ├─[36mVar:[0m
    │   │     │ │ │ │ ├─[33minbox[0m
    │   │     │ │ │ │ └─[32mMap[0m
    │   │     │ │ │ │   ├─[32mUser[0m
    │   │     │ │ │ │   └─[32mSet of Message[0m
    │   │     │ │ │ └─[32mSet of Message[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mm[0m
    │   │     │ │ │ └─[32mMessage[0m
    │   │     │ │ └─[32mSet of Message[0m
    │   │     │ └─[32mMap[0m
    │   │     │   ├─[32mUser[0m
    │   │     │   └─[32mSet of Message[0m
    │   │     ├─[36mAssignment[0m
    │   │     │ ├─[36mRelation:[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mm[0m
    │   │     │ │ │ └─[32mMessage[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mfrom[0m
    │   │     │ │ │ └─[32mMap[0m
    │   │     │ │ │   ├─[32mMessage[0m
    │   │     │ │ │   └─[32mUser[0m
    │   │     │ │ └─[32mUser[0m
    │   │     │ ├─[36mEmptySet[0m
    │   │     │ │ └─[32mNull[0m
    │   │     │ └─[32mMap[0m
    │   │     │   ├─[32mMessage[0m
    │   │     │   └─[32mUser[0m
    │   │     ├─[36mAssignment[0m
    │   │     │ ├─[36mRelation:[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mm[0m
    │   │     │ │ │ └─[32mMessage[0m
    │   │     │ │ ├─[36mVar:[0m
    │   │     │ │ │ ├─[33mto[0m
    │   │     │ │ │ └─[32mMap[0m
    │   │     │ │ │   ├─[32mMessage[0m
    │   │     │ │ │   └─[32mUser[0m
    │   │     │ │ └─[32mUser[0m
    │   │     │ ├─[36mEmptySet[0m
    │   │     │ │ └─[32mNull[0m
    │   │     │ └─[32mMap[0m
    │   │     │   ├─[32mMessage[0m
    │   │     │   └─[32mUser[0m
    │   │     └─[36mAssignment[0m
    │   │       ├─[36mRelation:[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mm[0m
    │   │       │ │ └─[32mMessage[0m
    │   │       │ ├─[36mVar:[0m
    │   │       │ │ ├─[33mcontent[0m
    │   │       │ │ └─[32mMap[0m
    │   │       │ │   ├─[32mMessage[0m
    │   │       │ │   └─[32mContent[0m
    │   │       │ └─[32mContent[0m
    │   │       ├─[36mEmptySet[0m
    │   │       │ └─[32mNull[0m
    │   │       └─[32mMap[0m
    │   │         ├─[32mMessage[0m
    │   │         └─[32mContent[0m
    │   └─[36mOP[0m
    │     └─[36mPrinciple[0m
    │       └─[36mBinop[0m
    │         ├─[34mThen[0m
    │         ├─[36mCall[0m
    │         │ ├─[33msend[0m
    │         │ ├─[36mArgs[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33mb[0m
    │         │ │ │   └─[32mUser[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33mt[0m
    │         │ │ │   └─[32mUser[0m
    │         │ │ ├─[36mArg[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │   ├─[33mm[0m
    │         │ │ │   └─[32mMessage[0m
    │         │ │ └─[36mArg[0m
    │         │ │   ├─[36mVar:[0m
    │         │ │     ├─[33mc[0m
    │         │ │     └─[32mContent[0m
    │         │ └─[32mBool[0m
    │         ├─[36mBinop[0m
    │         │ ├─[34mThen[0m
    │         │ ├─[36mCall[0m
    │         │ │ ├─[33mreceive[0m
    │         │ │ ├─[36mArgs[0m
    │         │ │ │ ├─[36mArg[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │   ├─[33mt[0m
    │         │ │ │ │   └─[32mUser[0m
    │         │ │ │ └─[36mArg[0m
    │         │ │ │   ├─[36mVar:[0m
    │         │ │ │     ├─[33mm[0m
    │         │ │ │     └─[32mMessage[0m
    │         │ │ └─[32mBool[0m
    │         │ ├─[36mBinop[0m
    │         │ │ ├─[34mLand[0m
    │         │ │ ├─[36mBinop[0m
    │         │ │ │ ├─[34mIn[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ ├─[33mm[0m
    │         │ │ │ │ └─[32mMessage[0m
    │         │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ ├─[34mJoin[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ ├─[33mt[0m
    │         │ │ │ │ │ └─[32mUser[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ ├─[33minbox[0m
    │         │ │ │ │ │ └─[32mMap[0m
    │         │ │ │ │ │   ├─[32mUser[0m
    │         │ │ │ │ │   └─[32mSet of Message[0m
    │         │ │ │ │ └─[32mSet of Message[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ ├─[36mBinop[0m
    │         │ │ │ ├─[34mEq[0m
    │         │ │ │ ├─[36mBinop[0m
    │         │ │ │ │ ├─[34mJoin[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ ├─[33mm[0m
    │         │ │ │ │ │ └─[32mMessage[0m
    │         │ │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ │ ├─[33mcontent[0m
    │         │ │ │ │ │ └─[32mMap[0m
    │         │ │ │ │ │   ├─[32mMessage[0m
    │         │ │ │ │ │   └─[32mContent[0m
    │         │ │ │ │ └─[32mContent[0m
    │         │ │ │ ├─[36mVar:[0m
    │         │ │ │ │ ├─[33mc[0m
    │         │ │ │ │ └─[32mContent[0m
    │         │ │ │ └─[32mBool[0m
    │         │ │ └─[32mBool[0m
    │         │ └─[32mBool[0m
    │         └─[32mBool[0m
    └─[36mApps[0m
      └─[36mApp[0m
        ├─[33mtodo_label_mail[0m
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
        └─[36mSyncs[0m
          ├─[36mSync[0m
          │ ├─[36mSyncCall[0m
          │ │ ├─[33mtodo[0m
          │ │ └─[36mCall[0m
          │ │   ├─[33mdelete[0m
          │ │   ├─[36mArgs[0m
          │ │   │ └─[36mArg[0m
          │ │   │   ├─[36mVar:[0m
          │ │   │     ├─[33mt[0m
          │ │   │     └─[32mContent[0m
          │ │   └─[32mBool[0m
          │ └─[36mBody[0m
          │   └─[36mSyncCall[0m
          │     ├─[33mlabel[0m
          │     └─[36mCall[0m
          │       ├─[33mclear[0m
          │       ├─[36mArgs[0m
          │       │ └─[36mArg[0m
          │       │   ├─[36mVar:[0m
          │       │     ├─[33mt[0m
          │       │     └─[32mContent[0m
          │       └─[32mBool[0m
          └─[36mSync[0m
            ├─[36mSyncCall[0m
            │ ├─[33memail[0m
            │ └─[36mCall[0m
            │   ├─[33mreceive[0m
            │   ├─[36mArgs[0m
            │   │ ├─[36mArg[0m
            │   │ │ ├─[36mOne of [0m
            │   │ │ └─[36mVar:[0m
            │   │ │   ├─[33mtodo_user[0m
            │   │ │   └─[32mUser[0m
            │   │ └─[36mArg[0m
            │   │   ├─[36mVar:[0m
            │   │     ├─[33mm[0m
            │   │     └─[32mMessage[0m
            │   └─[32mBool[0m
            └─[36mBody[0m
              └─[36mSyncCall[0m
                ├─[33mtodo[0m
                └─[36mCall[0m
                  ├─[33madd[0m
                  ├─[36mArgs[0m
                  │ └─[36mArg[0m
                  │   ├─[36mBinop[0m
                  │     ├─[34mJoin[0m
                  │     ├─[36mVar:[0m
                  │     │ ├─[33mm[0m
                  │     │ └─[32mMessage[0m
                  │     ├─[36mVar:[0m
                  │     │ ├─[33mcontent[0m
                  │     │ └─[32mMap[0m
                  │     │   ├─[32mMessage[0m
                  │     │   └─[32mContent[0m
                  │     └─[32mContent[0m
                  └─[32mBool[0m |}] 
