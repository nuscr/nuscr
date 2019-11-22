let list =
  [ ( "parser/GMessage.scr"
    , "module scribble.examples.GMessage;\n\n\
       type <xsd> \"{http://www.acme.com/financial}Order\" from \
       \"http://www.acme.com/schemas/Order.xsd\" as Order;\n\
       type <xsd> \"{http://www.acme.com/financial}Customer\" from \
       \"http://www.acme.com/schemas/Customer.xsd\" as Customer;\n\n\
       global protocol GMessageTest(role Buyer,role SellerA,role SellerB) {\n\
       \tbuy(Order,Customer) from Buyer to SellerA;\n\
       \tbuy(Order,Customer) from Buyer to SellerA,SellerB;\n\
      \        (* it was buy(o:Order,c:Customer)... *)\n\
       \tbuy(Order, Customer) from Buyer to SellerA;\n\
       \tbuy(Order) from Buyer to SellerA,SellerB;\n\
       \tbuy(Order) from Buyer to SellerA;\n\
       }\n\n\
       global protocol PlaceOrder(role Buyer,role Seller) {\n\
       }\n\n\
       global protocol Receipt(role Buyer,role Seller) {\n\
       }\n" )
  ; ( "parser/first.scr"
    , "/* Scribble supports C-style comments.\n\
      \   Block comments cannot be nested. */\n\n\
       // also supports line comments\n\n\
       (* nuScr also supports ml style comments.\n\
      \   (* they can be nested *)\n\
      \   The overall comment only ends when all the comments end.\n\
       *)\n\n\
       (*) nuScr also adds new sml line comments\n\n\
       (* these comments can be put inside blocks\n\
      \   (*) without closing them\n\
       *)\n\n\
       module nuscr.examples.First;\n\n\
       (*) this defines a module, there's a module per file\n\n\n\
       type <xsd> \"{http://www.acme.com/financial}Order\" from \
       \"http://www.acme.com/schemas/Order.xsd\" as Order;\n\
       //type <xsd> \"{http://www.acme.com/financial}Customer\" from \
       \"http://www.acme.com/schemas/Customer.xsd\" as Customer;\n\n\n\
       global protocol GMessageTest(role Buyer,role SellerA,role SellerB) {\n\
       \tbuy(Order,Customer) from Buyer to SellerA;\n\
       \tbuy(Order,Customer) from Buyer to SellerA,SellerB;\n\
       (*) this next line doesn't seem to be in Scribble.g\n\
       (*)\tbuy(o:Order,c:Customer) from Buyer to SellerA;\n\
       \tbuy(Order,Customer) from Buyer to SellerA;\n\
       \tbuy(Order) from Buyer to SellerA,SellerB;\n\
       (*) this next line doesn't seem to be in Scribble.g\n\
       (*)\tbuy(ord:Order) from Buyer to SellerA;\n\
       }\n\n\
       global protocol PlaceOrder(role Buyer,role Seller) {\n\
       }\n\n\
       global protocol Receipt(role Buyer,role Seller) {\n\
       }\n" )
  ; ( "parser/GProtocol.scr"
    , "module scribble.examples.GProtocol;\n\n\
       global protocol GMessageTest(role Buyer,role SellerA,role SellerB) {\n\
       }\n\n\
       global protocol Order<sig Fred as Joe,type Jack>(role Buyer,role \
       Seller) {\n\
       }\n\n\
       (* in the Scribble distribution this protocol has Buyer as B but the\n\
      \   original parser does not support that*)\n\
       global protocol Receipt(role Buyer,role Seller) {\n\
       }\n\n\n\
       (*) these protocols with instantiates are also not in the parser\n\
       (*) global protocol GMessageTest2(role Buyer,role SellerA,role \
       SellerB) instantiates GMessageTest(Buyer,SellerA,SellerB);\n\n\
       (*) global protocol Order2<sig Fred as Joe,type Jack>(role \
       Buyer,role Seller) instantiates Order<hello(World),Fred as \
       Joe>(Buyer,Seller);\n" )
  ; ( "consensus/Clock.scr"
    , "module Clock;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol Lock(role A, role B, role C)\n\
       {\n\
       \tREQ (ts:int) from A to B;\n\
       \tREQ (ts:int) from A to C;\n\
       \tchoice at B\n\
       \t{\n\
       \t\tACK (ts:int) from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Lock(B, A, C);\n\
       \t}\n\n\
       \tchoice at C\n\
       \t{\n\
       \t\tACK (ts:int) from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Lock(C, A, B);\n\
       \t}\n\
       \t\n\
       \tREL () from A to B;\n\
       \tREL () from A to C;\n\
       }\n\n\n" )
  ; ( "consensus/ClockAnnotTR.scr"
    , "module ClockAnnotTR; //A tail recursive version of ClockAnnot.scr\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol Lock(role A, role B, role C)\n\
       {\n\
       \tREQ (ts_rab:int) from A to B;\n\
       \tREQ (ts_rac:int) from A to C; @\"ts_rac > ts_rab\"\n\
       \tchoice at B\n\
       \t{\n\
       \t\tACK (ts_aba:int) from B to A; @\"ts_aba >  ts_rab\"\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\tACK (ts_aca:int) from C to A; @\"ts_aca > ts_rac\"\n\
       \t\t\tREL (ts_reab:int) from A to B; @\"ts_reab > ts_rab\"\n\
       \t\t\tREL (ts_reac:int) from A to C; @\"ts_reac > ts_rac\"\t\t\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tdo Lock(C, A, B);\n\
       \t\t}\n\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Lock(B, A, C); // should we set the initial clocks here?\n\
       \t}\n\
       }\n" )
  ; ( "consensus/ClockTR.scr"
    , "module ClockTR; //A tail recursive version of Clock.scr\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol Lock(role A, role B, role C)\n\
       {\n\
      \    REQ (ts:int) from A to B;\n\
      \    REQ (ts:int) from A to C;\n\
      \    choice at B\n\
      \    {\n\
      \        ACK (ts:int) from B to A;\n\
      \        choice at C \n\
      \        {\n\
      \            ACK (ts:int) from C to A;\n\
      \            REL () from A to B;\n\
      \            REL () from A to C; \n\
      \        }\n\
      \        or\n\
      \        {\n\
      \            do Lock(C, A, B);\n\
      \        }\n\
      \    }\n\
      \    or\n\
      \    {\n\
      \        do Lock(B, A, C);\n\
      \    }\n\
       }\n\n\n" )
  ; ( "consensus/ClockAnnot.scr"
    , "module ClockAnnot;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol Lock(role A, role B, role C)\n\
       {\n\
       \tREQ (ts_rab:int) from A to B;\n\
       \tREQ (ts_rac:int) from A to C; @\"ts_rac > ts_rab\"\n\
       \tchoice at B\n\
       \t{\n\
       \t\tACK (ts_aba:int) from B to A; @\"ts_aba >  ts_rab\"\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Lock(B, A, C); // should we set the initial clocks here?\n\
       \t}\n\n\
       \tchoice at C\n\
       \t{\n\
       \t\tACK (ts_aca:int) from C to A; @\"ts_aca > ts_rac\"\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Lock(C, A, B);\n\
       \t}\n\
       \t\n\
       \tREL (ts_reab:int) from A to B; @\"ts_reab > ts_rab\"\n\
       \tREL (ts_reac:int) from A to C; @\"ts_reac > ts_rac\"\n\
       }\n" )
  ; ( "consensus/ClockAnnotRecTR.scr"
    , "module ClockAnnotRecTR; //A tail recursive version of \
       ClockAnnotRec.scr\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol Lock(role A, role B, role C) (tsa:int, tsb:int, \
       tsc:int) @\"(0,0,0)\"\n\
       {\n\
       \tREQ (ts_rab:int) from A to B; @\"ts_rab > tsa\"\n\
       \tREQ (ts_rac:int) from A to C; @\"ts_rac > ts_rab \"\n\
       \tchoice at B\n\
       \t{\n\
       \t\tACK (ts_aba:int) from B to A; @\"ts_aba > max(ts_rab, tsb)\"\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\tACK (ts_aca:int) from C to A; @\"ts_aca > max(ts_rac, tsc)\"\n\
       \t\t\tREL (ts_reab) from A to B; @\"ts_reab > ts_rab\"\n\
       \t\t\tREL (ts_reac) from A to C; @\"ts_reac > ts_rac\"\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tdo Lock(C, A, B); @\"(ts_rac+1,tsb,ts_aca+1)\" //new initial \
       values\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Lock(B, A, C); @\"(ts_rac+1, ts_aba+1, tsc)\" // new initial \
       values\n\
       \t}\n\n\
       }\n" )
  ; ( "consensus/ClockAnnotRec.scr"
    , "module ClockAnnotRec;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol Lock(role A, role B, role C) (tsa:int, tsb:int, \
       tsc:int)@\"(0,0,0)\"\n\
       {\n\
       \tREQ (ts_rab:int) from A to B; @\"ts_rab > tsa\"\n\
       \tREQ (ts_rac:int) from A to C; @\"ts_rac > ts_rab \"\n\
       \tchoice at B\n\
       \t{\n\
       \t\tACK (ts_aba:int) from B to A; @\"ts_aba > max(ts_rab, tsb)\"\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Lock(B, A, C); @\"(ts_rac+1, ts_aba+1, tsc)\" // new initial \
       values\n\
       \t}\n\n\
       \tchoice at C\n\
       \t{\n\
       \t\tACK (ts_aca:int) from C to A; @\"ts_aca > max(ts_rac, tsc)\"\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Lock(C, A, B); @\"(ts_rac+1,tsb,ts_aca+1)\" //new initial \
       values\n\
       \t}\n\
       \t\n\
       \tREL (ts_reab:int) from A to B; @\"ts_reab > ts_rab\"\n\
       \tREL (ts_reac:int) from A to C; @\"ts_reac > ts_rac\"\n\
       }\n" )
  ; ( "from-scribble-java/tmp/Quirk.scr"
    , "module Quirk;\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from C to D;  // Enabling violation once unfolded\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;  // Not standard mergeable -- and cannot factor \
       out syntactically\n\
       \t\t4() from C to D;\n\
       \t\t5() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t6() from C to D;\n\
       \t\t7() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tchoice at B\n\
       \t\t\t{\n\
       \t\t\t\t1() from B to C;\n\
       \t\t\t\t//continue X;  // Non-enabled A under this choice at B\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t3() from B to C;\n\
       \t\t\t\t//continue X;\n\
       \t\t\t}\n\
       \t\t\tcontinue X;  // Scribble needs \"sequencing\"\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from B to C;\n\
       \t\tchoice at A  // Not mergeable by, e.g., ICALP13\n\
       \t\t{\n\
       \t\t\t4() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t5() from A to B;\n\
       \t\t\t2() from B to C;  // An \"extra iteration\" between B-C (but \
       not A-B), but fine  // Generates non-det B?2's for C to different \
       states, but OK\t\t\n\
       \t\t\t3() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int1;\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int2;\n\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t//1() from A to B;  // Uncomment still bad\n\
       \t\t1(Int1) from A to B;\n\
       \t\t2() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1(Int2) from A to B;  // here, payloads aren't considered as \
       distinguishing for labels\n\
       \t\t                      // error is the special case of actions \
       that are non-deterministic labels up to payloads\n\
       \t\t                      // (Syntactic MPST merge not defined for \
       repeat labels, Cf. -oldwf)\n\
       \t\t3() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int1;\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int2;\n\n\n\
       // Cf. bad.safety.stuckmsg.payloads.Test01b\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2(Int1) from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2(Int2) from B to A;  // (Syntactic MPST merge not defined for \
       repeat labels)\n\
       \t\t\t\t\t\t\t\t\t\t\t\t\t// here, payloads are considered as \
       distinguishing (as default) -- whole I/O action used as label\n\
       \t\t\t\t\t\t\t\t\t\t\t\t\t// Cf. Test01b, non-deterministic actions \
       of the same state\n\
       \t\t                      // here, actions on different states -- \
       error manifests as regular stuck\n\
       \t}\n\
       }\n\
       //*/\n\n" )
  ; ( "from-scribble-java/tmp/Test2.scr"
    , "//Raymond@HZHL3 ~/code/scribble-java/scribble-java\n\
       //$ java -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar' \
       org.scribble.cli.CommandLine -path \
       modules/validation/src/test/scrib/src/ -validate Test\n\n\
       module Test2;\n\n\n\
       import Test;\n\n\n\
       /*global protocol Proto(role C, role D)\n\
       {\n\
       \t2() from C to D;\n\
       }*/\n\n\n\
       /*type <java> \"java.lang.String\" from \"rt.jar\" as String2;\n\n\
       //*\n\
       global protocol Proto2(role C, role D)\n\
       {\n\
       \tTest2() from C to D;\n\
       }\n\
       //*/\n\n\n\
       //*\n\
       import test3.Test3 as Test3;  // The name Test3 is Test3 in Test1\n\
       import Test4;                 // The name Test4 is test3.Test3 in \
       Test1\n\
       import Test4 as T4;\n\n\
       global protocol Proto2(role C, role D)\n\
       {\n\
       \t2(Test3.TTT) from C to D;\n\
       \tdo Test3.Bar3(C, D);\n\
       \tdo Test4.Proto4(C, D);\n\
       \tdo T4.Proto4(C, D);\n\
       }\n\
       //*/\n\n\n\
       /*/\n\
       global protocol Foo(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t//do Bar(A, B);\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t3() from A to C;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t4() from A to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Bar(role C, role D)\n\
       {\n\
       \t3() from C to D;\n\
       \tdo Test.Foo(C, D);\n\
       }\n\n\
       //global protocol Foo2(role A, role B)\n\
       global protocol Foo(role A, role B)\n\
       {\n\
       \t4() from A to B;\n\
       }\n\
       //*/\n\n" )
  ; ( "from-scribble-java/tmp/Demo.scr"
    , "//$ bin/scribblec.sh modules/core/src/test/scrib/Test.scr -ip \
       modules/core/src/test/scrib/ -d modules/core/src/test/scrib/\n\n\n\
       module Demo;\n\n\n\
       /**\n\
      \ * Stuck message\n\
      \ */\n\n\
       /*\n\
       global protocol Proto(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to A;\n\
       \t\t//2() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /**\n\
      \ * Wait-for\n\
      \ */\n\n\
       /*\n\
       global protocol Proto(role A, role B, role C)\n\
       {\n\
       \t//rec X {\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t//continue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       explicit global protocol Proto(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \t//connect B to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t//connect A to C;  // Sync-blocked (cf. input-blocked, terminated)\n\
       \t\t1() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       explicit global protocol Proto(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tconnect A to C;\n\
       \t\t\t1() from A to C;\n\
       \t\t\t//continue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to A;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /**\n\
      \ * Orphans\n\
      \ */\n\n\
       /*\n\
       global protocol Proto(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /**\n\
      \ * \"Role liveness\"\n\
      \ */\n\n\
       /*\n\
       global protocol Proto(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from B to C;\n\
       \t\t//3() from C to A;\n\
       \t}\n\
       \t3() from C to A;\n\
       }\n\
       //*/\n\n\n\
       /**\n\
      \ * \"Message liveness\"\n\
      \ */\n\n\
       /*\n\
       global protocol Proto(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from B to C;\n\
       \t}\n\
       \t3() from A to C;\n\
       \t//3() from A to C;  // WF_1 will raise an \"incorrect\" \
       role-progress violation for A (but safely conservative? i.e. just \
       incompleteness of WF?)\n\
       \t//...  // Dragons: won't be visited via 1() ...but will be visited \
       via 2()? is that enough?\n\
       }\n\
       //*/\n\n\n\
       /**\n\
      \ *\n\
      \ */\n\n\
       /*\n\
       global protocol Proto(role A, role B, role C)\n\
       {\n\
       \t1() from A to B;\n\
       \tdo ProtoAux1(A, B, C);\n\
       }\n\n\
       aux global protocol ProtoAux1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\tdo ProtoAux2(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from A to B;\n\
       \t\t3() from B to C;\n\
       \t}\n\
       }\n\n\
       aux global protocol ProtoAux2(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\tdo ProtoAux1(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t5() from A to B;\n\
       \t\t5() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n" )
  ; ( "from-scribble-java/tmp/Fixme.scr"
    , "//$ bin/scribblec.sh modules/core/src/test/scrib/Fixme.scr -ip \
       modules/core/src/test/scrib/ -d modules/core/src/test/scrib/\n\n\
       //http://sandbox.kidstrythisathome.com/erdos/\n\n\n\
       module Fixme;\n\n\n\
       \t// TODO: (re)connect/wrap, e.g. SMTP\n\n\
       \t// TODO: \"introduces\" well-formedness?\n\
       \t// e.g. \"A from B to C;\"\n\
       \t// e.g. \"a from A to B; A[a] from B to C;\"\n\
       \t// e.g. connect A to B; connect B to C; connect C to A;  // \
       connect A to C would be bad\n\
       \t//.. expliconn+introduces vs. delegation -- MPST with explicit \
       (shared/sess) channels\n\n\
       \t// TODO: API gen\n\
       \t// - connect/disconnect -- need to make backwards compatible with \
       non-explicit\n\
       \t//   -- non-explicit just has implicit initial connects -- just \
       need to make sure no deadlock\n\
       \t//   -- consider accept for callbacks?\n\
       \t// - factor out Scribble abstract transport wrappers with \
       language-specific plugins; take as parameter for \"wrap\" action\n\
       \t// - non-det EFSMs -- minimisation will remove all mergeable \
       non-det actions, but non-mergeable non-det actions can exist due to \
       explicit connect\n\n\n\
       //.. output and connect can be mixed in a state... check where \
       getStateKind used for any problems\n\
       //    -- getTakeable in WFConfig for model building (but maybe ends \
       up OK, just treat connect/accept the same way?)\n\
       //\t\t-- main approach should be generally consier mixed \
       send/connect states, but differentiate connect for input blocking \
       detection\n\
       //.. check orphan messages wrt connect/accept states -- maybe OK, \
       orphan check does consider connectedness\n\n\n\
       // - scrib assertions for model checker -- check state visited?\n\
       // - parameterise subprotos on protos?\n\n\n\
       // FIXME: .. refactor interfaces for model \
       building/checking/minimising/etc into state classes (make subclasses \
       of Model/EndpointState -- cf. ast/del)\n\n\n\
       // ..is branching on accept+message more expressive than accept \
       followed by branch?  i.e. can connect always be \"factored up\" \
       before the choice? -- maybe depends on interplay with select \
       fairness (want to use connect to nest role scope inside choice case \
       to avoid assuming fairness, but this can conflict with non-det \
       accept?)\n\n\
       //*** TODO introduces -- or explicit shared channel/port creation \
       and passing (\"open\")\n\n\
       // DONE: delete noLive or add to fair check\n\
       // DONE: decouple branch API gen from I/O i/f's -- duplicate enum in \
       concrrete states? -- removed dependencies on I/O i/f's for Branch \
       APIs where I/O i/f generation is disabled\n\
       // DONE: I/O i/f name max length -- 255 Linux/Windows/etc max file \
       name length\n\n\
       // CHECKME: check for (not yet supported) deleg actions in API gen\n\
       // CHECKME: delegation type payload projection\n\
       // CHECKME: '.' in Name constructor args -- no: rename constructor \
       arg to simpname (cf SessionTypeFactory)\n\
       // MainContext.newJob -- move MainContext to core?\n\n\
       // FIXME: Module.getGlobal/LocalProtocolDecl\n\n\n\
       // TODO: test API gen for (fair) SupplierInfo\n\
       // TODO: test connect/wrap API gen for SMTP\n\
       // TODO: MT/EDP server examples\n\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from A to C;  // TODO: standard syntactic merge\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t5() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(Proto2@C) from A to B;   // FIXME: should not be aux? (or if aux \
       should check WF as root?)\n\
       }\n\n\
       aux global protocol Proto2(role C, role D)\n\
       {\n\
       \t1() from C to D;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto(role A, role B, role C, role D)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect C to D;\n\
       \t//... A, B  // FIXME: investigate\n\
       \t//... C, D\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto(role A, role B, role C, role D)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tconnect A to D;\n\
       \tconnect C to D;\n\
       \tdisconnect A and C;  // FIXME: investigate\n\
       \tdisconnect A and D;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tm1(Int) from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t\t//m4(int) from A to B;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tm2(Int) from A to B;\n\
       \t\t\t//m3(int) from A to B;\n\
       \t\t}\n\
       \t\t1() from C to D;  // FIXME: wrong enabled check result -- maybe \
       not? under unfolding it is captured inside the choice\n\
       \t}\n\
       \t//* /\n\
       }\n\
       //*/\n\n" )
  ; ( "from-scribble-java/tmp/Test.scr"
    , "//Raymond@HZHL3 ~/code/scribble-java/scribble-java\n\
       //$ bin/scribblec.sh -ip scribble-test/src/test/scrib/ -d \
       scribble-test/src/test/scrib/tmp/ \
       scribble-test/src/test/scrib/tmp/Test.scr\n\
       //$ bin/scribblec.sh -ip scribble-test/src/test/scrib/ -d \
       scribble-test/src/test/scrib/tmp/ \
       scribble-test/src/test/scrib/tmp/Test.scr -api Proto1 B\n\n\
       // OLD:\n\
       //$ java -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar' \
       org.scribble.cli.CommandLine -path modules/core/src/test/scrib/ \
       modules/core/src/test/scrib/Test.scr\n\
       //$ java -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar' \
       org.scribble2.cli.CommandLine -path \
       modules/validation/src/test/scrib/src \
       modules/validation/src/test/scrib/src/Test.scr\n\n\n\
       //http://sandbox.kidstrythisathome.com/erdos/\n\n\n\
       module tmp.Test;\n\n\
       // TODO: replace a lot of FACTORY name calls by clone\n\
       // FIXME: -minlts states IDs don't match validation output\n\n\
       // CHECKME: playing multiple roles in the same session instance (but \
       what is the concrete notion of \"process id\"? -- if actually try to \
       use, e.g., ThreadID, then have to track ownership of endpoints)\n\n\
       // - API gen for connection-with-message\n\
       // - API gen for wrap (SMTP)\n\
       // - API gen for shared channel passing\n\
       // - callback API needs generic session-state arg\n\n\
       // - Redo HTTP/SMTP message deserialization (and maybe \
       ScribMsgFormatter i/f) -- deserialization routines should be \
       per-message\n\n\
       // Job modes (full/default, fase17, oldscrib, MPST, etc.) -- factor \
       out GMConfig interface\n\n\
       // FIXME: demo.bettybook.math.MathC cancelledkey exception\n\n\
       // Add a unit test for validation to bypass aux\n\n\n\
       // TODO: port forwarding version of P2 -- funny case of \"session\" \
       persisting over full disconnect, should/how to correlate?\n\
       // FIXME: good.liveness.Test02, reachability check (empty rec \
       projection)\n\n\n\n\n\n\n\n\
       //-------------\n\
       //- drop unused recs -- partly done by inlining, but not done yet \
       for subprotos\n\n\
       //- projection dependencies (projections as modules?)\n\
       //- check projected role/choice/do pruning/fixing? -- applies only \
       to above proj? (i.e., proto decls) -- CHECKME: can do role fixing by \
       getting roles from iproj? similarly do-pruning?\n\n\
       //- connections\n\
       //- refactor ast to parser, refactor lang to type? -- separate \
       protocol classes?\n\
       //- or: ast/del to new lang -- core lang to type, except protocol\n\
       //- remove unused visitors and del code -- move new visitors there?\n\
       //- shorten class names (e.g., Protocol->Proto), rename parser \
       constants, revise ScribTreeAdaptor and DelDecorator\n\
       //- refactor model building\n\
       //- refactor do param/arg ast classes -- generally clean up AST \
       class hierarchy\n\
       //- refactor parsing and CL\n\
       //- check demos\n\
       \t\t\n\
       \t\t// TODO make test framework to check proj->inline matches \
       inline->proj\n\
       \t\t// TODO make test framework to check global-\"proj\"-via-taus \
       matches actual proj -- matches mean language acceptance?\n\
       \t\t// could also test once-unfold-all against model\n\n\n\
       // Done: -- parser/ast refactoring\n\
       \t// change parsed to in jobcontext to record original roledecls \
       (for GDo::project) -- and change getInlined on proto to prune (via \
       getRoles on def)\n\
       \t// separate Job into core and lang parts (mainly parsed Module)\n\
       \t// move ast/del, old visit and new frontend to lang\n\
       \t// integrate new frontend with MainContext?\n\
       \t// move lang types to type.sess, and move new visit\n\
       \t// try modulecontextvisitor by \"static lookup\"\n\
       \t// try move antlrwrapper to parser again\n\
       \t//for isSingleConts (and project, buildGraph), make T \
       visitWith(STypeReducer<K, B> v) ? -- roleenabling and extchoicecons \
       are more like reducers?\n\
       \t// refactor scribble.g, simplify ast/del/etc class names and \
       deprecated class code\n\
       \t//...remove Token args from af constructors -- removed, and re-added\n\
       \t//refactor reconstructs to use af? -- refactored af and \
       reconstruct to use addChildren1 -- FIXME\n\
       \t\t//...fix simp node constructors and af (no IdNode child) --- fix \
       ambignodel to use af -- separate af methods\n\
       \t// add exp conn checks to model validation, maybe just as \
       estatevisitor?  (cf. prev syntactic connection checks) -- now done \
       as global syntax visitor\n\n\n\
       // For model build/check refactoring:\n\n\
       \t\t// Outstanding:\n\
       \t\t\t// ...disallow/check for reserved __ -- do in gtype translation?\n\
       \t\t\t// FIXME: qualifed sig name as non-role do-arg\n\
       \t\t\t// TODO: rename addChildren1\n\
       \t\t\t// CHECKME: duplicate proto mods\n\
       \t\t\t// ...check ad hoc ScribNode constructions go through af \
       properly\n\
       \t\t\t// ...refactor -spin as an ext\n\
       \t\t\t// TODO: fix *imed*-projection (Job) choice subjects -- \
       inlined-projection is a single proto with all roles\n\n\
       // parser/ast re-refactoring\n\
       \t// use stypefactory in core -- also make visitorfactorys (both \
       core and ast)\n\
       \t// check G/LType casts vs. SType<Global/Local, ...> (favour latter)\n\
       \t// refactor Set<SState> to Map<Integer, SState>\n\
       \t\n\
       // Job/codegen refactoring:\n\
       \t// old style subproto visitor for imed-projection do-pruning -- \
       move imed-projs from Core to Job ?\n\
       \t// also do-arg pruning, for role-pruned projection decls\n\
       \t// choice-specific fairness annotations\n\
       \t// fix codegen when -fair\n\
       \t// CHECKME: accept branching on messages (and safety properties) \
       -- connection should not be established if message stuck? (cf. \
       reception error) -- issue is implementation: what is a realistic \
       semantics for sync-connect-branching (i.e., prevent client from \
       async progressing after messaging) -- consider reversing \
       client/server branching side? i.e. client requests, but then \
       receives? (and consider serverside-branching a sugar?)\n\n\
       // Later:\n\
       \t// re-check disamb pass -- e.g., do-target disamb done by \
       translation (CHECKME: DoDel disamb -- should use af?) -- check, \
       e.g., message transfer message disamb (signames, payloads)\n\
       \t\t\t// sort out full name expansion between disamb and translation \
       -- fix modulecontextcollector for imported module member fullnames\n\
       \t// refactor additional disamb syntactic checks into core?  e.g., \
       do-calls, distinct roledecls, protodecls, etc.\n\
       \t\t// -- otoh, ast errors give source feedback -- maybe implement \
       both\n\n\
       // Test refactoring:\n\
       \t// refactor test to use main instead of cli\n\n\n\
       data <java> \"...\" from \"...\" as T;\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t5() from C to D;\n\
       \t\tcontinue X;\n\
       \t}\n\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to A;  // Testing wait-for cycle detection  // Also \
       tests eventual reception\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;  // Testing global model building, non-det \
       edges (identical configs collapsed)\n\
       \t\t2() from A to B;  // Un/comment, test combinations\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t//3() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;  // Testing SGraph.getTraceFromInit, error in init \
       state (empty trace)\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\t\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to C;  // Same message OK\n\
       \t}\n\
       \tdo Proto1(A, B, C);\n\
       }\n\
       //*/\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\tconnect A to B;\n\
       \t\t1() from A to B;\n\
       \t\tdisconnect A and B;\n\
       \t}\t\n\
       \tor\n\
       \t{\n\
       \t\tconnect A to C;\n\
       \t\t1() from A to C;  // Same message OK\n\
       \t\tdisconnect A and C;\n\
       \t}\n\
       \tdo Proto1(A, B, C);\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;  // TODO: junit tests for explicit\n\
       \trec X\n\
       \t{\t\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\t\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\tconnect A to C;\n\
       \t\t\t3() from A to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X \n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \tchoice at B  // Testing this.collections remove in EGraphBuilderUtil\n\
       \t{\n\
       \t\t2() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tcontinue X;  // TODO: add junit test, testing inlined choice \
       subj fixing\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X \n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A  // TODO: add junit test, testing core inlined \
       projection choice subjects (for C) -- already exists?\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;  // TODO: add junit, testing -project C [-fair] \
       -- basically, Travel Agent\n\
       \t\tdo Proto1(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t//2() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t//3() from A to B;\n\
       \t}\n\
       \t//4() from B to A;\n\
       \tdo Proto1(A, B);\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}  // CHECKME: gives a non-det global model, correct/intended?\n\
       \t//4() from B to A;\n\
       \tdo Proto1(A, B);\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1<>(role A, role B)  // TODO: add unit test, \
       testing empty paramdecls\n\
       {\n\
       \t() from A to B;\n\
       \tdo Proto1<>(A, B);  // TODO: add unit test, testing empty \
       nonroleargs\n\
       }\n\
       //*/\n\n\
       /*\n\
       sig <java> \"...\" from \"...\" as M;\n\n\
       global protocol Proto1<sig MM>(role A, role B)\n\
       {\n\
       \tMM from A to B;\n\
       \tdo Proto1<Test.M>(A, B);  // TODO: add unit test, testing \
       qualified sig name as nonrole arg\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A__, role B)\n\
       {\n\
       \t() connect A__ to B;\n\
       \trec X\n\
       \t{\n\
       \t\t() from A__ to B;  // TODO: add double underscore check (e.g., \
       translation)\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1<sig M>(role A, role B)\n\
       {\n\
       \trec X {\n\
       \t//1() from A to B;\n\
       \t\t//1(Test.T) from A to B;\n\
       \t\t1(T) from A to B;\n\
       \t\tM from B to A;\n\
       \t\t//do Proto1(A, B);\n\
       \t\tcontinue X;\n\
       \t\t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Foo(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tdo FooAux(A, B, C);\n\
       }\n\n\
       aux explicit global protocol FooAux(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdo FooAux(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tconnect A to C;\n\
       \t\t3() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Foo(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tconnect A to C;\n\
       \t\t3() from A to C;\n\
       \t}\t\t\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Foo(role A, role B)\n\
       {\n\
       \t//connect A to B;\n\
       \t1() from A to B;\n\
       \tdisconnect A and B;\n\
       \tconnect A to B;\n\
       \t1() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Foo(role A, role B, role C)\n\
       {\n\
       \t1() from A to B;\t\n\
       \tdo Bar(A, C);\n\
       }\n\n\
       aux global protocol Bar(role D, role E)\n\
       {\n\
       \t2() from D to E;\t\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Foo(role A, role B, role C)\n\
       {\n\
       \t0() connect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdo Foo(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tfoo() connect A to C;\n\
       \t\t3() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\
       /*// Add unit test -- testing: inlining of shadowed recvars\n\
       global protocol Foo(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*  // Add unit test -- testing: repeat rec sigs in inlining due to \
       multilple do's, w.r.t. unfolding\n\
       global protocol Foo(role A, role B)\n\
       {\n\
       \tdo Bar(A, B);\n\
       \tdo Bar(A, B);\n\
       }\n\n\
       aux global protocol Bar(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*  // Add unit test -- testing: repeat rec sigs in inlining due to \
       multilple do's, w.r.t. unfolding\n\
       global protocol Foo(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\tdo Bar(A, B);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Bar(A, B);\n\
       \t}\n\
       }\n\n\
       aux global protocol Bar(role C, role D)\n\
       {\n\
       \t3() from C to D;\n\
       \tdo Bar(C, D);\n\
       }\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X \n\
       \t{\t\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\trec Y\n\
       \t\t\t{\n\
       \t\t\t\t0() from A to B;\n\
       \t\t\t\t//do Proto1(A, B);\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\tdo Proto1Aux(A, B);\n\
       \t\t//do Proto1(A, B);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Proto1Aux2(A, B);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\n\
       aux global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \t0() from A to B;\n\
       \tdo Proto1(A, B);\n\
       \t//do Proto1Aux(A, B);\n\
       }\n\n\
       global protocol Proto1Aux2(role A, role B)\n\
       {\n\
       \t3() from A to B;\n\
       \tdo Proto1Aux2(A, B);\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t0() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\t\n\
       \t\t\t//do Proto1(A, B);\n\
       \t\t\tcontinue X;\n\
       \t\t\t//3() from A to B;\t\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\t\n\
       \t\t}\n\
       \t\t//4() from A to B;\t\n\
       \t}\n\
       \t5() from A to B;\t\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t//1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t//2() from C to A;\n\
       \t\t//2() from A to B;\n\
       \t\t2() from A to C;\n\
       \t\t//2() from B to C;\n\
       \t}\t\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tdo Proto1Aux(A, B);\n\
       }\n\n\
       global protocol Proto1Aux(role AA, role BB)\n\
       {\n\
       \t() from AA to BB;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t0() from A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdo Proto1(A, B);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from A to B;\n\
       \t}\n\
       \t4() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\t\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\tdo Proto1(A, B);\t\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t3() from B to A;\n\
       \t\t\t//do Proto1(A, B);\n\
       \t\t\t//continue X;\n\
       \t\t\tdo Proto1(B, A);\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       //type <dotnet> \"System.UInt32\" from \"...\" as int;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tFoo() from A to B;\n\
       \t/*choice at A\n\
       \t{\t\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}* /\n\
       }\n\
       //*/\n\n\n\n\n\n\
       /*\n\
       type <dotnet> \"System.UInt32\" from \"...\" as int;\n\n\
       global protocol SH(role P, role R, role C)\n\
       {\n\
       \tplane(int, int, int, int) from P to R;\n\
       \tdo Loop(P, R, C);\n\
       }\n\n\
       aux global protocol Loop(role P, role R, role C)\n\
       {\n\
       \tchoice at P\n\
       \t{\n\
       \t\tAbove(int) from P to R;   \n\
       \t\tRes(int) from R to P;\n\
       \t\tAbove(int) from P to R;   \n\
       \t\tRes(int) from R to P;\n\
       \t\tchoice at P\n\
       \t\t{\n\
       \t\t\tBothIn() from P to R;\n\
       \t\t\tBothIn(int) from P to C;\n\
       \t\t\tdo Loop(P, R, C);\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tBothOut() from P to R;\n\
       \t\t\tBothOut() from P to C;\n\
       \t\t\tdo Loop(P, R, C);\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tIntersect() from P to R;\n\
       \t\t\tRes(int) from R to P;\n\
       \t\t\tchoice at P\n\
       \t\t\t{\n\
       \t\t\t\tOne(int) from P to C;\n\
       \t\t\t\tdo Loop(P, R, C);\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tTwo(int, int) from P to C;\n\
       \t\t\t\tdo Loop(P, R, C);\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\t//do Loop(P, R, C);  // FIXME: -spin fail if commented\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tClose() from P to R;   \n\
       \t\tClose() from P to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol SH2(role P, role R, role C)\n\
       {\n\
       \tplane(x1:int, x2:int, x3:int, x4:int) from P to R;\n\
       \tdo Loop2(P, R, C);\n\
       }\n\n\
       aux global protocol Loop2(role P, role R, role C)\n\
       {\n\
       \tchoice at P\n\
       \t{\n\
       \t\tAbove(v1:int) from P to R;   \n\
       \t\tRes(b1:int) from R to P;\n\
       \t\tAbove(v2:int) from P to R;   \n\
       \t\tRes(b2:int) from R to P;\n\
       \t\tchoice at P\n\
       \t\t{\n\
       \t\t\tBothIn() from P to R;\n\
       \t\t\tBothIn(r1:int) from P to C;\n\
       \t\t\tdo Loop2(P, R, C);\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tBothOut() from P to R;\n\
       \t\t\tBothOut() from P to C;\n\
       \t\t\tdo Loop2(P, R, C);\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tIntersect() from P to R;\n\
       \t\t\tRes(i:int) from R to P;\n\
       \t\t\tchoice at P\n\
       \t\t\t{\n\
       \t\t\t\tOne(r2:int) from P to C;\n\
       \t\t\t\tdo Loop2(P, R, C);\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tTwo(r3:int, r4:int) from P to C;\n\
       \t\t\t\tdo Loop2(P, R, C);\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tClose() from P to R;   \n\
       \t\tClose() from P to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\n\n\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X {\n\
       \t\tchoice at A {\n\
       \t\t\ta() from A to B;\n\
       \t\t\tb() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t} or {\n\
       \t\t\tc() from A to B;\n\
       \t\t\td() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() connect A to B;\n\
       \t} \n\
       \tor\n\
       \t{\n\
       \t\t2() connect A to B;\n\
       \t}\n\
       \t2() connect A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tconnect B to C;\n\
       \t\tchoice at B\n\
       \t\t{\n\
       \t\t\t1() from B to C;\n\
       \t\t\tdisconnect B and C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from B to C;\n\
       \t\t\t3() from C to B;\n\
       \t\t}\n\
       \t}\n\
       \t//3() from C to B;  // Testing unconnected errors -- error by \
       syntactic check; but should *not* by -f17 if seq comp supported\n\
       \t4() from B to A;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \t1() from A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\trec Y\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t1() from A to B;\n\
       \trec X  // Testing: projecting-pruning (for B) -- cf., syntactic \
       merge (cannot merge X and end)\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X  // Testing: projecting-pruning (for B) -- X not pruned \
       (becomes single \"unguarded\" X case) with safety errors\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to C;\n\
       \t\t\t//continue X;  // OK if uncommented\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t1() from A to B;\n\
       \trec X  // Testing: projecting-pruning (for B)\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec Y\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to C;\n\
       \t\t\t\tcontinue Y;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to C;\n\
       \t\t\t\tcontinue X;  // Testing: projecting-pruning (for B) -- role \
       progress violation for B\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t1() from A to B;\n\
       \trec Y  // Testing: projecting-pruning (for B)\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to C;\n\
       \t\t\t\tcontinue Y;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to C;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec Y\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\tcontinue X;  // Testing projection-pruning (for B) -- not pruned\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t1() from A to B;\n\
       \trec X\n\
       \t{\n\
       \t\trec Y\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\tcontinue X;  // Testing projection-pruning (for B) -- pruned\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from A to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;  // Bad: C may be stuck waiting for 1/2\n\
       \t\t}\n\
       \t}\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;  // Bad: C stuck waiting for 1\n\
       \t\t}\n\
       \t}\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \t3() from A to C;  // Testing -fair and sequencing\n\
       }\n\
       //*/\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t\t3() from A to C;   \n\
       \t\t\t\t// TESTING: from some global perspectives, this could be \
       seen as a mixed-role choice for C between B and A\n\
       \t\t\t\t// Any way to create an unsound mixed-role choice using \
       sequencing?  And taking the mixed-role choice view\n\
       \t\t\t\t// What fixes this may be that Scribble actually projects \
       this as sequencing, not choice? \n\
       \t\t\t\t// -- N.B. comes out as unfinished error, not deadlock (A is \
       in output state)\n\
       \t\t\t\t// -- In this way, bounded channels actually helpful for \
       detecting stuck messages? -- or would just come out as role-progress \
       violation, in the worse case?\n\
       \t\t\t\t// -- As opposed to \"inlining the continuation\" (or \
       projecting a tau?)\n\
       \t\t\t\t// At least, not having sequencing probably safe backup?\n\
       \t\tcontinue X;\n\
      \  }\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from B to A;\n\
       \t\tchoice at B  // Testing projection, A not syntactically in this \
       choice but cannot discard the X -- handled by UnfoldingVisitor(?)\n\
       \t\t{\n\
       \t\t\t2() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       //\t\tor  // Testing \n\
       //\t\t{\n\
       //\t\t\t3() from B to C;\n\
       //\t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t1() connect A to B;\n\
       \t1() connect B to C;\n\
       \t1() connect C to A;  // Testing accept-correlation check\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       //\trec X\n\
       //\t{\n\
       //\t\t1() from A to B;\n\
       //\t\t1() from B to C;\n\
       //\t\tchoice at A\n\
       //\t\t{\n\
       //\t\t\t2() from A to B;  // Testing projection -- just X for C, but \
       under a non-empty recursive prefix\n\
       //\t\t\tcontinue X;\n\
       //\t\t}\n\
       //\t\tor\n\
       //\t\t{\n\
       //\t\t\t3() from A to B;\n\
       //\t\t\t3() from B to C;\n\
       //\t\t}\n\
       //\t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t// Testing unfair transform\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t1a() from A to B;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t1b() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1c() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t2a() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t2b() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to B;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t4() from A to B;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A  // Testing unfair transform of recursive non-det \
       choice\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to B;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to B;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t4() from A to B;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /* // TODO: more unfinished (and accept-correlation) role tests\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;  // FIXME unfinished term not reported -- \
       FIXME: still need to check good terms (taking accept-guarded states \
       into account)\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from B to A;\n\
       \t\t1() from B to A;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /* Testing aux protocol as CommandLine root protocol\n\
       aux global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t5() from A to C;  // A \"non-det merge\" that cannot be \
       refactored by prefixing/sequencing\n\
       \t\t3() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t5() from A to C;\n\
       \t\t4() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       //--- correlation\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       //explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       //\t1() connect A to B;  // Bad\n\
       //\tdisconnect A and B;\n\
       //\t1() connect A to B;\n\
       //\t2() from A to B;\n\
       //\tdisconnect A and B;\n\n\
       //\t1() connect A to B;  // Bad\n\
       //\tdisconnect A and B;\n\
       //\t2() connect A to B;\n\
       //\tdisconnect A and B;\n\
       \t\n\
       //\t1() connect A to B;  // OK: this is what theabove should be \
       intuitively\n\
       //\tdisconnect A and B;\n\
       //\t2() connect A to C;\n\
       //\tdisconnect A and C;\n\n\
       //\t1() connect A to B;  // Morally OK -- but conservatively rule \
       out by syntactic condition -- do via modelling? endpoint \
       transformations? (B should actually be a branch 1 or 2) -- or global \
       model based on two concurent instances of each endpoint? (two \
       enough?) -- mergeability of all accepts\n\
       //\tdisconnect A and B;\n\
       //\t1() connect A to B;\n\
       //\tdisconnect A and B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t1() connect A to B;  // FIXME: correlation\n\
       \t2() connect A to C;\n\
       \t3() connect B to C;\n\
       }\n\
       //*/\n\n\n\
       /*/\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// OK\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() connect A to C;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() connect A to B;\n\
       \t\t}\n\
       \t} \n\
       \tor\n\
       \t{\n\
       \t\t2() connect A to C;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t2() connect A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() connect A to B;\n\
       \tdisconnect A and B;\n\
       \t1() connect A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() connect A to B;\n\
       \t\tdisconnect A and B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() connect A to B;\n\
       \t\tdisconnect A and B;\n\
       \t}\n\
       \t3() connect A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tdo Proto1Aux(A, B);\n\
       }\n\n\
       aux global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \tdisconnect A and B;\n\
       \t//connect A to B;  // Bad\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() connect A to B;\n\
       \tdisconnect A and B;\n\
       \t1() connect A to B;\n\
       }\n\
       //*/\n\n\
       //---correlation\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t}\n\
       \tor  // Not OK: projection treats C?A:1 as single-case choice \n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// CHECKME: orphans currently detected on local termination, not \
       full system termination (same for stuck and deadlock)\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to A;\n\
       \t} or {\n\
       \t\t1b() from A to B;\n\
       \t\t3() from B to C;  // -nolocalchoicecheck to show orphans\n\
       \t\t4() from C to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdo Proto1Aux(A, B);  // Testing recvar name disambiguation\n\
       \t}\n\
       }\n\n\n\
       global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to B;\n\
       \t\t\t\tchoice at A\n\
       \t\t\t\t{\n\
       \t\t\t\t\tcontinue X;\n\
       \t\t\t\t}\n\
       \t\t\t\tor\n\
       \t\t\t\t{\n\
       \t\t\t\t\t3() from A to B;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t//4() from A to B;\n\
       \t\t\t\tcontinue X;  // Testing unguarded shadowed recvars at \
       different nestings\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t5() from A to B;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\trec X\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\trec X  // Testing shadowed unguarded recs\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\tcontinue X;  // Testing inlined-unfolding for shadowed recs\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to B;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t2() from B to A;\n\
       \t\t\tcontinue X;\n\
       \t\t} or {\n\
       \t\t\t1() from A to B;\n\
       \t\t\t3() from B to A;  // Testing non-det unfair-transform \
       (\"unfairness\" has the power to enforce a single case out of \
       non-det options)\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t2() from B to A;\n\
       \t\t\tcontinue X;  // Testing non-det unfair-transform\n\
       \t\t} or {\n\
       \t\t\t1() from A to B;\n\
       \t\t\t3() from B to A;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;  // Testing non-det unfair-transform\n\
       \t\t} or {\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tdisconnect A and B;\n\
       \tdo Proto1Aux(A, B);\n\
       }\n\n\
       // Trivial test for \"expressiveness\" of aux -- but could just make \
       explicit..  // CHECKME example where \"aux\" is fully needed\n\
       aux global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t//1() from A to C;\n\
       \t\t\tcontinue X;  // Testing fairness\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from A to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to B;\n\
       \t\t5() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;  // Syntactic merge means: non-det \"squashed\" \
       so original choice path now ambiguous, so we need to act \
       conservatively -- conservatively means squashed role needs to accept \
       any incoming messages as a branch (to handle ambiguity) while not \
       being allowed to make any output choices (only unary send allowed, \
       so must be identical in all possibilities)\n\
       \t\t\t\t// EFSM transform has to follow this intuition, full \
       continuations after a non-det input choice have to be \
       convservatively squashed (and if not squashable, then check model \
       using unsquashed version) -- or implement as syntactic merge, and \
       use original if not mergeable\n\
       \t\t4() from C to B;\n\
       \t\t6() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t3() from B to D;\n\
       \t\t4() from C to D;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t3() from B to D;\n\
       \t\t5() from C to D;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to D;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t5() from C to D;  // Not mergeable by ICALP13\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to B;\n\
       \t\t5() from C to B;  // Tricky to check, e.g., output state \
       sub-EFSMs are the same (cf. syntactic equality) for terminating the \
       merge, maybe need strict isomorphism (without renaming)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t3() from B to C;\n\
       \t\t\t4() from B to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t3() from B to C;\n\
       \t\t\t5() from B to C;  // Consider EFSM of C for merging -- do \
       non-det input squashing inductively, and only if no recursive edges \
       back to original or preceding state? -- difficult to confirm, e.g., \
       output state sub-EFSMs are the same (cf. syntactic equality) for \
       terminating the merge, maybe need isomorphism\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t//5() from B to C;  // Makes merge easier\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t5() from B to C;  // Consider EFSM of C for merging\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t3() from B to C;\n\
       \t\t4() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;  // Merging (non-det input state \"squashing\") \
       first messages only not enough (cf. inductive syntactic merge)\n\
       \t\t3() from B to C;\n\
       \t\t5() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t//choice at B\n\
       \t\t{\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t//2() from B to A;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1a() from A to B;\n\
       \t\t\t1a() from A to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1b() from A to B;\n\
       \t\t\t1b() from A to C;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t2a() from A to B;\n\
       \t\t\t2a() from B to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2b() from A to B;\n\
       \t\t\t2b() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t3()\tfrom C to A;\n\
       \t3()\tfrom C to A;  // Role-progress violation for C under WF_1\n\
       \tchoice at A\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t5() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t6() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       // Counter example to completeness of safety for current WF\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from A to C;\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t5() from A to C;  // FIXME: should be mergable as a branch at C? \
       (yes, by ICALP13) -- but requires treating non-det as det (i.e. \
       language equiv. vs. bisim? -- local language minimisation wrt. \
       inputs only?) -- in general, consider subsequent B and C \
       interactions; but this exact example should be safe\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A t B;  // Testing Antlr error display overriding\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1(Int) from A to B;  // Testing bad payloads\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() \n\
       \t\tfrom A to B;\n\
       \t\t5() from B to C;  // \"Standard merge\" (ICALP13) -- 2/5 cases \
       merged for input choice at C  // but is it actually mergable in \
       ICALP13/WADFEST? because only defined on branch, not select as \
       needed for C here -- it's fine, only merging the top level branches \
       (distinct labels)\n\
       \t\t6() from C to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from C to A;  // Not mergeable by WADFEST, because \
       continuations of C to be merged are not input-branches (but WAFDEST \
       could probably be extended easily -- most simply by just allowing \
       TmergeT for any T, not just branch -- more generally, would have to \
       inductively coerce non-branches into branches, which is what the \
       below example (i.e. WADFEST) is a special case of)\n\
       \t\t\t\t// However, mergeable by ICALP13 (just a typo by WADFEST)\n\
       \t\t\t\t// (WADFEST merges also only defined on branches, not \
       receives, but receive can be easily converted to singleton branches? \
       -- a point of directed branches is that they have no payloads, and \
       receives have no labels, which Scribble needs to consider)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       // Counter example to completeness of safety for current WF -- this \
       notion of completeness is wrt. a global semantics, but not wrt. \
       specific definition of Scribble projection/EFSM? (i.e. if we \
       project/build a subsequent input choice at C from A, not just the \
       initial one from B -- this is essentially mixing a ``non-choice'' \
       (pre-determined flow) at A with an external choice at C -- i.e. \
       ICALP13/WADFEST merge: convert non-det external choice into det \
       non-choice followed by inductively merged (external choice) \
       continuations) -- actually not necessarily, can consider still \
       non-det, but just that the continuation branches are safe (but this \
       view is more like inferring non-directly specified cases for nested \
       branches)\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t//choice at A {  // better for merging?\n\
       \t\t\t3() from A to C;\n\
       \t\t//}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1b() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t4() from A to C;  // FIXME: \"merge/coerce\" branches for C -- \
       implement as some king of variant of \"determinisation\"? i.e. \
       2.3+2.4 -> 2.(3+4) (issue is it's not bisim preserving -- is local \
       language minimisation a sound general principle? well, not for the \
       minimal non-det branch example (or actually, yes? see below), but \
       somehow only for \"nested\" external choices? i.e. same-label \
       mergability?)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;  // FIXME: should we just language-equiv \
       minimise endpoints? implicitly take that as the CFSM meaning of this \
       global protocol? depends on global semantics -- but would make \
       general projection/graphbuilding and mergability more uniform? -- \
       uniformity should be the aim, see below -- no: not just independent \
       EFSM minimisation, but should consider the whole CFSM system, see \
       below\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t5() from C to A;  // FIXME: language minimising at C will \
       determinise, but minimising at A has no effect: minimising makes \
       (non-det) external choice at C into internal choice, which is \
       incompatible with original internal choice at A -- so independent \
       language minimising is not sound -- basically syntactic branch-only \
       merging restricts \"determinising\" to external choices only -- so \
       do independent EFSM language-minimisation applied to inputs only? \
       (but sometimes non-det outputs can be safely minimised -- but just \
       leave them as is for global model checking?)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;  // Simply syntactically not representable with \
       directed choice, but safe under the same intuition as mergability -- \
       in this case, independent endpoint minimisation modifies both A and \
       B consistently\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t//choice at A\n\
       \t\t//{\n\
       \t\t//\t4() from A to C;\n\
       \t\t//}\n\
       \t\t//or\n\
       \t\t//{\n\
       \t\t//\t3() from A to C;\n\
       \t\t//}\n\
       \tdo Proto1Aux(A, C); // WADFEST merge -- morally: WADFEST merge \
       \"infers\" safe branche cases for nested branches -- can factor out \
       by subprotos\n\
       \t\t\t\t\t// NO: WADFEST prevents internal choice 4/5 by A, only \
       allows external choice by C\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1b() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t//choice at A\n\
       \t\t//{\n\
       \t\t//\t4() from A to C;\n\
       \t\t//}\n\
       \t\t//or\n\
       \t\t//{\n\
       \t\t//\t3() from A to C;\n\
       \t\t//}\n\
       \t\tdo Proto1Aux(A, C);\n\
       \t}\n\
       }\n\n\
       // Makes clear that TmergeT for any T should be OK\n\
       aux global protocol Proto1Aux(role A, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t3() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t5() from C to A;  // TODO: most general merge: coerce an \
       external choice for A here -- less morally clear than, e.g., WADFEST \
       restriction to branch-only merge though\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A {\n\
       \t\tbuyer1(Int) from A to B;  // Total\n\
       \t\t(Int) from B to A;        // B will pay this much\n\
       \t\tbuyer2(Int) from A to C;  // C will pay remainder\n\
       \t} or {\n\
       \t\tbuyer1(Int) from A to C;  // Total\n\
       \t\t(Int) from C to A;        // C will pay this much\n\
       \t\tbuyer2(Int) from A to B;  // B will pay remainder\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \t..(syntactic) reachability tests\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t3() from A to C;  // TODO: \"optional\": needs empty actions and \
       \"empty-removal\" transformation\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to A;\n\
       \t\t\t3() from A to C;  // Trying to find an unfair-transformation \
       problem wrt. not visiting \"2\" case after \"1\" transitions\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to A;\n\
       \t\t\t3() from A to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \t2() from B to C;  // Testing CommandLine with, e.g., -fsm(dot) arg \
       (disamb error before projection passes means graph cannot be built)\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A  // -oldwf, default, -fair\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t//2() from A to C;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t3() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t() from B to C;\n\
       \t\t1() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t() from B to C;  // Classic mergeability (no \"equiv.\" protocol \
       by sequencing -- unless maybe a generous async. equiv.)\n\
       \t\t2() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t//3() from C to A;\n\
       \t\t3() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t5() from B to C;\n\
       \t\t//6() from C to A;\n\
       \t\t5() from C to B;  // Mergeable -- A not involved downstream\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       \t3() from A to B;  // Sequencing after recursive-choice\n\
       \t3() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1(Proto2@A) from A to B;  // Testing delegation payload projection\n\
       }\n\n\
       global protocol Proto2(role A, role B)\n\
       {\n\
       \t2() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(Proto1@A) from A to B;  // Testing recursive protocoldecls\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(Proto2@A) from A to B;  // Testing recursive protocoldecls\n\
       }\n\n\
       global protocol Proto2(role A, role B)\n\
       {\n\
       \t//(Proto1@A) from A to B;\n\
       \t(Proto3@A) from A to B;\n\
       \t//do Proto3(A, B);\n\
       }\n\n\
       global protocol Proto3(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \t//(Proto1@A) from A to B;\n\
       \t//(Proto2@A) from A to B;\n\
       \t//do Proto1(A, B);\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t4() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t5() from B to C;  // Mergeable subset of choice cases (cf. \
       syntactic merge?\n\
       \t\t5() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from A to B;\n\
       \t\t5() from B to C;\n\
       \t\t5() from C to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdo Game(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       }\n\n\
       aux global protocol Game(role A, role B, role C)\n\
       {\n\
       \t() from A to B;\n\
       \t() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t//do Proto1Aux(A, B);\n\
       \t\t}\n\
       \t}\n\
       }\n\n\
       aux global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \tcontinue X;  // Still checked for aux\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1<sig M>(role A, role B)\n\
       {\n\
       \tdo Proto1Aux<M>(A, B);\n\
       }\n\n\
       aux global protocol Proto1Aux<sig M>(role A, role B)\n\
       {\n\
       \tM from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(Test.Foo) from A to B;  // Testing DataType disamb\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tInt from A to B;  // Testing disamb (and AST visitChild)\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       sig <java> \"...\" from \"...\" as M;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(M) from A to B;  // Testing disamb (and AST visitChild)\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;  // Testing \"fair\"/\"unfair\" liveness -- \
       issue of global liveness vs. local subtyping) -- generating \"unfair \
       output subtyped\" global model (can it be done simply by terminal \
       set role check?)\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;  // Testing non-det unfairClone -- \
       uninteresting test because non-det single-action-edge choice-merges \
       get implicitly minimised (by graph building)\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t3() from B to A;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;  // Testing non-det unfairClone\n\
       \t\t\t3() from B to A;\n\
       \t\t\t//4() from B to A;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect B to C;\n\
       \tconnect A to C;  // How does C \"correlate\" A/B connections to \
       sessions -- introduces? -- need to study real examples\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect B to C;  // C doesn't care whether each connection is \
       new or old session?\n\
       \t\t2() from B to C;\n\
       \t\tdisconnect B and C;\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tdisconnect A and B;\n\
       \tconnect A to B;  // What does it mean for B to leave the session \
       and rejoin?  Need some condition like if fully leave, then rejoining \
       should be same as new session? (i.e. this case bad) -- Consider \
       implementability\n\
       \tdisconnect A and B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() connect A to C;\n\
       \t\t\tdisconnect A and C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() connect A to C;\n\
       \t\t\tdisconnect A and C;\n\
       \t\t\tcontinue X\n\
       \t\t}\n\
       \t}\n\
       \tconnect A to C;  // Good until here, now bad because C already \
       fully left?\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tconnect A to B;  // What does this mean in terms of a \
       \"session\"? (should consider the implementation, session id, ...) \
       -- distinction between initial accept and in-session accepts?\n\
       \t\t//1() from A to B;\n\
       \t\t//1() from B to A;\n\
       \t\tdisconnect A and B;  // Shouldn't allow continuation after a \
       certain point?  no session structure left?\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect B to C;\n\
       \t\tconnect A to C;\n\
       \t\t1() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tconnect A to C;  // Inconsistent choice connect subjects\n\
       \t}\n\
       }\n\
       //*/\n\n\
       //***\n\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdisconnect A and B;  // CHECKME: disconnect shouldn't have \
       asymmetric src/dest -- check enabling conditions wrt. choices, \
       projection, etc\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1(Int) connect A to B;  // Testing non-det payloads for \
       message-connects\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() connect A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() connect B to C;\n\
       \t\t\t2() from B to C;\n\
       \t\t\tdisconnect B and C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to B;\n\
       \t\t\t3() connect B to C;  // Cf. PartnershipSupplier filter subproto\n\
       \t\t\t3() from B to C;\n\
       \t\t\tdisconnect B and C;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\trec Y\n\
       \t\t\t{\n\
       \t\t\t\tchoice at B\n\
       \t\t\t\t{\n\
       \t\t\t\t\t3() from B to A;\n\
       \t\t\t\t\tcontinue X;\n\
       \t\t\t\t}\n\
       \t\t\t\tor\n\
       \t\t\t\t{\n\
       \t\t\t\t\t4() from B to A;\n\
       \t\t\t\t\tcontinue Y;  // Needs fairness\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tchoice at B  // C not involved, but still live (without \
       fairness)\n\
       \t\t\t{\n\
       \t\t\t\t3() from B to A;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t4() from B to A;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tchoice at B\n\
       \t\t\t{\n\
       \t\t\t\t3() from B to A;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\trec Y  // Bad (fair or not)\n\
       \t\t\t\t{\n\
       \t\t\t\t\t4() from B to A;\n\
       \t\t\t\t\tcontinue Y;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;  // Live, without fairness\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;  // CHECKME: bad sequence if commented, \
       correct? -- bad sequence because C not in block so projection \
       pruned, then only rec-block left -- is this satisfactory? (consider \
       standalone global semantics vs. global as syntactic sugar for \
       locals) -- however, \"bad sequence\" restriction probably does not \
       hurt expressiveness\n\
       \t}\n\
       \t3() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \trec X  // Testing non-fair EFSM generation\n\
       \t{\n\
       \t\tchoice at B\n\
       \t\t{\n\
       \t\t\t3() from B to A;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from B to A;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\twrap B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\twrap A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tdisconnect A and B;  // Testing unfairClone terminal state \
       reconcilliation -- FIXME: this example doesn't actually test this, \
       cf. SupplierInfoExplicit for requestor\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \t//3() from A to C;  // Testing fair/unfair liveness for C\n\
       \t3() from C to A;  // Message liveness also subject to fair/unfair\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t() from B to C;\n\
       \t\t3() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t() from B to C;  // choice + sequencing not the same as just \
       syntactic sugar for factoring out a common branch continuation, i.e. \
       cannot factor out \"() from B\" as a continuation without either \
       losing causality for output to A or changing output order at B -- \
       arguable that changing order at B is equivalent (for some \
       equivalence), but not equivalent under basic bisimilarity\n\
       \t\t4() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;  // CHECKME: empty case for C discarded by \
       projection, but do we need tau for correctness?  this example works \
       because it ends up as stuck error -- a point is generating tau and \
       then bisim-minimising doesn't remove the tau, whereas the current \
       projection does remove the tau (so current projection is not equiv \
       to bisim-minimisation intuition)\n\
       \t\t\t\t// should be OK: intiution: an input-state endpoint cannot \
       choose to not receive a message, i.e. input states should never have \
       tau -- so whole system must satisfy properties when modelling \
       \"partial\" local branches as a \"complete\" branch for just the \
       involved choice cases (i.e. ignore any non-involved cases)\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from A to C;\n\
       \t}\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \t3() from A to C;  // Not \"strongly\" live for C -- by subtyping, \
       an implementation of A may never terminate -- strongly live probably \
       means all roles have to be involved in every choice path -- not \
       quite: it all depends on definition of subtyping, could make a \
       \"live\" notion of subtyping that doesn't allow a non-live subset of \
       choices -- this also depends on the select primitives and typing \
       rules (it could come down to decidability of if-conditions...) -- \
       problem is, even without subtyping, select primitive is always about \
       selecting just one case, can't really make a \"live\" typing on top \
       of that... -- could be positioned as basic session typing needs \
       strong liveness, while weak liveness can be aimed at assuming a more \
       general program verification -- or maybe a more powerful \
       \"imperative style\" typing system could work, e.g. while (...) { \
       ..non-live choice on s..} ..live choice on s.., i.e. it is ok to \
       select a non-live case if the while will terminate to eventually \
       lead us to a live case.. -- this fairness/liveness issue is another \
       \"bondary\" issue between modelling/types/practice, bit like \
       linearity\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       //explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect A to C;  // Testing API gen (without I/O i/f gen)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A  // Testing minfsm\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t2() from A to B;\n\
       \trec X\n\
       \t{\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;  // Testing minfsm\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tcontinue X;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t2() from A to B;\n\
       \trec X\n\
       \t{\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;  // Testing minfsm\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tcontinue X;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;  // Bad\n\
       \tdisconnect A and C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;  // Good: mergeable\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect C to B;  // If A and C are in above, 2() can be stuck at \
       B's connect/accept here -- but stuck error not directly detected, \
       error manifests as B/C deadlock\n\
       \t\t2() from A to B;  // Mergeable if B/C connection not deadlocked\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \t2() from B to C;  // Trying to make an unconnected orphan from B \
       to C, but currently will always get a connectedness error first -- \
       model building semantics shouldn't/won't allow explicit unnconnected \
       orphans, message cannot be sent if not connected\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to C;\n\
       \tconnect C to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to C;\n\
       \t\tconnect A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t\t2() from C to B;  // Bad: connect and msg from different choice \
       subjects\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tconnect A to B;  // Good: tests recursion pruning for \
       connection actions\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A  // ** using old WF, this breaks connectedness \
       checking -- WFChoiceChecker is an UnfoldingVisitor, but it is prunes \
       the visit on entering the unfolded choice\n\
       \t\t{\n\
       \t\t\tconnect A to B;\n\
       \t\t\tcontinue X;  // Bad\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tconnect B to C;\n\
       \t\t\tdisconnect B and C;  // Comment is bad\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\t2() from C to A;\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Disable local choice subject inference\n\
       \trec X\n\
       \t{\n\
      \  \t// Testing getTrace?\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t//1() from A to B;\n\
       \t\t\t1() from A to C;\n\
       \t\t\t3() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t\t3() from C to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Disable local choice subject inference\n\
       \trec X\n\
       \t{\n\
       \t\t// Testing getTrace?\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from A to C;\n\
       \t\t\t2() from C to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to C;\n\
       \t\t\t3() from C to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;  // Testing graph building\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Partners(\n\
       \t\trole LOGINsvc,\n\
       \t\trole REQUESTOR,\n\
       \t\trole AUTHsvc,\n\
       \t\trole FILTERsvc,\n\
       \t\trole SUPPLIERsvc,\n\
       \t\trole CONTRACTsvc)\n\
       {\n\
       \tlogin() from REQUESTOR to LOGINsvc;\n\
       \tchoice at LOGINsvc\n\
       \t{\n\
       \t\tloginfailure() from LOGINsvc to REQUESTOR;\n\
       \t\t0() from REQUESTOR to AUTHsvc;\n\
       \t\t0() from AUTHsvc to FILTERsvc;\n\
       \t\t0() from AUTHsvc to SUPPLIERsvc;\n\
       \t\t0() from AUTHsvc to CONTRACTsvc;\n\
       \t} or {\n\
       \t\tloginsuccess() from LOGINsvc to REQUESTOR;\n\
       \t\trec MAIN\n\
       \t\t{\n\
       \t\t\tchoice at REQUESTOR\n\
       \t\t\t{\n\
       \t\t\t\tgetsuppliers() from REQUESTOR to AUTHsvc;\n\
       \t\t\t\tchoice at AUTHsvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;  // Bad: \
       testing getTrace performance\n\
       \t\t\t\t\tdeny() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;\n\
       \t\t\t\t\tsuppliers() from SUPPLIERsvc to AUTHsvc;\n\
       \t\t\t\t\tfilterSuppliers() from AUTHsvc to FILTERsvc;\n\
       \t\t\t\t\tfiltered() from FILTERsvc to AUTHsvc;\n\
       \t\t\t\t\tsuppliers() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t}\n\
       \t\t\t} or {\n\
       \t\t\t\tgetcontracts() from REQUESTOR to AUTHsvc;\n\
       \t\t\t\tchoice at AUTHsvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\tdeny() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\tgetcontracts() from AUTHsvc to CONTRACTsvc;\n\
       \t\t\t\t\tcontracts() from CONTRACTsvc to AUTHsvc;\n\
       \t\t\t\t\tfilterContracts() from AUTHsvc to FILTERsvc;\n\
       \t\t\t\t\tfiltered() from FILTERsvc to AUTHsvc;\n\
       \t\t\t\t\tcontracts() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t\tcontinue MAIN;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1\n\
       (\n\
       \t\trole REQuestor,\n\
       \t\trole AUTHsvc,\n\
       \t\trole SUPPLIERsvc,\n\
       \t\trole CONTRACTsvc)\n\
       {\n\
       \trec MAIN\n\
       \t{\n\
       \t\tchoice at REQuestor\n\
       \t\t{\n\
       \t\t\tgetsuppliers() from REQuestor to AUTHsvc;\n\
       \t\t\tchoice at AUTHsvc\n\
       \t\t\t{\n\
       \t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;  // Bad: testing \
       getTrace\n\
       \t\t\t\tdeny() from AUTHsvc to REQuestor;\n\
       \t\t\t} or {\n\
       \t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;\n\
       \t\t\t\tsuppliers() from SUPPLIERsvc to AUTHsvc;\n\
       \t\t\t\tsuppliers() from AUTHsvc to REQuestor;\n\
       \t\t\t}\n\
       \t\t} or {\n\
       \t\t\tgetcontracts() from REQuestor to AUTHsvc;\n\
       \t\t\tchoice at AUTHsvc\n\
       \t\t\t{\n\
       \t\t\t\tdeny() from AUTHsvc to REQuestor;\n\
       \t\t\t} or {\n\
       \t\t\t\tgetcontracts() from AUTHsvc to CONTRACTsvc;\n\
       \t\t\t\tcontracts() from CONTRACTsvc to AUTHsvc;\n\
       \t\t\t\tcontracts() from AUTHsvc to REQuestor;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tcontinue MAIN;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\
       /*\n\
       global protocol Proto(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdisconnect A and B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;  // B can still send even if A disconnects \
       (async) -- so not wait-for from that situation\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*/\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\n\
       \t\t1() from A to C;\n\
       \t\t1() from A to C;\n\n\
       \t\t3() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\n\
       \t\t() from B to C;  // Trying to get A into above block while C is \
       in here, such that WF1 forces C also into the above -- so that WF1 \
       becomes unsound\n\
       \t\t2() from A to C;\n\n\
       \t\t4() from C to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\t1() from C to B;\n\
       \t\t1() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from C to B;\n\
       \t\t2() from C to A;\n\n\
       \t\t2() from A to B;\n\
       \t\t2() from A to B;\n\n\
       \t\t2() from A to C;\n\n\
       \t\t2() from C to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t1() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\n\
       \t2() from C to B;  // TODO: investigate: WF_1 won't get past here, \
       is it OK?\n\
       \t2() from C to B;  // ..becomes \"fake\" role liveness problem \
       because of WF1 -- not live even assuming fairness\n\
       \t// ..dragons\n\n\
       \t// TODO: investigate reachability of local states in global model\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\t0() from B to A;\n\
       \t\t2() from C to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\trec X\n\
       \t\t\t{\n\
       \t\t\t\tchoice at A\n\
       \t\t\t\t{\n\
       \t\t\t\t\t1() from A to B;\n\
       \t\t\t\t\tcontinue X;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\t1() from B to A;\n\
       \t\t2() from C to B;  // Good\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\t0() from B to A;\n\n\
       \t\t//2() from C to B;\n\
       \t\t//2() from C to B;\n\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\trec X\n\
       \t\t\t{\n\
       \t\t\t\tchoice at A\n\
       \t\t\t\t{\n\
       \t\t\t\t\t1() from A to B;\n\
       \t\t\t\t\t//2() from C to B;\n\
       \t\t\t\t\tcontinue X;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\t1() from B to A;\n\
       \t}\n\n\
       \t2() from C to B;  // TODO: investigate: WF_1 won't get past here, \
       is it OK?\n\
       \t2() from C to B;  // ..becomes \"fake\" role liveness problem \
       because of WF1\n\
       \t// ...\n\n\
       \t// TODO: investigate reachability of local states in global model\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Trying to construct a counterexample for WF1 soundness..\n\
       \t// ..need to find an error state that is unreachable by WF1 but \
       reachable by e.g. WF2\n\
       \t// Try to find a choice where B is falsely committed to a branch \
       due to WF1 (so state space of model is unsoundly restricted)\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;  // A cannot do this in WF1 unless B receives a \
       1() first\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from A to C;\n\
       \t\t//2() from A to B;\n\
       \t\t\t\t// Local choice subjects require B to also receive from A in \
       this block\n\
       \t\t\t\t// Two cases: same or different label\n\
       \t\t\t\t// If different label, then no possibility of false branch \
       commitment\n\
       \t\t\t\t// If same label, then non-det always allows B to enter this \
       block, even under WF1\n\
       \t\t\t\t// Therefore: false choice commitment not possible\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B  // Good non-det EFSM for A (including \
       minimisation)\n\
       \t\tconnect A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t//connect A to C;  // Tests -minfsm\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Needs local choice subject disabled, though the point is it's \
       still bad\n\
       \tchoice at A\n\
       \t{\n\
       \t\ta() from A to B;\n\
       \t\t//d() from C to B;  // Moved down to make C enabled\n\
       \t\tcprime() from A to C;\n\
       \t\td() from C to B;\n\
       \t\tb() from B to A;\n\
       \t\ty() from A to B;  // Orphan\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tc() from A to C;\n\
       \t\td() from C to B;  // ..point is B could get in this case, while \
       A (and C) are in the other\n\
       \t\tb() from B to A;\n\
       \t\ta() from A to B;  // (..not stuck msg error because the a() from \
       above is consumed here)\n\
       \t\tx() from B to A;  // Orphan\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Counterexample for WF1 if local choice subject disabled\n\
       \tchoice at A\n\
       \t{\n\
       \t\ta() from A to B;\n\
       \t\ta() from A to B;  // WF1 unnaturally resolves the non-det choice \
       at B by forcing B to commit to this branch before C is enabled\n\
       \t\tcprime() from A to C;\n\
       \t\td() from C to B;\n\
       \t\tb() from B to A;\n\n\
       \t\t//y() from A to B;  // Potential orphan\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tc() from A to C;\n\
       \t\td() from C to B;\n\
       \t\tb() from B to A;  // Must come before a's, to prevent reverse \
       choice race\n\
       \t\ta() from A to B;\n\
       \t\ta() from A to B;\n\n\
       \t\t//x() from B to A;  // Potential orphan\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Disable local choice subject inference -- mergeable\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t2() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t\t2() from C to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t2() from C to B;\n\n\
       \t\t3() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t\t2() from C to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\n\
       \t\t//3() from B to C;\n\
       \t\t4() from B to C;  // Bad\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t2() from C to B;\n\
       \t\t2() from C to B;\n\n\
       \t\t3() from B to C;  // Should be potential stuck\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t\t2() from C to B;\n\
       \t\t2() from C to B;\n\
       \t\t() from C to A;  // Another counterexample to WF1 (B falsely \
       committed to here when A/C are)\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\n\
       \t\t4() from B to C;  // Should be potential stuck\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;  // TODO: link model check errors to source \
       code? -- though this protocol is syntactically bad...\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Testing one-slot asynchrony\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from A to C;\n\
       \t\t\t1() from C to B;  // Bad... but the point is one-slot \
       asynchrony prevents exploring the global state where A!B:1 done \
       twice without B?A:1 at least once, i.e. A cannot loop round first \
       block twice without B also following into the first block... can \
       something like this make WF unsound? -- is recursive mixed-role \
       poly-inputs the only context for this problem?\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\t2() from C to B;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tcontinue X;  // Checking safety in non-terminating global model\n\
       \t}\n\
       \tchoice at C\n\
       \t{\n\
       \t\t2() from C to D;\n\
       \t\t3() from D to C;  // Stuck messages also manifest as message \
       liveness violations (CHECKME: subsumed? so unnecessary to check \
       safety except for actual terminations? -- but morally, they are \
       local safety errors for the roles, as opposed to global liveness \
       errors for the system)\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from C to D;\n\
       \t\t4() from D to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t1() from A to C;\n\
       \t\t\t\tcontinue X;  // ** CHECKME: badly formed (for B) -- empty \
       block bvelow is pruned leaving only the continue case, which leads \
       to deadlock in global model, is this OK as WF algorithm? (any way to \
       be unsound?)  or should explicitly detect inconsistent choice block \
       projections? \n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to C;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /* // TODO: test \"transitive\" intermediate continue-edge fixing?\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec Y\n\
       \t{\n\
       \t\t0() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tcontinue Y;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*  // Cf. -oldwf (non disjoint enabling)\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\trec Y\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t1() from A to B;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to B;\n\
       \t\t\t}\n\
       \t\t\tchoice at A  // EFSM state blowup -- FIXME: syntactic \
       unfolding still necessary with global model checking? (for \
       enabling?) or minimise before model checking?\n\
       \t\t\t{\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t//2() from A to B;\n\
       \t\t\t\t//continue X;\n\
       \t\t\t\tcontinue Y;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec Y\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\tcontinue X;  // FIXME: non-fair is just slow, or \
       non-terminating?\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tcontinue Y;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to B;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /* // ??\n\
       import Test2;\n\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\
       //*/\n\n\n\
       /* // Simple name coincides with full name\n\
      \   // Some corner cases related to simple/full name overlap for \
       default pa\n\
       module Test;\n\n\
       import Test;\n\
       //*/\n\n\
       /*\n\
       module Test;\n\n\
       import Test as Test;\n\
       //*/\n\n\
       /*\n\
       module Test;\n\n\
       import Test2 as Test;  // Good or bad?\n\n\
       global protocol Proto(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \tdo Test.Proto(A, B);\n\
       }\n\
       //*/\n\n\n\
       /*  // Some corner cases related to simple/full name overlap for \
       default package Modules not tested in test suite (all Modules \
       packaged)\n\
       import Test2;\n\
       import Test3 as Test3;         // The name Test3 is test3.Test3 in \
       Test2\n\
       import test3.Test3 as Test4;   // The name Test4 is actually Test4 \
       in Test2\n\
       //import test3.Test3 as Test3;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \tdo Test2.Proto2(A, B);\n\
       \tdo Test3.Foo3(A, B);\n\
       \tdo Test4.Bar3(A, B);\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Foo(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \tdo Bar(A, B);\n\
       \t2() from A to B;\n\
       }\n\n\
       aux global protocol Bar(role A, role B)  // Testing bad unused role \
       decls (wrt. subprotocol collected role occurrences) -- allow as aux?\n\
       {\n\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Foo(role A, role B)  // Project for A\n\
       {\n\
       \tdo Bar1<1(), 2()>(A, B);\n\
       \tdo Bar1<3(), 4()>(A, B);\n\
       }\n\n\
       global protocol Bar1<sig M1, sig M2>(role A, role B)\n\
       {\n\
       \tdo Bar2<M1, M2>(A, B, A);  // TODO: duplicate role args\n\
       }\n\n\
       global protocol Bar2<sig M1, sig M2>(role A, role B, role C)\n\
       {\n\
       \tM1 from A to B;\n\
       \tM2 from B to C;\n\
       }\n\
       //*/\n\n\n" )
  ; ( "from-scribble-java/tmp/ExpConn.scr"
    , "module ExpConn;\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// CHECKME: orphans currently detected on local termination, not \
       full system termination (same for stuck and deadlock)\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to A;\n\
       \t} or {\n\
       \t\t1b() from A to B;\n\
       \t\t3() from B to C;  // -nolocalchoicecheck to show orphans\n\
       \t\t4() from C to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdo Proto1Aux(A, B);  // Testing recvar name disambiguation\n\
       \t}\n\
       }\n\n\
       global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to B;\n\
       \t\t\t\tchoice at A\n\
       \t\t\t\t{\n\
       \t\t\t\t\tcontinue X;\n\
       \t\t\t\t}\n\
       \t\t\t\tor\n\
       \t\t\t\t{\n\
       \t\t\t\t\t3() from A to B;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t//4() from A to B;\n\
       \t\t\t\tcontinue X;  // Testing unguarded shadowed recvars at \
       different nestings\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t5() from A to B;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\trec X\n\
       \t\t\t{\n\
       \t\t\t\t2() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\trec X  // Testing shadowed unguarded recs\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\tcontinue X;  // Testing inlined-unfolding for shadowed recs\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t3() from A to B;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t2() from B to A;\n\
       \t\t\tcontinue X;\n\
       \t\t} or {\n\
       \t\t\t1() from A to B;\n\
       \t\t\t3() from B to A;  // Testing non-det unfair-transform \
       (\"unfairness\" has the power to enforce a single case out of \
       non-det options)\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t2() from B to A;\n\
       \t\t\tcontinue X;  // Testing non-det unfair-transform\n\
       \t\t} or {\n\
       \t\t\t1() from A to B;\n\
       \t\t\t3() from B to A;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;  // Testing non-det unfair-transform\n\
       \t\t} or {\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tdisconnect A and B;\n\
       \tdo Proto1Aux(A, B);\n\
       }\n\n\
       // Trivial test for \"expressiveness\" of aux -- but could just make \
       explicit..  // CHECKME example where \"aux\" is fully needed\n\
       aux global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t//1() from A to C;\n\
       \t\t\tcontinue X;  // Testing fairness\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from A to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to B;\n\
       \t\t5() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;  // Syntactic merge means: non-det \"squashed\" \
       so original choice path now ambiguous, so we need to act \
       conservatively -- conservatively means squashed role needs to accept \
       any incoming messages as a branch (to handle ambiguity) while not \
       being allowed to make any output choices (only unary send allowed, \
       so must be identical in all possibilities)\n\
       \t\t\t\t// EFSM transform has to follow this intuition, full \
       continuations after a non-det input choice have to be \
       convservatively squashed (and if not squashable, then check model \
       using unsquashed version) -- or implement as syntactic merge, and \
       use original if not mergeable\n\
       \t\t4() from C to B;\n\
       \t\t6() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t3() from B to D;\n\
       \t\t4() from C to D;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t3() from B to D;\n\
       \t\t5() from C to D;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to D;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t5() from C to D;  // Not mergeable by ICALP13\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to B;\n\
       \t\t5() from C to B;  // Tricky to check, e.g., output state \
       sub-EFSMs are the same (cf. syntactic equality) for terminating the \
       merge, maybe need strict isomorphism (without renaming)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t3() from B to C;\n\
       \t\t\t4() from B to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t3() from B to C;\n\
       \t\t\t5() from B to C;  // Consider EFSM of C for merging -- do \
       non-det input squashing inductively, and only if no recursive edges \
       back to original or preceding state? -- difficult to confirm, e.g., \
       output state sub-EFSMs are the same (cf. syntactic equality) for \
       terminating the merge, maybe need isomorphism\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t//5() from B to C;  // Makes merge easier\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t5() from B to C;  // Consider EFSM of C for merging\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t3() from B to C;\n\
       \t\t4() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;  // Merging (non-det input state \"squashing\") \
       first messages only not enough (cf. inductive syntactic merge)\n\
       \t\t3() from B to C;\n\
       \t\t5() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t//choice at B\n\
       \t\t{\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t//2() from B to A;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1a() from A to B;\n\
       \t\t\t1a() from A to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1b() from A to B;\n\
       \t\t\t1b() from A to C;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t2a() from A to B;\n\
       \t\t\t2a() from B to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2b() from A to B;\n\
       \t\t\t2b() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t3()\tfrom C to A;\n\
       \t3()\tfrom C to A;  // Role-progress violation for C under WF_1\n\
       \tchoice at A\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t5() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t6() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       // Counter example to completeness of safety for current WF\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from A to C;\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t5() from A to C;  // FIXME: should be mergable as a branch at C? \
       (yes, by ICALP13) -- but requires treating non-det as det (i.e. \
       language equiv. vs. bisim? -- local language minimisation wrt. \
       inputs only?) -- in general, consider subsequent B and C \
       interactions; but this exact example should be safe\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A t B;  // Testing Antlr error display overriding\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1(Int) from A to B;  // Testing bad payloads\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() \n\
       \t\tfrom A to B;\n\
       \t\t5() from B to C;  // \"Standard merge\" (ICALP13) -- 2/5 cases \
       merged for input choice at C  // but is it actually mergable in \
       ICALP13/WADFEST? because only defined on branch, not select as \
       needed for C here -- it's fine, only merging the top level branches \
       (distinct labels)\n\
       \t\t6() from C to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from C to A;  // Not mergeable by WADFEST, because \
       continuations of C to be merged are not input-branches (but WAFDEST \
       could probably be extended easily -- most simply by just allowing \
       TmergeT for any T, not just branch -- more generally, would have to \
       inductively coerce non-branches into branches, which is what the \
       below example (i.e. WADFEST) is a special case of)\n\
       \t\t\t\t// However, mergeable by ICALP13 (just a typo by WADFEST)\n\
       \t\t\t\t// (WADFEST merges also only defined on branches, not \
       receives, but receive can be easily converted to singleton branches? \
       -- a point of directed branches is that they have no payloads, and \
       receives have no labels, which Scribble needs to consider)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       // Counter example to completeness of safety for current WF -- this \
       notion of completeness is wrt. a global semantics, but not wrt. \
       specific definition of Scribble projection/EFSM? (i.e. if we \
       project/build a subsequent input choice at C from A, not just the \
       initial one from B -- this is essentially mixing a ``non-choice'' \
       (pre-determined flow) at A with an external choice at C -- i.e. \
       ICALP13/WADFEST merge: convert non-det external choice into det \
       non-choice followed by inductively merged (external choice) \
       continuations) -- actually not necessarily, can consider still \
       non-det, but just that the continuation branches are safe (but this \
       view is more like inferring non-directly specified cases for nested \
       branches)\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t//choice at A {  // better for merging?\n\
       \t\t\t3() from A to C;\n\
       \t\t//}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1b() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t4() from A to C;  // FIXME: \"merge/coerce\" branches for C -- \
       implement as some king of variant of \"determinisation\"? i.e. \
       2.3+2.4 -> 2.(3+4) (issue is it's not bisim preserving -- is local \
       language minimisation a sound general principle? well, not for the \
       minimal non-det branch example (or actually, yes? see below), but \
       somehow only for \"nested\" external choices? i.e. same-label \
       mergability?)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;  // FIXME: should we just language-equiv \
       minimise endpoints? implicitly take that as the CFSM meaning of this \
       global protocol? depends on global semantics -- but would make \
       general projection/graphbuilding and mergability more uniform? -- \
       uniformity should be the aim, see below -- no: not just independent \
       EFSM minimisation, but should consider the whole CFSM system, see \
       below\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t5() from C to A;  // FIXME: language minimising at C will \
       determinise, but minimising at A has no effect: minimising makes \
       (non-det) external choice at C into internal choice, which is \
       incompatible with original internal choice at A -- so independent \
       language minimising is not sound -- basically syntactic branch-only \
       merging restricts \"determinising\" to external choices only -- so \
       do independent EFSM language-minimisation applied to inputs only? \
       (but sometimes non-det outputs can be safely minimised -- but just \
       leave them as is for global model checking?)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;  // Simply syntactically not representable with \
       directed choice, but safe under the same intuition as mergability -- \
       in this case, independent endpoint minimisation modifies both A and \
       B consistently\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t//choice at A\n\
       \t\t//{\n\
       \t\t//\t4() from A to C;\n\
       \t\t//}\n\
       \t\t//or\n\
       \t\t//{\n\
       \t\t//\t3() from A to C;\n\
       \t\t//}\n\
       \tdo Proto1Aux(A, C); // WADFEST merge -- morally: WADFEST merge \
       \"infers\" safe branche cases for nested branches -- can factor out \
       by subprotos\n\
       \t\t\t\t\t// NO: WADFEST prevents internal choice 4/5 by A, only \
       allows external choice by C\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1b() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t//choice at A\n\
       \t\t//{\n\
       \t\t//\t4() from A to C;\n\
       \t\t//}\n\
       \t\t//or\n\
       \t\t//{\n\
       \t\t//\t3() from A to C;\n\
       \t\t//}\n\
       \t\tdo Proto1Aux(A, C);\n\
       \t}\n\
       }\n\n\
       // Makes clear that TmergeT for any T should be OK\n\
       aux global protocol Proto1Aux(role A, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t3() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t3() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t5() from C to A;  // TODO: most general merge: coerce an \
       external choice for A here -- less morally clear than, e.g., WADFEST \
       restriction to branch-only merge though\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A {\n\
       \t\tbuyer1(Int) from A to B;  // Total\n\
       \t\t(Int) from B to A;        // B will pay this much\n\
       \t\tbuyer2(Int) from A to C;  // C will pay remainder\n\
       \t} or {\n\
       \t\tbuyer1(Int) from A to C;  // Total\n\
       \t\t(Int) from C to A;        // C will pay this much\n\
       \t\tbuyer2(Int) from A to B;  // B will pay remainder\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \t..(syntactic) reachability tests\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t3() from A to C;  // TODO: \"optional\": needs empty actions and \
       \"empty-removal\" transformation\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to A;\n\
       \t\t\t3() from A to C;  // Trying to find an unfair-transformation \
       problem wrt. not visiting \"2\" case after \"1\" transitions\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to A;\n\
       \t\t\t3() from A to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \t2() from B to C;  // Testing CommandLine with, e.g., -fsm(dot) arg \
       (disamb error before projection passes means graph cannot be built)\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A  // -oldwf, default, -fair\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t//2() from A to C;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t3() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t() from B to C;\n\
       \t\t1() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t() from B to C;  // Classic mergeability (no \"equiv.\" protocol \
       by sequencing -- unless maybe a generous async. equiv.)\n\
       \t\t2() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to C;\n\
       \t\t//3() from C to A;\n\
       \t\t3() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t5() from B to C;\n\
       \t\t//6() from C to A;\n\
       \t\t5() from C to B;  // Mergeable -- A not involved downstream\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       \t3() from A to B;  // Sequencing after recursive-choice\n\
       \t3() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1(Proto2@A) from A to B;  // Testing delegation payload projection\n\
       }\n\n\
       global protocol Proto2(role A, role B)\n\
       {\n\
       \t2() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(Proto1@A) from A to B;  // Testing recursive protocoldecls\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(Proto2@A) from A to B;  // Testing recursive protocoldecls\n\
       }\n\n\
       global protocol Proto2(role A, role B)\n\
       {\n\
       \t//(Proto1@A) from A to B;\n\
       \t(Proto3@A) from A to B;\n\
       \t//do Proto3(A, B);\n\
       }\n\n\
       global protocol Proto3(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       \t//(Proto1@A) from A to B;\n\
       \t//(Proto2@A) from A to B;\n\
       \t//do Proto1(A, B);\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t4() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t5() from B to C;  // Mergeable subset of choice cases (cf. \
       syntactic merge?\n\
       \t\t5() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from A to B;\n\
       \t\t5() from B to C;\n\
       \t\t5() from C to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdo Game(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       }\n\n\
       aux global protocol Game(role A, role B, role C)\n\
       {\n\
       \t() from A to B;\n\
       \t() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t//do Proto1Aux(A, B);\n\
       \t\t}\n\
       \t}\n\
       }\n\n\
       aux global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \tcontinue X;  // Still checked for aux\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1<sig M>(role A, role B)\n\
       {\n\
       \tdo Proto1Aux<M>(A, B);\n\
       }\n\n\
       aux global protocol Proto1Aux<sig M>(role A, role B)\n\
       {\n\
       \tM from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(Test.Foo) from A to B;  // Testing DataType disamb\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tInt from A to B;  // Testing disamb (and AST visitChild)\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       sig <java> \"...\" from \"...\" as M;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t(M) from A to B;  // Testing disamb (and AST visitChild)\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;  // Testing \"fair\"/\"unfair\" liveness -- \
       issue of global liveness vs. local subtyping) -- generating \"unfair \
       output subtyped\" global model (can it be done simply by terminal \
       set role check?)\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;  // Testing non-det unfairClone -- \
       uninteresting test because non-det single-action-edge choice-merges \
       get implicitly minimised (by graph building)\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t3() from B to A;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;  // Testing non-det unfairClone\n\
       \t\t\t3() from B to A;\n\
       \t\t\t//4() from B to A;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect B to C;\n\
       \tconnect A to C;  // How does C \"correlate\" A/B connections to \
       sessions -- introduces? -- need to study real examples\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect B to C;  // C doesn't care whether each connection is \
       new or old session?\n\
       \t\t2() from B to C;\n\
       \t\tdisconnect B and C;\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tdisconnect A and B;\n\
       \tconnect A to B;  // What does it mean for B to leave the session \
       and rejoin?  Need some condition like if fully leave, then rejoining \
       should be same as new session? (i.e. this case bad) -- Consider \
       implementability\n\
       \tdisconnect A and B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() connect A to C;\n\
       \t\t\tdisconnect A and C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() connect A to C;\n\
       \t\t\tdisconnect A and C;\n\
       \t\t\tcontinue X\n\
       \t\t}\n\
       \t}\n\
       \tconnect A to C;  // Good until here, now bad because C already \
       fully left?\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tconnect A to B;  // What does this mean in terms of a \
       \"session\"? (should consider the implementation, session id, ...) \
       -- distinction between initial accept and in-session accepts?\n\
       \t\t//1() from A to B;\n\
       \t\t//1() from B to A;\n\
       \t\tdisconnect A and B;  // Shouldn't allow continuation after a \
       certain point?  no session structure left?\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect B to C;\n\
       \t\tconnect A to C;\n\
       \t\t1() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tconnect A to C;  // Inconsistent choice connect subjects\n\
       \t}\n\
       }\n\
       //*/\n\n\
       //***\n\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdisconnect A and B;  // CHECKME: disconnect shouldn't have \
       asymmetric src/dest -- check enabling conditions wrt. choices, \
       projection, etc\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1(Int) connect A to B;  // Testing non-det payloads for \
       message-connects\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() connect A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() connect B to C;\n\
       \t\t\t2() from B to C;\n\
       \t\t\tdisconnect B and C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to B;\n\
       \t\t\t3() connect B to C;  // Cf. PartnershipSupplier filter subproto\n\
       \t\t\t3() from B to C;\n\
       \t\t\tdisconnect B and C;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\trec Y\n\
       \t\t\t{\n\
       \t\t\t\tchoice at B\n\
       \t\t\t\t{\n\
       \t\t\t\t\t3() from B to A;\n\
       \t\t\t\t\tcontinue X;\n\
       \t\t\t\t}\n\
       \t\t\t\tor\n\
       \t\t\t\t{\n\
       \t\t\t\t\t4() from B to A;\n\
       \t\t\t\t\tcontinue Y;  // Needs fairness\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tchoice at B  // C not involved, but still live (without \
       fairness)\n\
       \t\t\t{\n\
       \t\t\t\t3() from B to A;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t4() from B to A;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tchoice at B\n\
       \t\t\t{\n\
       \t\t\t\t3() from B to A;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\trec Y  // Bad (fair or not)\n\
       \t\t\t\t{\n\
       \t\t\t\t\t4() from B to A;\n\
       \t\t\t\t\tcontinue Y;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;  // Live, without fairness\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;  // CHECKME: bad sequence if commented, \
       correct? -- bad sequence because C not in block so projection \
       pruned, then only rec-block left -- is this satisfactory? (consider \
       standalone global semantics vs. global as syntactic sugar for \
       locals) -- however, \"bad sequence\" restriction probably does not \
       hurt expressiveness\n\
       \t}\n\
       \t3() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \trec X  // Testing non-fair EFSM generation\n\
       \t{\n\
       \t\tchoice at B\n\
       \t\t{\n\
       \t\t\t3() from B to A;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from B to A;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\twrap B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\twrap A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tdisconnect A and B;  // Testing unfairClone terminal state \
       reconcilliation -- FIXME: this example doesn't actually test this, \
       cf. SupplierInfoExplicit for requestor\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \t//3() from A to C;  // Testing fair/unfair liveness for C\n\
       \t3() from C to A;  // Message liveness also subject to fair/unfair\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t() from B to C;\n\
       \t\t3() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t() from B to C;  // choice + sequencing not the same as just \
       syntactic sugar for factoring out a common branch continuation, i.e. \
       cannot factor out \"() from B\" as a continuation without either \
       losing causality for output to A or changing output order at B -- \
       arguable that changing order at B is equivalent (for some \
       equivalence), but not equivalent under basic bisimilarity\n\
       \t\t4() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;  // CHECKME: empty case for C discarded by \
       projection, but do we need tau for correctness?  this example works \
       because it ends up as stuck error -- a point is generating tau and \
       then bisim-minimising doesn't remove the tau, whereas the current \
       projection does remove the tau (so current projection is not equiv \
       to bisim-minimisation intuition)\n\
       \t\t\t\t// should be OK: intiution: an input-state endpoint cannot \
       choose to not receive a message, i.e. input states should never have \
       tau -- so whole system must satisfy properties when modelling \
       \"partial\" local branches as a \"complete\" branch for just the \
       involved choice cases (i.e. ignore any non-involved cases)\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from A to C;\n\
       \t}\n\
       \t3() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \t3() from A to C;  // Not \"strongly\" live for C -- by subtyping, \
       an implementation of A may never terminate -- strongly live probably \
       means all roles have to be involved in every choice path -- not \
       quite: it all depends on definition of subtyping, could make a \
       \"live\" notion of subtyping that doesn't allow a non-live subset of \
       choices -- this also depends on the select primitives and typing \
       rules (it could come down to decidability of if-conditions...) -- \
       problem is, even without subtyping, select primitive is always about \
       selecting just one case, can't really make a \"live\" typing on top \
       of that... -- could be positioned as basic session typing needs \
       strong liveness, while weak liveness can be aimed at assuming a more \
       general program verification -- or maybe a more powerful \
       \"imperative style\" typing system could work, e.g. while (...) { \
       ..non-live choice on s..} ..live choice on s.., i.e. it is ok to \
       select a non-live case if the while will terminate to eventually \
       lead us to a live case.. -- this fairness/liveness issue is another \
       \"bondary\" issue between modelling/types/practice, bit like \
       linearity\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       //explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect A to C;  // Testing API gen (without I/O i/f gen)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A  // Testing minfsm\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t2() from A to B;\n\
       \trec X\n\
       \t{\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;  // Testing minfsm\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tcontinue X;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t2() from A to B;\n\
       \trec X\n\
       \t{\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;  // Testing minfsm\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tcontinue X;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;  // Bad\n\
       \tdisconnect A and C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;  // Good: mergeable\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect C to B;  // If A and C are in above, 2() can be stuck at \
       B's connect/accept here -- but stuck error not directly detected, \
       error manifests as B/C deadlock\n\
       \t\t2() from A to B;  // Mergeable if B/C connection not deadlocked\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \t2() from B to C;  // Trying to make an unconnected orphan from B \
       to C, but currently will always get a connectedness error first -- \
       model building semantics shouldn't/won't allow explicit unnconnected \
       orphans, message cannot be sent if not connected\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to C;\n\
       \tconnect C to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to C;\n\
       \t\tconnect A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t\t2() from C to B;  // Bad: connect and msg from different choice \
       subjects\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\tconnect A to B;  // Good: tests recursion pruning for \
       connection actions\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A  // ** using old WF, this breaks connectedness \
       checking -- WFChoiceChecker is an UnfoldingVisitor, but it is prunes \
       the visit on entering the unfolded choice\n\
       \t\t{\n\
       \t\t\tconnect A to B;\n\
       \t\t\tcontinue X;  // Bad\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tconnect B to C;\n\
       \t\t\tdisconnect B and C;  // Comment is bad\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t\t2() from C to A;\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Disable local choice subject inference\n\
       \trec X\n\
       \t{\n\
      \  \t// Testing getTrace?\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t//1() from A to B;\n\
       \t\t\t1() from A to C;\n\
       \t\t\t3() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t\t3() from C to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Disable local choice subject inference\n\
       \trec X\n\
       \t{\n\
       \t\t// Testing getTrace?\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from A to C;\n\
       \t\t\t2() from C to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t3() from A to C;\n\
       \t\t\t3() from C to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;  // Testing graph building\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Partners(\n\
       \t\trole LOGINsvc,\n\
       \t\trole REQUESTOR,\n\
       \t\trole AUTHsvc,\n\
       \t\trole FILTERsvc,\n\
       \t\trole SUPPLIERsvc,\n\
       \t\trole CONTRACTsvc)\n\
       {\n\
       \tlogin() from REQUESTOR to LOGINsvc;\n\
       \tchoice at LOGINsvc\n\
       \t{\n\
       \t\tloginfailure() from LOGINsvc to REQUESTOR;\n\
       \t\t0() from REQUESTOR to AUTHsvc;\n\
       \t\t0() from AUTHsvc to FILTERsvc;\n\
       \t\t0() from AUTHsvc to SUPPLIERsvc;\n\
       \t\t0() from AUTHsvc to CONTRACTsvc;\n\
       \t} or {\n\
       \t\tloginsuccess() from LOGINsvc to REQUESTOR;\n\
       \t\trec MAIN\n\
       \t\t{\n\
       \t\t\tchoice at REQUESTOR\n\
       \t\t\t{\n\
       \t\t\t\tgetsuppliers() from REQUESTOR to AUTHsvc;\n\
       \t\t\t\tchoice at AUTHsvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;  // Bad: \
       testing getTrace performance\n\
       \t\t\t\t\tdeny() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;\n\
       \t\t\t\t\tsuppliers() from SUPPLIERsvc to AUTHsvc;\n\
       \t\t\t\t\tfilterSuppliers() from AUTHsvc to FILTERsvc;\n\
       \t\t\t\t\tfiltered() from FILTERsvc to AUTHsvc;\n\
       \t\t\t\t\tsuppliers() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t}\n\
       \t\t\t} or {\n\
       \t\t\t\tgetcontracts() from REQUESTOR to AUTHsvc;\n\
       \t\t\t\tchoice at AUTHsvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\tdeny() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\tgetcontracts() from AUTHsvc to CONTRACTsvc;\n\
       \t\t\t\t\tcontracts() from CONTRACTsvc to AUTHsvc;\n\
       \t\t\t\t\tfilterContracts() from AUTHsvc to FILTERsvc;\n\
       \t\t\t\t\tfiltered() from FILTERsvc to AUTHsvc;\n\
       \t\t\t\t\tcontracts() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t\tcontinue MAIN;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1\n\
       (\n\
       \t\trole REQuestor,\n\
       \t\trole AUTHsvc,\n\
       \t\trole SUPPLIERsvc,\n\
       \t\trole CONTRACTsvc)\n\
       {\n\
       \trec MAIN\n\
       \t{\n\
       \t\tchoice at REQuestor\n\
       \t\t{\n\
       \t\t\tgetsuppliers() from REQuestor to AUTHsvc;\n\
       \t\t\tchoice at AUTHsvc\n\
       \t\t\t{\n\
       \t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;  // Bad: testing \
       getTrace\n\
       \t\t\t\tdeny() from AUTHsvc to REQuestor;\n\
       \t\t\t} or {\n\
       \t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;\n\
       \t\t\t\tsuppliers() from SUPPLIERsvc to AUTHsvc;\n\
       \t\t\t\tsuppliers() from AUTHsvc to REQuestor;\n\
       \t\t\t}\n\
       \t\t} or {\n\
       \t\t\tgetcontracts() from REQuestor to AUTHsvc;\n\
       \t\t\tchoice at AUTHsvc\n\
       \t\t\t{\n\
       \t\t\t\tdeny() from AUTHsvc to REQuestor;\n\
       \t\t\t} or {\n\
       \t\t\t\tgetcontracts() from AUTHsvc to CONTRACTsvc;\n\
       \t\t\t\tcontracts() from CONTRACTsvc to AUTHsvc;\n\
       \t\t\t\tcontracts() from AUTHsvc to REQuestor;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tcontinue MAIN;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\n\
       /*\n\
       global protocol Proto(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdisconnect A and B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;  // B can still send even if A disconnects \
       (async) -- so not wait-for from that situation\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*/\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\n\
       \t\t1() from A to C;\n\
       \t\t1() from A to C;\n\n\
       \t\t3() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\n\
       \t\t() from B to C;  // Trying to get A into above block while C is \
       in here, such that WF1 forces C also into the above -- so that WF1 \
       becomes unsound\n\
       \t\t2() from A to C;\n\n\
       \t\t4() from C to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\t1() from C to B;\n\
       \t\t1() from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from C to B;\n\
       \t\t2() from C to A;\n\n\
       \t\t2() from A to B;\n\
       \t\t2() from A to B;\n\n\
       \t\t2() from A to C;\n\n\
       \t\t2() from C to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t1() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\n\
       \t2() from C to B;  // TODO: investigate: WF_1 won't get past here, \
       is it OK?\n\
       \t2() from C to B;  // ..becomes \"fake\" role liveness problem \
       because of WF1 -- not live even assuming fairness\n\
       \t// ..dragons\n\n\
       \t// TODO: investigate reachability of local states in global model\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\t0() from B to A;\n\
       \t\t2() from C to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\trec X\n\
       \t\t\t{\n\
       \t\t\t\tchoice at A\n\
       \t\t\t\t{\n\
       \t\t\t\t\t1() from A to B;\n\
       \t\t\t\t\tcontinue X;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\t1() from B to A;\n\
       \t\t2() from C to B;  // Good\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\t0() from B to A;\n\n\
       \t\t//2() from C to B;\n\
       \t\t//2() from C to B;\n\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\trec X\n\
       \t\t\t{\n\
       \t\t\t\tchoice at A\n\
       \t\t\t\t{\n\
       \t\t\t\t\t1() from A to B;\n\
       \t\t\t\t\t//2() from C to B;\n\
       \t\t\t\t\tcontinue X;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t0() from B to C;\n\
       \t\t1() from B to A;\n\
       \t}\n\n\
       \t2() from C to B;  // TODO: investigate: WF_1 won't get past here, \
       is it OK?\n\
       \t2() from C to B;  // ..becomes \"fake\" role liveness problem \
       because of WF1\n\
       \t// ...\n\n\
       \t// TODO: investigate reachability of local states in global model\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Trying to construct a counterexample for WF1 soundness..\n\
       \t// ..need to find an error state that is unreachable by WF1 but \
       reachable by e.g. WF2\n\
       \t// Try to find a choice where B is falsely committed to a branch \
       due to WF1 (so state space of model is unsoundly restricted)\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;  // A cannot do this in WF1 unless B receives a \
       1() first\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from A to C;\n\
       \t\t//2() from A to B;\n\
       \t\t\t\t// Local choice subjects require B to also receive from A in \
       this block\n\
       \t\t\t\t// Two cases: same or different label\n\
       \t\t\t\t// If different label, then no possibility of false branch \
       commitment\n\
       \t\t\t\t// If same label, then non-det always allows B to enter this \
       block, even under WF1\n\
       \t\t\t\t// Therefore: false choice commitment not possible\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B  // Good non-det EFSM for A (including \
       minimisation)\n\
       \t\tconnect A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t//connect A to C;  // Tests -minfsm\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Needs local choice subject disabled, though the point is it's \
       still bad\n\
       \tchoice at A\n\
       \t{\n\
       \t\ta() from A to B;\n\
       \t\t//d() from C to B;  // Moved down to make C enabled\n\
       \t\tcprime() from A to C;\n\
       \t\td() from C to B;\n\
       \t\tb() from B to A;\n\
       \t\ty() from A to B;  // Orphan\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tc() from A to C;\n\
       \t\td() from C to B;  // ..point is B could get in this case, while \
       A (and C) are in the other\n\
       \t\tb() from B to A;\n\
       \t\ta() from A to B;  // (..not stuck msg error because the a() from \
       above is consumed here)\n\
       \t\tx() from B to A;  // Orphan\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Counterexample for WF1 if local choice subject disabled\n\
       \tchoice at A\n\
       \t{\n\
       \t\ta() from A to B;\n\
       \t\ta() from A to B;  // WF1 unnaturally resolves the non-det choice \
       at B by forcing B to commit to this branch before C is enabled\n\
       \t\tcprime() from A to C;\n\
       \t\td() from C to B;\n\
       \t\tb() from B to A;\n\n\
       \t\t//y() from A to B;  // Potential orphan\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tc() from A to C;\n\
       \t\td() from C to B;\n\
       \t\tb() from B to A;  // Must come before a's, to prevent reverse \
       choice race\n\
       \t\ta() from A to B;\n\
       \t\ta() from A to B;\n\n\
       \t\t//x() from B to A;  // Potential orphan\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Disable local choice subject inference -- mergeable\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t2() from C to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t\t2() from C to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t2() from C to B;\n\n\
       \t\t3() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t\t2() from C to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\n\
       \t\t//3() from B to C;\n\
       \t\t4() from B to C;  // Bad\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t2() from C to B;\n\
       \t\t2() from C to B;\n\n\
       \t\t3() from B to C;  // Should be potential stuck\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t\t2() from C to B;\n\
       \t\t2() from C to B;\n\
       \t\t() from C to A;  // Another counterexample to WF1 (B falsely \
       committed to here when A/C are)\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to B;\n\n\
       \t\t4() from B to C;  // Should be potential stuck\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tcontinue X;  // Test graph building: EndpointGraph null exit\n\
       \t}\n\
       }\n\
       */\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect from B to C;\n\
       \t\t1() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tconnect from B to C;\n\
       \t\t2() from B to C;  // Not mergeable (connect/accept is unit \
       message)\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Good\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tconnect B to C;  // Test wait-for error detection: C is \
       non-initial accept, but B is a corresponding connect\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Bad\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \t0() from A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tconnect B to C;  // Test safe termination check for non-initial \
       accept states\n\
       \t\t3() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Good\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \t0() from A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tconnect B to C;\n\
       \t3() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \t0() from A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tconnect B to C;  // Tests role liveness check for non-terminal \
       (accept) states\n\
       \t3() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;  // Basic test for connection deadlock detection\n\
       \t1() from A to B;\n\
       }\n\
       //*/\n\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tconnect B to C;  // Good because C is initial\n\
       \t\t2() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect C to B;  // Deadlock\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\tconnect B to C;  // Good\n\
       \t\t2() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tconnect A to D;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t1() from A to D;\n\
       \t\tconnect B to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t1() from A to D;\n\
       \t\tconnect C to D;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t1() from A to D;\n\
       \t\tconnect D to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tconnect A to D;\n\
       \t// Good\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t1() from A to D;\n\
       \t\tconnect B to C;\n\
       \t\tconnect C to D;\n\
       \t\tconnect D to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t1() from A to D;\n\
       \t\tconnect B to C;\n\
       \t\tconnect C to D;\n\
       \t\tconnect D to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t\t1() from A to D;\n\
       \t\tconnect B to C;\n\
       \t\tconnect C to D;\n\
       \t\tconnect D to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tconnect A to C;\n\
       \t\t3() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tconnect A to C;  // Mixed send/connect state at A\n\
       \t\t2() from A to B;\n\
       \t\t3() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect from A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t1() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t//3() from A to B;  // Uncomment is fix\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tconnect from C to B;  // Not role live: connect is a sync action\n\
       \t3() from C to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect from A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at A\n\
       \t\t\t{\n\
       \t\t\t\t1() from A to B;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \tconnect from B to C;  // Good: connect is a sync action, but \
       \"asymmetric\"\n\
       \t3() from C to B;\n\
       }\n\
       //*/\n\n\n\
       /*  // disconnect\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from B to C;\n\
       \t\tdisconnect B and C;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       \t3() from A to B;\n\
       \t//3() from B to C;  // Uncomment is bad\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Good\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t2() from A to C;\n\
       \t\tconnect B to C;\n\
       \t\tdisconnect B and C;\n\
       \t}\n\
       \tconnect B to C;\n\
       \t2() from B to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \t1() from A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdisconnect A and B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tdisconnect A and B;\n\
       \t}\n\
       \t3() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \t1() from A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdisconnect A and B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t3() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tdo Proto1Aux(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t//connect B to C;  // Bad if non-det\n\
       \t\t1() connect B to C;\n\
       \t\t1() from B to C;\n\
       \t\tdisconnect B and C;\n\
       \t\tdo Proto1Aux(A, B, C);  // FIXME: guarded do has same subprotsig \
       has an unguarded one (and ProjectedSubprotPruner is using \
       subprotsigs...) -- but current problem is actually ambiguous connect \
       enabling of C by B -- anyway to have explicit protocol with multiple \
       instances of the same do-subprotsig with and without choice-guards?\n\
       \t\t\t// TODO: this doesn't just break subprot pruning, but affects \
       whole subprot visiting framework because using current subprot \
       \"cycles\" may not provide coverage of all \"full recursive loops\" \
       (cf. unfolding/unrolling all rec/continues) -- current framework \
       gives coverage only if recursive subprots are only called from a \
       single \"location\" -- maybe fixable by generalising subprotsigs \
       record more than just the immediate proto+args context -- luckily \
       some analyses like enabling check are OK despite this\n\
       \t\t\t// basically: current subprotocolsigs do identify a common \
       \"state\" entry, but cannot be used to identify \"specific\" cycle \
       paths or \"maximal recursion paths\"\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\tdo Proto1Aux(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\t//connect B to C;  // Bad if non-det\n\
       \t\t2() connect B to C;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Good\n\
       \tconnect A to B;\n\
       \tconnect B to C;\n\
       \t1() from A to B;\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t3() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\tdo Proto1Aux2(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4() from A to B;\n\
       \t\t4() from B to C;\n\
       \t\t//do Proto1Aux2(A, B, C);\n\
       \t}\n\
       }\n\n\
       aux global protocol Proto1Aux2(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t5() from A to B;\n\
       \t\tdo Proto1Aux1(A, B, C);  // Testing graph building (C vs. A/B)\n\
       \t\t//do Proto1Aux2(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t6() from A to B;\n\
       \t\t//connect B to C;\n\
       \t\t6() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect B to C;\n\
       \t1() from A to B;\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t3() from A to B;\n\
       \t\t//3() from B to C;\n\
       \t\tdo Proto1Aux2(A, B, C);  // Testing mutually choice-unguarded \
       \"prunable\" do's\n\
       \t}\n\
       }\n\n\
       aux global protocol Proto1Aux2(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t5() from A to B;\n\
       \t\tdo Proto1Aux1(A, B, C);\n\
       \t\t//do Proto1Aux2(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t6() from A to B;\n\
       \t\t//connect B to C;\n\
       \t\t6() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \t1() from A to B;\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux1(role A, role B, role C)\n\
       {\n\
       \tdo Proto1Aux2(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux2(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tdo Proto1Aux1(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from A to B;\n\
       \t\tconnect B to C;\n\
       \t\t4() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect B to C;\n\
       \t1() from A to B;\n\
       \t//1() from B to C;\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux1(role A, role B, role C)\n\
       {\n\
       \t() from A to B;\n\
       \t//() from B to C;\n\
       \tdo Proto1Aux2(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux2(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tdo Proto1Aux1(A, B, C);\n\
       \t\t//do Proto1Aux2(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from A to B;\n\
       \t\t//connect B to C;\n\
       \t\t4() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tdo Proto1Aux(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\tdo Proto1Aux(A, B, C);  // ProjectedSubprotocolPruner\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t\tconnect B to C;\n\
       \t\t3() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from A to B;\n\
       \t}\n\
       \tconnect A to C;  // Good\n\
       \t4() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tconnect B to C;\n\
       \t1() from A to B;\n\
       \tchoice at B\n\
       \t{\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\t2() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3() from B to C;\n\
       \t}\n\
       \tconnect A to C;  // Bad: role liveness\n\
       \t4() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t//2() from A to B;  // Uncomment bad\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tconnect B to C;\n\
       \t\t\t4() from B to C;\n\
       \t\t\t5() from C to B;\n\
       \t\t\tdisconnect B and C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from B to A;\n\
       \t\tconnect B to C;\n\
       \t\tconnect C to D;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t1() from B to A;\n\
       \t\tconnect B to D;\n\
       \t\tconnect D to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\tconnect A to B;\n\
       \t\t//connect B to C;  // Uncomment bad\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tconnect A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int1;\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int2;\n\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux1(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\t1() from B to A;\n\
       \t\tdo Proto1Aux2<Int1>(A, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from B to A;\n\
       \t\tdo Proto1Aux2<Int2>(A, C);\n\
       \t}\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux2<type T>(role A, role C)\n\
       {\n\
       \t// Bad: non-det connect followed by distinct payloads\n\
       \tconnect A to C;\n\
       \t3(T) from A to C;\n\
       \t4() from C to A;\n\
       \tdisconnect A and C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int1;\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int2;\n\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Good by basic model checking, but..\n\
       \tconnect A to B;\n\
       \tconnect A to C;\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux1(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\t1() from B to A;\n\
       \t\tdo Proto1Aux2<Int1>(A, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from B to A;\n\
       \t\tdo Proto1Aux2<Int2>(A, C);  // Bad: payload\n\
       \t}\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux2<type T>(role A, role C)\n\
       {\n\
       \t3(T) from A to C;\n\
       \t4() from C to A;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       // Good: same payload\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int1;\n\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \t// Good\n\
       \tconnect A to B;\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux1(role A, role B, role C)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\t1() from B to A;\n\
       \t\tdo Proto1Aux2<Int1>(A, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from B to A;\n\
       \t\tdo Proto1Aux2<Int1>(A, C);\n\
       \t}\n\
       \tdo Proto1Aux1(A, B, C);\n\
       }\n\n\
       aux global protocol Proto1Aux2<type T>(role A, role C)\n\
       {\n\
       \tconnect A to C;\n\
       \t3(T) from A to C;\n\
       \t4() from C to A;\n\
       \tdisconnect A and C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect from A to B;\n\
       \tdo Proto1Aux(A, B);\n\
       }\n\n\
       aux explicit global protocol Proto1Aux(role A, role B)\n\
       {\n\
       \t1() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \t// Basic connectedness tests\n\
       \tconnect A to B;\n\
       \t//connect B to A;\n\
       \tdisconnect A and B;\n\
       \t//1() from A to B;\n\
       \t//disconnect A and B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\tconnect B to C;\n\
       \t\t\t2() from B to C;\n\
       \t\t\tconnect C to A;\n\
       \t\t\t3() from C to A;\n\
       \t\t\t//3() from A to B;  // Uncomment OK\n\
       \t\t\tdisconnect B and C;\n\
       \t\t\tdisconnect C and A; // Comment bad\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from A to B;\n\
       \t\t}\n\
       \t}\n\
       \t//disconnect A and C;  // Uncomment bad\n\
       \tdisconnect A and B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at B\n\
       \t\t{\n\
       \t\t\t// Good, even though A not involved here\n\
       \t\t\tconnect B to C\n\
       \t\t\t2() from B to C;\n\
       \t\t\t3() from C to B;\n\
       \t\t\tdisconnect B and C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from B to A;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tconnect A to B\n\
       \t\t1() from A to B;\n\
       \t\t2() from B to A;\n\
       \t\tdisconnect A and B;\n\
       \t\tconnect B to A\n\
       \t\t3() from A to B;\n\
       \t\t4() from B to A;\n\
       \t\tdisconnect B and A;\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tdisconnect A and B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tconnect A to B;\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t\t//continue X;  // Uncomment bad\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tconnect B to C;\n\
       \t\t\t1() from B to C;\n\
       \t\t\t//continue X;  // Uncomment bad\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C, role D)\n\
       {\n\
       \tconnect A to B;\n\
       \t1() from A to B;\n\
       \t//connect D to C;  // Uncomment OK\n\
       \t1() from C to D;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;  // Comment bad\n\
       \t1() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\tconnect A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;  // Bad: caught syntactically -- this blocked \
       choice case won't manifest as an error state in the presence of a \
       non-blocked co-case\n\
       \t\t// FIXME: could add implicit error actions (e.g. unconncted \
       message passing) to model building, to make error states explicit\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\tconnect A to B;\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tconnect A to B;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \t// Good\n\
       \tconnect A to B;\n\
       \tchoice at B\n\
       \t{\n\
       \t\t1() from B to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at B\n\
       \t{\n\
       \t\tconnect A to B;  // Bad\n\
       \t\t1() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\tconnect A to B;\n\
       \t\t1() from B to A;  // Good\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at B  // Good\n\
       \t{\n\
       \t\t1() from B to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tconnect B to C;\n\
       \t\t\t1() from B to C;\n\
       \t\t\t//continue X;  // Uncomment bad\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B)\n\
       {\n\
       \tconnect A to B;\n\
       \t//connect A to B;  // Uncomment bad\n\
       \t//connect B to A;  // Uncomment bad\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       explicit global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tconnect A to B;\n\
       \t1() from A to B;\n\
       \t//connect A to C;  // Uncomment OK\n\
       \t2() from A to C;\n\
       }\n\
       //*/\n\n" )
  ; ( "from-scribble-java/tmp/Betty.scr"
    , "module Betty;\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
       /*\n\
       type <java> \"test.nego.SAP\" from \"test/nego/SAP.java\" as SAP;\n\n\
       global protocol Negotiate(role C, role P) {\n\
       \tpropose(SAP) from C to P;\n\
       \trec X {\n\
       \t\tchoice at P {\n\
       \t\t\taccpt() from P to C;\n\
       \t\t\tconfirm() from C to P;\n\
       \t\t} or {\n\
       \t\t\treject() from P to C;\n\
       \t\t} or {\n\
       \t\t\tpropose(SAP) from P to C;\n\
       \t\t\tchoice at C {\n\
       \t\t\t\taccpt() from C to P;\n\
       \t\t\t\tconfirm() from P to C;\n\
       \t\t\t} or {\n\
       \t\t\t\treject() from C to P;\n\
       \t\t\t} or {\n\
       \t\t\t\tpropose(SAP) from C to P;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\n\
       global protocol Proto1(role A, role B) {\n\
       \t123(Int, String) from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;\n\
       \t} or {\n\
       \t\t3() from A to B;\n\
       \t\t4() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;\n\
       \t} or {\n\
       \t\t4() from A to C;\n\
       \t\t3() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\tbuyer1(Int) from A to B;  // Total\n\
       \t\t(Int) from B to A;        // B will pay this much\n\
       \t\tbuyer2(Int) from A to C;  // C will pay remainder\n\
       \t} or {\n\
       \t\tbuyer1(Int) from A to C;  // Total\n\
       \t\t(Int) from C to A;        // C will pay this much\n\
       \t\tbuyer2(Int) from A to B;  // B will pay remainder\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t1() from B to C;\n\
       \t} or {\n\
       \t\t//2() from A to B;\n\
       \t\t2() from B to A;\n\
       \t\tchoice at B {\n\
       \t\t\t3() from B to C;\n\
       \t\t} or {\n\
       \t\t\t4() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to A;\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t5() from C to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to A;\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\tdo Merge(A, C);\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\tdo Merge(A, C);\n\
       \t}\n\
       }\n\n\
       aux global protocol Merge(role A, role C) {\n\
       \t4() from A to C;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\tdo Merge(A, C);\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\tdo Merge(A, C);\n\
       \t}\n\
       }\n\n\
       aux global protocol Merge(role A, role C) {\n\
       \t4() from C to A;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\tdo Merge(A, C);\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t3() from B to C;\n\
       \t\tdo Merge(A, C);\n\
       \t}\n\
       }\n\n\
       aux global protocol Merge(role A, role C) {\n\
       \tchoice at A {\n\
       \t\t4() from A to C;\n\
       \t} or {\n\
       \t\t5() from A to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;\n\
       \t\t3() from B to C;\n\
       \t\t4() from C to A;\n\
       \t} or {\n\
       \t\t1b() from A to B;\n\
       \t\t3() from B to C;  // -nolocalchoicecheck to show orphans\n\
       \t\t4() from C to A;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;\n\
       \t} or {\n\
       \t\t3() from A to B;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B) {\n\
       \trec X {\n\
       \t\tchoice at A {\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t\t2() from A to B;\n\
       \t\t} or {\n\
       \t\t\t3() from A to B;\n\
       \t\t}\n\
       \t\t4() from A to B;\n\
       \t}\n\
       \t5() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B) {\n\
       \trec X {\n\
       \t\t1() from A to B;\n\
       \t\tcontinue X;\n\
       \t}\n\
       \t2() from A to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C, role D) {\n\
       \trec X {\n\
       \t\t1() from A to B;\n\
       \t\tcontinue X;\n\
       \t}\n\
       \t2() from C to D;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B) {\n\
       \trec X {\n\
       \t\tchoice at A {\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t} or {\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\trec X {\n\
       \t\t\t1() from A to B;\n\
       \t\t\t1() from B to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\trec X {\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t\t2() from B to C;\n\
       \t}\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \tchoice at A {\n\
       \t\trec X {\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t} or {\n\
       \t\t2() from A to B;\n\
       \t}\n\
       \t2() from C to B;\n\
       }\n\
       //*/\n\n\n\
       /*\n\
       global protocol Proto1(role A, role B, role C) {\n\
       \trec X {\n\
       \t\tchoice at A {\n\
       \t\t\t1() from A to B;\n\
       \t\t\tcontinue X;\n\
       \t\t} or {\n\
       \t\t\t2() from A to B;\n\
       \t\t\t2() from B to C;\n\
       \t\t}\n\
       \t}\n\
       }\n\
       //*/\n\n" )
  ; ( "from-scribble-java/demo/fase17/travel/TravelAgent.scr"
    , "//$ bin/scribblec.sh \
       scribble-core/src/test/scrib/demo/fase17/travel/TravelAgent.scr -ip \
       scribble-core/src/test/scrib/ -d scribble-core/src/test/scrib -api \
       TravelAgent C -V\n\
       //$ javadoc -cp \
       scribble-cli/target/classes/';'scribble-core/target/classes';'scribble-parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar;'scribble-core/src/test/scrib \
       -subpackages demo.fase17.travel.TravelAgent.TravelAgent -d \
       test/doc/fase17/travel\n\n\n\
       module demo.fase17.travel.TravelAgent;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\n\n\
       explicit global protocol TravelAgent(role C, role A, role S)\n\
       {\n\
       \tconnect C to A;\n\
       \tdo Nego(C, A, S);\n\
       }\n\n\
       aux global protocol Nego(role C, role A, role S)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tquery(String) from C to A;\n\
       \t\tquote(Int) from A to C;\n\
       \t\tdo Nego(C, A, S);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Pay(C, A, S);\n\
       \t}\n\
       }\n\
       \t\t\n\
       // has to be aux (if default, then C/S duplicate connection; if \
       explicit, C/A not connected)\n\
       aux global protocol Pay(role C, role A, role S)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tconnect C to S;\n\
       \t\tpayment(String) from C to S;\n\
       \t\tconfirm(Int) from S to C;\n\
       \t\taccpt(Int) from C to A;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\treject() from C to A;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/demo/fase17/overview/P1.scr"
    , "module demo.fase17.overview.P1;\n\n\n\
       global protocol P1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from A to C;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/demo/fase17/travel2/TravelAgent2.scr"
    , "//$ bin/scribblec.sh \
       modules/core/src/test/scrib/demo/fase17/travel2/TravelAgent2.scr -ip \
       modules/core/src/test/scrib/ -d modules/core/src/test/scrib \
       -nocorrelation\n\
       //$ javadoc -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar;'modules/core/src/test/scrib \
       -subpackages demo.fase17.travel2.TravelAgent2.TravelAgent2 -d \
       test/doc/fase17/travel2\n\n\n\
       module demo.fase17.travel2.TravelAgent2;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\n\n\
       explicit global protocol TravelAgent2(role C, role A, role S)\n\
       {\n\
       \tconnect C to A;\n\
       \tdo Nego(C, A, S);\n\
       }\n\n\
       aux global protocol Nego(role C, role A, role S)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tquery(String) from C to A;\n\
       \t\tquote(Int) from A to C;\n\
       \t\tdo Nego(C, A, S);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tdo Pay(C, A, S);\n\
       \t}\n\
       }\n\
       \t\t\n\
       aux global protocol Pay(role C, role A, role S)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\taccpt() from C to A;\n\
       \t\tconnect A to S;\n\n\
       \t\tport(Int) from S to A;\n\
       \t\tport(Int) from A to C;           // A should forward prev Int to C\n\
       \t\tpayment(String) connect C to S;  // C should connect to S at \
       prev Int port  // FIXME: connect/accept message\n\n\
       //\t\tport(p:Int) from S to A;\n\
       //\t\tport(p) from A to C;\n\
       //\t\tpayment(String) connect C to S;  @port=p\n\n\
       \t\tconfirm(Int) from S to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\treject() from C to A;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/demo/fase17/intro/FirstR.scr"
    , "module demo.fase17.intro.FirstR;\n\n\n\
       explicit global protocol P2(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() connect A to B;\n\
       \t\t\tdisconnect A and B;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() connect A to C;\n\
       \t\t\tdisconnect A and C;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/demo/fase17/intro/FirstL.scr"
    , "module demo.fase17.intro.FirstL;\n\n\n\
       global protocol P1(role A, role B, role C)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\tchoice at A\n\
       \t\t{\n\
       \t\t\t1() from A to B;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t2() from A to C;\n\
       \t\t}\n\
       \t\tcontinue X;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfoExper.scr"
    , "module src.SupplierInfoExper;\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <xsd> \"...\" from \"...xsd\" as usercontext;\n\
       type <xsd> \"...\" from \"...xsd\" as filters;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\n\
       global protocol PartnershipSupplier(\n\
       \t\trole loginsvc,\n\
       \t\trole requestor)\n\
       {\n\
       \tlogin(username, password) from requestor to loginsvc;\n\
       \tchoice at loginsvc\n\
       \t{\n\
       \t\tloginfailure() from loginsvc to requestor;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tloginsuccess() from loginsvc to requestor;\n\
       \t\tdo Main(loginsvc, requestor);\n\
       \t}\n\
       }\n\n\
       global protocol Main(\n\
       \t\trole loginsvc,\n\
       \t\trole requestor)\n\
       {\n\
       \tloginsvc connects(      // connect/disconnect now done as explicit \
       statements\n\
       \t\t\trole authorisersvc,\n\
       \t\t\trole filtersvc,\n\
       \t\t\trole suppliersvc,\n\
       \t\t\trole contractsvc)\n\
       \t{\n\
       \t\trec mainBlock\n\
       \t\t{\n\
       \t\t\tchoice at requestor\n\
       \t\t\t{\n\
       \t\t\t\t// GET SUPPLIER INFO\n\
       \t\t\t\tgetsuppliers(uuid) from requestor to authorisersvc;\n\
       \t\t\t\tchoice at authorisersvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from authorisersvc to requestor, suppliersvc;\n\
       \t\t\t\t}\n\
       \t\t\t\tor\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\t\t\tgetsuppliers() from authorisersvc to suppliersvc;\n\
       \t\t\t\t\tsuppliers() from suppliersvc to authorisersvc;\n\
       \t\t\t\t\tfilter1(usercontext, filters, supplierdetails) from \
       authorisersvc to filtersvc;\n\
       \t\t\t\t\tfiltered1() from filtersvc to authorisersvc;\n\
       \t\t\t\t\tsuppliers() from authorisersvc to requestor;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t// GET CONTRACT INFO\n\
       \t\t\t\tgetcontracts() from requestor to authorisersvc;\n\
       \t\t\t\t//choice at authoriser\n\
       \t\t\t\tchoice at authorisersvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from authorisersvc to requestor, contractsvc;\n\
       \t\t\t\t}\n\
       \t\t\t\tor\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\t\t\tgetcontracts() from authorisersvc to contractsvc;\n\
       \t\t\t\t\tsuppliers() from contractsvc to authorisersvc;\n\
       \t\t\t\t\tfilter2(usercontext, filters, supplierdetails) from \
       authorisersvc to filtersvc;\n\
       \t\t\t\t\tfiltered2() from filtersvc to authorisersvc;\n\
       \t\t\t\t\tcontracts() from authorisersvc to requestor;\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t\tcontinue mainBlock;\n\
       \t\t}\n\
       \t}\n\
       }\n\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfo.scr"
    , "module src.SupplierInfo;\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <xsd> \"UserContext\" from \"AUTH.xsd\" as usercontext;\n\
       type <xsd> \"FilterSet\" from \"AUTH.xsd\" as filters;\n\n\n\
       explicit global protocol Partners(\n\
       \t\trole LOGINsvc,\n\
       \t\trole REQuestor,\n\
       \t\trole AUTHsvc,\n\
       \t\t//role usersvc,\n\
       \t\trole FILTERsvc,\n\
       \t\trole SUPPLIERsvc,\n\
       \t\trole CONTRACTsvc)\n\
       {\n\
       \tconnect REQuestor to LOGINsvc;\n\n\
       \tlogin(username, password) from REQuestor to LOGINsvc;\n\
       \tchoice at LOGINsvc\n\
       \t{\n\
       \t\tloginsuccess() from LOGINsvc to REQuestor;\n\
       \t\t//loginfailure() from LOGINsvc to REQuestor;\n\
       \t} or {\n\
       \t\tloginsuccess() from LOGINsvc to REQuestor;\n\n\
       \t\tconnect REQuestor to AUTHsvc;\n\
       \t\tconnect AUTHsvc to SUPPLIERsvc;\n\
       \t\tconnect AUTHsvc to CONTRACTsvc;\n\
       \t\tconnect AUTHsvc to FILTERsvc;\n\n\
       \t\trec MAIN\n\
       \t\t{\n\
       \t\t\tchoice at REQuestor\n\
       \t\t\t{\n\
       \t\t\t\t// GET SUPPLIER INFO\n\
       \t\t\t\tgetsuppliers(uuid) from REQuestor to AUTHsvc;\n\
       \t\t\t\tchoice at AUTHsvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from AUTHsvc to REQuestor;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQuestor\n\
       \t\t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;\n\
       \t\t\t\t\tsuppliers() from SUPPLIERsvc to AUTHsvc;\n\
       \t\t\t\t\tfilterSuppliers(usercontext, filters, supplierdetails) \
       from AUTHsvc to FILTERsvc;\n\
       \t\t\t\t\tfiltered() from FILTERsvc to AUTHsvc;\n\
       \t\t\t\t\tsuppliers() from AUTHsvc to REQuestor;\n\
       \t\t\t\t}\n\
       \t\t\t\tcontinue MAIN;\n\
       \t\t\t} or {\n\
       \t\t\t\t// GET CONTRACT INFO\n\
       \t\t\t\tgetcontracts() from REQuestor to AUTHsvc;\n\
       \t\t\t\tchoice at AUTHsvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from AUTHsvc to REQuestor;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQuestor\n\
       \t\t\t\t\tgetcontracts() from AUTHsvc to CONTRACTsvc;\n\
       \t\t\t\t\tcontracts() from CONTRACTsvc to AUTHsvc;\n\
       \t\t\t\t\tfilterContracts(usercontext, filters, contractdetails) \
       from AUTHsvc to FILTERsvc;\n\
       \t\t\t\t\tfiltered() from FILTERsvc to AUTHsvc;\n\
       \t\t\t\t\tcontracts() from AUTHsvc to REQuestor;\n\
       \t\t\t\t}\n\
       \t\t\t\tcontinue MAIN;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfoOrig.scr"
    , "module src.SupplierInfoOrig;\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\n\
       global protocol PartnershipSupplier(\trole loginsvc,\n\
       \t\t\t\t\t\t\t\t\t\trole requestor,\n\
       \t\t\t\t\t\t\t\t\t\trole authorisersvc,\n\
       \t\t\t\t\t\t\t\t\t\trole usersvc,\n\
       \t\t\t\t\t\t\t\t\t\trole filtersvc,\n\
       \t\t\t\t\t\t\t\t\t\trole suppliersvc,\n\
       \t\t\t\t\t\t\t\t\t\trole contractsvc)\n\
       {\n\
       \tlogin(username, password) from requestor to loginsvc;\n\
       \tchoice at loginsvc\n\
       \t{\n\
       \t\tloginfailure() from loginsvc to requestor;\n\
       \t} or {\n\
       \t\tloginsuccess() from loginsvc to requestor;\n\
       \t\trec mainBlock\n\
       \t\t{\n\
       \t\t\tchoice at requestor\n\
       \t\t\t{\t\t\t\t\t// GET SUPPLIER INFO\n\
       \t\t\t\tgetsuppliers(uuid) from requestor to authorisersvc;\n\
       \t\t\t\tchoice at authorisersvc\n\
       \t\t\t\t{\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from authorisersvc to requestor;\n\
       \t\t\t\t} or {\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\t\t\tgetsuppliers() from authorisersvc to suppliersvc;\n\
       \t\t\t\t\tsuppliers() from suppliersvc to authorisersvc;\n\
       \t\t\t\t\tfilter(usercontext, filters, supplierdetails) from \
       authorisersvc to filtersvc;\n\
       \t\t\t\t\tfiltered() from filtersvc to authorisersvc;\n\
       \t\t\t\t\tsuppliers() from authorisersvc to requestor;\n\
       \t\t\t\t}\n\
       \t\t\t\tcontinue mainBlock;\n\
       \t\t\t} or {\t\t\t\t// GET CONTRACT INFO\n\
       \t\t\t\tgetcontracts() from requestor to authorisersvc;\n\
       \t\t\t\tchoice at authoriser\n\
       \t\t\t\t{\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from authorisersvc to requestor;\n\
       \t\t\t\t} or {// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\t\t\tgetcontracts() from authorisersvc to contractsvc;\n\
       \t\t\t\t\tsuppliers() from contractsvc to authorisersvc;\n\
       \t\t\t\t\tfilter(usercontext, filters, contractdetails) from \
       authorisersvc to filtersvc;\n\
       \t\t\t\t\tfiltered() from filtersvc to authorisersvc;\n\
       \t\t\t\t\tcontracts() from authorisersvc to requestor;\n\
       \t\t\t\t}\n\
       \t\t\t\tcontinue mainBlock;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfoNoFairBeta.scr"
    , "module src.SupplierInfoNoFairBeta;\n\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <xsd> \"...\" from \"...xsd\" as usercontext;\n\
       type <xsd> \"...\" from \"...xsd\" as filters;\n\n\n\
       explicit global protocol PartnershipSupplier\n\
       (\n\
       \trole loginsvc,\n\
       \trole requestor,\n\
       \trole authorisersvc,\n\
       \trole filtersvc,\n\
       \trole suppliersvc,\n\
       \trole contractsvc\n\
       )\n\
       {\n\
       \tconnect requestor to loginsvc;\n\
       \tlogin(username, password) from requestor to loginsvc;\n\
       \tchoice at loginsvc\n\
       \t{\n\
       \t\tloginfailure() from loginsvc to requestor;\n\
       \t\tdisconnect requestor and loginsvc;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tloginsuccess() from loginsvc to requestor;\n\
       \t\tconnect requestor to authorisersvc;\n\
       \t\tconnect authorisersvc to filtersvc;\n\
       \t\tdo Main(requestor, authorisersvc, filtersvc, suppliersvc, \
       contractsvc);\n\
       \t}\n\
       }\n\n\n\
       aux global protocol Main\n\
       (\n\
       \trole requestor,\n\
       \trole authorisersvc,\n\
       \trole filtersvc,\n\
       \trole suppliersvc,\n\
       \trole contractsvc\n\
       )\n\
       {\n\
       \tchoice at requestor\n\
       \t{\n\
       \t\t// GET SUPPLIER INFO\n\
       \t\tgetsuppliers(uuid) from requestor to authorisersvc;\n\
       \t\tdo SuppInfo(requestor, authorisersvc, filtersvc, suppliersvc);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t// GET CONTRACT INFO\n\
       \t\tgetcontracts() from requestor to authorisersvc;\n\
       \t\tdo ContractInfo(requestor, authorisersvc, filtersvc, contractsvc);\n\
       \t}\n\
       \tdo Main(requestor, authorisersvc, filtersvc, suppliersvc, \
       contractsvc);\n\
       }\n\n\n\
       aux global protocol SuppInfo\n\
       (\n\
       \trole requestor,\n\
       \trole authorisersvc,\n\
       \trole filtersvc,\n\
       \trole suppliersvc\n\
       )\n\
       {\n\
       \tchoice at authorisersvc\n\
       \t{\n\
       \t\t// DENIED\n\
       \t\tdeny() from authorisersvc to requestor;\n\n\
       \t\t0() from authorisersvc to filtersvc;  // FIXME: need \
       accept+message branching, to factor out non-fair filter subproto\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tconnect authorisersvc to suppliersvc;\n\
       \t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\tgetsuppliers() from authorisersvc to suppliersvc;\n\
       \t\tsuppliers() from suppliersvc to authorisersvc;\n\
       \t\tdo FilterInfo\n\
       \t\t\t\t<filterSupplier(usercontext, filters, supplierdetails)>\n\
       \t\t\t\t(authorisersvc, filtersvc);\n\
       \t\tdisconnect authorisersvc and suppliersvc;\n\
       \t\tsuppliers() from authorisersvc to requestor;\n\
       \t}\n\
       }\n\n\n\
       aux global protocol ContractInfo\n\
       (\n\
       \trole requestor,\n\
       \trole authorisersvc,\n\
       \trole filtersvc,\n\
       \trole contractsvc\n\
       )\n\
       {\n\
       \tchoice at authorisersvc\n\
       \t{\n\
       \t\t// DENIED\n\
       \t\tdeny() from authorisersvc to requestor;\n\n\
       \t\t0() from authorisersvc to filtersvc;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tconnect authorisersvc to contractsvc;\n\
       \t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\tgetcontracts() from authorisersvc to contractsvc;\n\
       \t\tcontracts() from contractsvc to authorisersvc;\n\
       \t\tdo FilterInfo\n\
       \t\t\t\t<filterContract(usercontext, filters, contractdetails)>\n\
       \t\t\t\t(authorisersvc, filtersvc);\n\
       \t\tdisconnect authorisersvc and contractsvc;\n\
       \t\tcontracts() from authorisersvc to requestor;\n\
       \t}\n\
       }\n\n\n\
       //aux global protocol FilterInfo<type details>(  // Bad non-det. \
       payload\n\
       aux global protocol FilterInfo\n\
       <\n\
       \tsig Query\n\
       >\n\
       (\n\
       \trole authorisersvc,\n\
       \trole filtersvc\n\
       )\n\
       {\n\
       \t//filter(usercontext, filters, details) from authorisersvc to \
       filtersvc;\n\
       \tQuery from authorisersvc to filtersvc;\n\
       \tfiltered() from filtersvc to authorisersvc;\n\
       }\n\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfoBasic.scr"
    , "module src.SupplierInfoBasic;\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <xsd> \"...\" from \"...xsd\" as usercontext;\n\
       type <xsd> \"...\" from \"...xsd\" as filters;\n\n\n\
       global protocol PartnershipSupplier(\n\
       \t\trole loginsvc,\n\
       \t\trole requestor,\n\
       \t\trole authorisersvc,\n\
       \t\trole filtersvc,\n\
       \t\trole suppliersvc,\n\
       \t\trole contractsvc)\n\
       {\n\
       \tlogin(username, password) from requestor to loginsvc;\n\
       \tchoice at loginsvc\n\
       \t{\n\
       \t\t//loginsuccess() from loginsvc to requestor  // Error if below \
       is commented\n\
       \t\tloginfailure() from loginsvc to requestor;\n\n\
       \t\t//getsuppliers(uuid) from requestor to authorisersvc;  // Error\n\n\
       \t\t0() from requestor to authorisersvc;\n\
       \t\t0() from authorisersvc to filtersvc;\n\
       \t\t0() from authorisersvc to suppliersvc;\n\
       \t\t0() from authorisersvc to contractsvc;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tloginsuccess() from loginsvc to requestor;\n\
       \t\trec mainBlock\n\
       \t\t{\n\
       \t\t\tchoice at requestor\n\
       \t\t\t{\n\
       \t\t\t\t// GET SUPPLIER INFO\n\
       \t\t\t\tgetsuppliers(uuid) from requestor to authorisersvc;\n\n\
       \t\t\t\t1() from authorisersvc to contractsvc;  // Cheaper here than \
       below\n\n\
       \t\t\t\tchoice at authorisersvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from authorisersvc to requestor;\n\n\
       \t\t\t\t\t1() from authorisersvc to suppliersvc;\n\
       \t\t\t\t\t1() from authorisersvc to filtersvc;\n\
       \t\t\t\t}\n\
       \t\t\t\tor\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\t\t\tgetsuppliers() from authorisersvc to suppliersvc;\n\
       \t\t\t\t\tsuppliers() from suppliersvc to authorisersvc;\n\n\
       \t\t\t\t\tfilterSuppliers(usercontext, filters, supplierdetails) \
       from authorisersvc to filtersvc;\n\
       \t\t\t\t\tfiltered() from filtersvc to authorisersvc;\n\
       \t\t\t\t\tsuppliers() from authorisersvc to requestor;\n\
       \t\t\t\t}\n\n\
       \t\t\t\t//1() from authorisersvc to contractsvc;\n\
       \t\t\t\tcontinue mainBlock;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t// GET CONTRACT INFO\n\
       \t\t\t\tgetcontracts() from requestor to authorisersvc;\n\n\
       \t\t\t\t2() from authorisersvc to suppliersvc;\n\n\
       \t\t\t\tchoice at authorisersvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from authorisersvc to requestor;\n\n\
       \t\t\t\t\t2() from authorisersvc to contractsvc;\n\
       \t\t\t\t\t2() from authorisersvc to filtersvc;\n\
       \t\t\t\t}\n\
       \t\t\t\tor\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\t\t\tgetcontracts() from authorisersvc to contractsvc;\n\
       \t\t\t\t\tcontracts() from contractsvc to authorisersvc;\n\n\
       \t\t\t\t\tfilterContracts(usercontext, filters, contractdetails) \
       from authorisersvc to filtersvc;\n\
       \t\t\t\t\tfiltered() from filtersvc to authorisersvc;\n\
       \t\t\t\t\tcontracts() from authorisersvc to requestor;\n\
       \t\t\t\t}\n\n\
       \t\t\t\t//2() from authorisersvc to suppliersvc;\n\
       \t\t\t\tcontinue mainBlock;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfoSubprot.scr"
    , "module src.SupplierInfoSubprot;\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <xsd> \"...\" from \"...xsd\" as usercontext;\n\
       type <xsd> \"...\" from \"...xsd\" as filters;\n\n\n\
       global protocol PartnershipSupplier(\n\
       \t\trole loginsvc,\n\
       \t\trole requestor,\n\
       \t\trole authorisersvc,\n\
       \t\trole filtersvc,\n\
       \t\trole suppliersvc,\n\
       \t\trole contractsvc)\n\
       {\n\
       \tlogin(username, password) from requestor to loginsvc;\n\
       \tchoice at loginsvc\n\
       \t{\n\
       \t\t//loginsuccess() from loginsvc to requestor;  // Only an error \
       if the following line remains uncommented\n\
       \t\tloginfailure() from loginsvc to requestor;\n\
       \t\t//getsuppliers(uuid) from requestor to authorisersvc;\n\
       \t\t0() from requestor to authorisersvc;\n\
       \t\t0() from authorisersvc to filtersvc;\n\
       \t\t0() from authorisersvc to suppliersvc;\n\
       \t\t0() from authorisersvc to contractsvc;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tloginsuccess() from loginsvc to requestor;\n\
       \t\tdo Main(requestor, authorisersvc, filtersvc, suppliersvc, \
       contractsvc);\n\
       \t}\n\
       }\n\n\
       aux global protocol Main(\n\
       \t\trole requestor,\n\
       \t\trole authorisersvc,\n\
       \t\trole filtersvc,\n\
       \t\trole suppliersvc,\n\
       \t\trole contractsvc)\n\
       {\n\
       \tchoice at requestor\n\
       \t{\n\
       \t\t// GET SUPPLIER INFO\n\
       \t\tgetsuppliers(uuid) from requestor to authorisersvc;\n\
       \t\t//1() from authorisersvc to contractsvc;  // FIXME: big state \
       explosion?\n\
       \t\t//do SuppInfo(requestor, authorisersvc, filtersvc, \
       suppliersvc);  // FIXME: non-WF subprotocol\n\
       \t\tchoice at authorisersvc\n\
       \t\t{\n\
       \t\t\t// DENIED\n\
       \t\t\tdeny() from authorisersvc to requestor;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\tgetsuppliers() from authorisersvc to suppliersvc;\n\
       \t\t\tdo FilterInfo\n\
       \t\t\t\t\t<filterSupplier(usercontext, filters, supplierdetails)>\n\
       \t\t\t\t\t(authorisersvc, filtersvc);\n\
       \t\t\tsuppliers() from suppliersvc to authorisersvc;\n\
       \t\t\tsuppliers() from authorisersvc to requestor;\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t// GET CONTRACT INFO\n\
       \t\tgetcontracts() from requestor to authorisersvc;\n\
       \t\t//2() from authorisersvc to suppliersvc;\n\
       \t\tchoice at authorisersvc\n\
       \t\t{\n\
       \t\t\t// DENIED\n\
       \t\t\tdeny() from authorisersvc to requestor;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\tgetcontracts() from authorisersvc to contractsvc;\n\
       \t\t\tsuppliers() from contractsvc to authorisersvc;\n\
       \t\t\tdo FilterInfo\n\
       \t\t\t\t\t<filterContract(usercontext, filters, contractdetails)>\n\
       \t\t\t\t\t//<filterSupplier(usercontext, filters, supplierdetails)>  \
       // OK (merge)\n\
       \t\t\t\t\t//<filterSupplier(usercontext, filters, contractdetails)>  \
       // Error\n\
       \t\t\t\t\t(authorisersvc, filtersvc);\n\
       \t\t\tcontracts() from authorisersvc to requestor;\n\
       \t\t}\n\
       \t}\n\n\
       \tdo Main(requestor, authorisersvc, filtersvc, suppliersvc, \
       contractsvc);\n\
       }\n\n\n\
       //aux global protocol FilterInfo<type details>(  // Bad non-det. \
       payload\n\
       aux global protocol FilterInfo\n\
       <\n\
       \tsig Query\n\
       >\n\
       (\n\
       \trole authorisersvc,\n\
       \trole filtersvc\n\
       )\n\
       {\n\
       \t//filter(usercontext, filters, details) from authorisersvc to \
       filtersvc;\n\
       \tQuery from authorisersvc to filtersvc;\n\
       \tfiltered() from filtersvc to authorisersvc;\n\
       }\n\n\n\
       // FIXME: don't want WF validation on this (sub)protocol by itself\n\
       /*\n\
       global protocol SuppInfo(\n\
       \t\trole requestor,\n\
       \t\trole authorisersvc,\n\
       \t\trole filtersvc,\n\
       \t\trole suppliersvc)\n\
       {\n\
       \t\tchoice at authorisersvc\n\
       \t\t{\n\
       \t\t\t// DENIED\n\
       \t\t\tdeny() from authorisersvc to requestor;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\tgetsuppliers() from authorisersvc to suppliersvc;\n\
       \t\t\tsuppliers() from suppliersvc to authorisersvc;\n\n\
       \t\t\tfilter(usercontext, filters, supplierdetails) from \
       authorisersvc to filtersvc;\n\
       \t\t\tfiltered() from filtersvc to authorisersvc;\n\
       \t\t\tsuppliers() from authorisersvc to requestor;\n\
       \t\t}\n\
       }\n\
       //*/\n\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfoExplicit.scr"
    , "//$ bin/scribblec.sh \
       scribble-core/src/test/scrib/demo/supplierinfo/SupplierInfoExplicit.scr \
       -fsm InfoAuth Client\n\n\n\
       module src.SupplierInfoExplicit;\n\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <xsd> \"...\" from \"...xsd\" as usercontext;\n\
       type <xsd> \"...\" from \"...xsd\" as filters;\n\n\n\
       explicit global protocol InfoAuth\n\
       (\n\
       \trole Client,\n\
       \trole LoginSvc,\n\
       \trole AuthSvc,\n\
       \trole FilterSvc,\n\
       \trole SupplierSvc,\n\
       \trole ContractSvc\n\
       )\n\
       {\n\
       \tconnect Client to LoginSvc;\n\
       \tlogin(username, password) from Client to LoginSvc;\n\
       \tchoice at LoginSvc\n\
       \t{\n\
       \t\t//loginsuccess() from LoginSvc to Client;  // Not an error if \
       the following line commented (and fairness assumed)\n\
       \t\tloginfailure() from LoginSvc to Client;\n\
       \t\t/*connect Client to AuthSvc;\n\
       \t\tgetsuppliers(uuid) from Client to AuthSvc;  // Error: orphans*/\n\
       \t\tdisconnect Client and LoginSvc;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tloginsuccess() from LoginSvc to Client;\n\
       \t\t//disconnect Client and LoginSvc;\n\
       \t\tconnect Client to AuthSvc;\n\
       \t\tconnect AuthSvc to FilterSvc;\n\
       \t\tconnect AuthSvc to SupplierSvc;\n\
       \t\tconnect AuthSvc to ContractSvc;\n\
       \t\tdo Main(Client, AuthSvc, FilterSvc, SupplierSvc, ContractSvc);\n\
       \t}\n\
       }\n\n\n\
       aux global protocol Main\n\
       (\n\
       \trole Client,\n\
       \trole AuthSvc,\n\
       \trole FilterSvc,\n\
       \trole SupplierSvc,\n\
       \trole ContractSvc\n\
       )\n\
       {\n\
       \tchoice at Client\n\
       \t{\n\
       \t\t// GET SUPPLIER INFO\n\
       \t\tgetsuppliers(uuid) from Client to AuthSvc;\n\
       \t\t//1() from AuthSvc to ContractSvc;\n\
       \t\tdo SuppInfo(Client, AuthSvc, FilterSvc, SupplierSvc);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t// GET CONTRACT INFO\n\
       \t\tgetcontracts() from Client to AuthSvc;\n\
       \t\t//2() from AuthSvc to SupplierSvc;\n\
       \t\tdo ContractInfo(Client, AuthSvc, FilterSvc, ContractSvc);\n\
       \t\t//do ContractInfo(Client, AuthSvc, FilterSvc, SupplierSvc);  // \
       Error\n\
       \t}\n\
       \tdo Main(Client, AuthSvc, FilterSvc, SupplierSvc, ContractSvc);\n\
       }\n\n\n\
       aux global protocol SuppInfo\n\
       (\n\
       \trole Client,\n\
       \trole AuthSvc,\n\
       \trole FilterSvc,\n\
       \trole SupplierSvc\n\
       )\n\
       {\n\
       \tchoice at AuthSvc\n\
       \t{\n\
       \t\t// DENIED\n\
       \t\tdeny() from AuthSvc to Client;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t// PREPARE FILTERED SUPPLIER INFO FOR Client\n\
       \t\tgetsuppliers() from AuthSvc to SupplierSvc;\n\
       \t\tsuppliers() from SupplierSvc to AuthSvc;\n\
       \t\tdo FilterInfo\n\
       \t\t\t\t<filterSupplier(usercontext, filters, supplierdetails)>\n\
       \t\t\t\t(AuthSvc, FilterSvc);\n\
       \t\tsuppliers() from AuthSvc to Client;\n\
       \t}\n\
       }\n\n\n\
       aux global protocol ContractInfo\n\
       (\n\
       \trole Client,\n\
       \trole AuthSvc,\n\
       \trole FilterSvc,\n\
       \trole ContractSvc\n\
       )\n\
       {\n\
       \tchoice at AuthSvc\n\
       \t{\n\
       \t\t// DENIED\n\
       \t\tdeny() from AuthSvc to Client;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t// PREPARE FILTERED SUPPLIER INFO FOR Client\n\
       \t\tgetcontracts() from AuthSvc to ContractSvc;\n\
       \t\tcontracts() from ContractSvc to AuthSvc;\n\
       \t\tdo FilterInfo\n\
       \t\t\t\t<filterContract(usercontext, filters, contractdetails)>\n\
       \t\t\t\t//<filterSupplier(usercontext, filters, supplierdetails)>  \
       // OK (merge)\n\
       \t\t\t\t//<filterSupplier(usercontext, filters, contractdetails)>  \
       // Error\n\
       \t\t\t\t(AuthSvc, FilterSvc);\n\
       \t\tcontracts() from AuthSvc to Client;\n\
       \t}\n\
       }\n\n\n\
       //aux global protocol FilterInfo<type details>(  // Bad non-det. \
       payload\n\
       aux global protocol FilterInfo\n\
       <\n\
       \tsig Query\n\
       >\n\
       (\n\
       \trole AuthSvc,\n\
       \trole FilterSvc\n\
       )\n\
       {\n\
       \t//filter(usercontext, filters, details) from AuthSvc to FilterSvc;\n\
       \tQuery from AuthSvc to FilterSvc;\n\
       \tfiltered() from FilterSvc to AuthSvc;\n\
       }\n\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfoNoFair.scr"
    , "//$ bin/scribblec.sh \
       scribble-core/src/test/scrib/demo/supplierinfo/SupplierInfoNoFair.scr \
       -fsm InfoAuth Client\n\n\n\
       module src.SupplierInfoNoFair;\n\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <xsd> \"...\" from \"...xsd\" as usercontext;\n\
       type <xsd> \"...\" from \"...xsd\" as filters;\n\n\n\
       explicit global protocol InfoAuth\n\
       (\n\
       \trole Client,\n\
       \trole LoginSvc,\n\
       \trole AuthSvc,\n\
       \trole FilterSvc,\n\
       \trole SupplierSvc,\n\
       \trole ContractSvc\n\
       )\n\
       {\n\
       \tconnect Client to LoginSvc;\n\
       \tlogin(username, password) from Client to LoginSvc;\n\
       \tchoice at LoginSvc\n\
       \t{\n\
       \t\tloginfailure() from LoginSvc to Client;\n\
       \t\t//disconnect Client and LoginSvc;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tloginsuccess() from LoginSvc to Client;\n\
       \t\t//disconnect Client and LoginSvc;\n\
       \t\tconnect Client to AuthSvc;\n\
       \t\tdo Main(Client, AuthSvc, FilterSvc, SupplierSvc, ContractSvc);\n\
       \t}\n\
       }\n\n\n\
       aux global protocol Main\n\
       (\n\
       \trole Client,\n\
       \trole AuthSvc,\n\
       \trole FilterSvc,\n\
       \trole SupplierSvc,\n\
       \trole ContractSvc\n\
       )\n\
       {\n\
       \tchoice at Client\n\
       \t{\n\
       \t\t// GET SUPPLIER INFO\n\
       \t\tgetsuppliers(uuid) from Client to AuthSvc;\n\
       \t\tdo SuppInfo(Client, AuthSvc, FilterSvc, SupplierSvc);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t// GET CONTRACT INFO\n\
       \t\tgetcontracts() from Client to AuthSvc;\n\
       \t\tdo ContractInfo(Client, AuthSvc, FilterSvc, ContractSvc);\n\
       \t}\n\
       \tdo Main(Client, AuthSvc, FilterSvc, SupplierSvc, ContractSvc);\n\
       }\n\n\n\
       aux global protocol SuppInfo\n\
       (\n\
       \trole Client,\n\
       \trole AuthSvc,\n\
       \trole FilterSvc,\n\
       \trole SupplierSvc\n\
       )\n\
       {\n\
       \tchoice at AuthSvc\n\
       \t{\n\
       \t\t// DENIED\n\
       \t\tdeny() from AuthSvc to Client;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tconnect AuthSvc to SupplierSvc;\n\
       \t\t// PREPARE FILTERED SUPPLIER INFO FOR Client\n\
       \t\tgetsuppliers() from AuthSvc to SupplierSvc;\n\
       \t\tsuppliers() from SupplierSvc to AuthSvc;\n\
       \t\t//disconnect AuthSvc and SupplierSvc;\n\
       \t\tdo FilterInfo\n\
       \t\t\t\t<filterSuppliers(usercontext, filters, supplierdetails)>\n\
       \t\t\t\t//<filterContracts(usercontext, filters, supplierdetails)>\n\
       \t\t\t\t(AuthSvc, FilterSvc);\n\
       \t\tdisconnect AuthSvc and SupplierSvc;\n\
       \t\tsuppliers() from AuthSvc to Client;\n\
       \t}\n\
       }\n\n\n\
       aux global protocol ContractInfo\n\
       (\n\
       \trole Client,\n\
       \trole AuthSvc,\n\
       \trole FilterSvc,\n\
       \trole ContractSvc\n\
       )\n\
       {\n\
       \tchoice at AuthSvc\n\
       \t{\n\
       \t\t// DENIED\n\
       \t\tdeny() from AuthSvc to Client;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tconnect AuthSvc to ContractSvc;\n\
       \t\t// PREPARE FILTERED SUPPLIER INFO FOR Client\n\
       \t\tgetcontracts() from AuthSvc to ContractSvc;\n\
       \t\tcontracts() from ContractSvc to AuthSvc;\n\
       \t\t//disconnect AuthSvc and ContractSvc;\n\
       \t\tdo FilterInfo\n\
       \t\t\t\t<filterContracts(usercontext, filters, contractdetails)>\n\
       \t\t\t\t(AuthSvc, FilterSvc);\n\
       \t\tdisconnect AuthSvc and ContractSvc;\n\
       \t\tcontracts() from AuthSvc to Client;\n\
       \t}\n\
       }\n\n\n\
       aux global protocol FilterInfo\n\
       <\n\
       \tsig Query\n\
       >\n\
       (\n\
       \trole AuthSvc,\n\
       \trole FilterSvc\n\
       )\n\
       {\n\
       \tQuery connect AuthSvc to FilterSvc;\n\
       \tfiltered() from FilterSvc to AuthSvc;\n\
       \tdisconnect AuthSvc and FilterSvc;\n\
       }\n\n" )
  ; ( "from-scribble-java/demo/supplierinfo/SupplierInfoDemo.scr"
    , "module src.SupplierInfoDemo;\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       //type <xsd> \"UserContext\" from \"AUTH.xsd\" as usercontext;\n\
       //type <xsd> \"FilterSet\" from \"AUTH.xsd\" as filters;\n\n\n\
       global protocol Partners(\n\
       \t\trole LOGINsvc,\n\
       \t\trole REQUESTOR,\n\
       \t\trole AUTHsvc,\n\
       \t\trole usersvc,\n\
       \t\trole FILTERsvc,\n\
       \t\trole SUPPLIERsvc,\n\
       \t\trole CONTRACTsvc)\n\
       {\n\
       \tlogin(username, password) from REQUESTOR to LOGINsvc;\n\
       \tchoice at LOGINsvc\n\
       \t{\n\
       \t\tloginfailure() from LOGINsvc to REQUESTOR;\n\
       \t} or {\n\
       \t\tloginsuccess() from LOGINsvc to REQUESTOR;\n\n\
       \t\trec MAIN\n\
       \t\t{\n\
       \t\t\tchoice at REQUESTOR\n\
       \t\t\t{\n\
       \t\t\t\t// GET SUPPLIER INFO\n\
       \t\t\t\tgetsuppliers(uuid) from REQUESTOR to AUTHsvc;\n\
       \t\t\t\tchoice at AUTHsvc\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\t\t\tgetsuppliers() from AUTHsvc to SUPPLIERsvc;\n\
       \t\t\t\t\tsuppliers() from SUPPLIERsvc to AUTHsvc;\n\
       \t\t\t\t\tfilterSuppliers(usercontext, filters, supplierdetails) \
       from AUTHsvc to FILTERsvc;\n\
       \t\t\t\t\tfiltered() from FILTERsvc to AUTHsvc;\n\
       \t\t\t\t\tsuppliers() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t}\n\
       \t\t\t\tcontinue MAIN;\n\
       \t\t\t} or {\n\
       \t\t\t\t// GET CONTRACT INFO\n\
       \t\t\t\tgetcontracts() from REQUESTOR to AUTHsvc;\n\
       \t\t\t\tchoice at authoriser\n\
       \t\t\t\t{\n\
       \t\t\t\t\t// DENIED\n\
       \t\t\t\t\tdeny() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\t// PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR\n\
       \t\t\t\t\tgetcontracts() from AUTHsvc to CONTRACTsvc;\n\
       \t\t\t\t\tcontracts() from CONTRACTsvc to AUTHsvc;\n\
       \t\t\t\t\tfilterContracts(usercontext, filters, contractdetails) \
       from AUTHsvc to FILTERsvc;\n\
       \t\t\t\t\tfiltered() from FILTERsvc to AUTHsvc;\n\
       \t\t\t\t\tcontracts() from AUTHsvc to REQUESTOR;\n\
       \t\t\t\t}\n\
       \t\t\t\tcontinue MAIN;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\n" )
  ; ( "from-scribble-java/scratch/scratch1/Scratch1.scr"
    , "//http://sandbox.kidstrythisathome.com/erdos/\n\n\
       //$ bin/scribblec.sh \
       modules/core/src/test/scrib/scratch/scratch1/Scratch1.scr -ip \
       modules/core/src/test/scrib/ -d modules/core/src/test/scrib/ -api \
       Proto1 C -subtypes\n\
       //$ javadoc -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar;'modules/core/src/test/scrib \
       modules/core/src/test/scrib/scratch/scratch1/Proto1*.java -d \
       test/doc/scratch/scratch1\n\n\
       module scratch.scratch1.Scratch1;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\n\
       global protocol Proto1(role C, role S)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from C to S;\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\t2(Int) from C to S;\n\
       \t\t\t3(Int) from S to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4a() from C to S;\n\
       \t\t\t4b() from C to S;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4a() from C to S;  // Currently needs -minfsm\n\
       \t\t\t4b() from C to S;\n\
       \t\t}\n\
       \t}\n\
       }\n\n\
       global protocol Proto2(role C, role S)\n\
       {\n\
       \t1() from C to S;\n\
       \t//2() from C to S;\n\
       \tchoice at C \n\
       \t{\n\
       \t\t3() from C to S;\n\
       \t\tdo Proto2(C, S);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2() from C to S;\n\
       \t}\n\
       }\n\n\
       global protocol Proto3(role C, role S)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from C to S;\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\t2(Int) from C to S;\n\
       \t\t\t3(Int) from S to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from C to S;\n\
       \t\t}\n\
       \t}\n\
       }" )
  ; ( "from-scribble-java/exercise/calculator/EProtocol.scr"
    , "//$ bin/scribblec.sh -d modules/core/src/test/scrib/ \
       modules/core/src/test/scrib/exercise/calculator/EProtocol.scr -api \
       Calc C\n\n\
       module exercise.calculator.EProtocol;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as string;\n\n\n\
       global protocol Calc(role S , role C ) {\n\
       \trec Loop {\n\
       \t\tchoice at C {\n\
       \t\t\tsum (int, int) from C to S ;\n\
       \t\t\tresult (int) from S to C ;\n\
       \t\t\tcontinue Loop ;\n\
       \t\t} or {\n\
       \t\t\tmultiply (int, int) from C to S ;\n\
       \t\t\tresult (int) from S to C ;\n\
       \t\t\tcontinue Loop ;\n\
       \t\t} or {\n\
       \t\t\tquit () from C to S ;\n\
       \t\t\tterminate () from S to C ;\n\
       \t\t}\n\
       \t}\n\
       }\n\n" )
  ; ( "from-scribble-java/exercise/voting/EProtocol.scr"
    , "//$ bin/scribblec.sh -d modules/core/src/test/scrib/ \
       modules/core/src/test/scrib/exercise/voting/EProtocol.scr -api \
       EVoting C\n\n\
       module exercise.voting.EProtocol;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\n\n\
       global protocol EVoting(role V, role S){\n\
       \tAuthenticate(String ) from V to S;\n\
       \tchoice at S {\n\
      \   \tOk(String) from S to V;\n\n\
      \   \tchoice at V {\n\
      \     \tYes(String) from V to S;\n\
      \   \t} or {\n\
       \t\t\tNo(String) from V to S;\n\
      \    }\n\
      \    Result(Int) from S to V;\n\
       \t} or {\n\
      \   \tReject(String ) from S to V;\n\
       \t}\n\
       }\n\n" )
  ; ( "from-scribble-java/test/test3/Test3.scr"
    , "//$ bin/scribblec.sh \
       scribble-test/src/test/scrib/test/test3/Test3.scr -d \
       scribble-test/src/test/scrib/ -cbapi Proto1 A -cbapi Proto1 B -cbapi \
       Proto1 C\n\
       //$ javadoc -cp \
       scribble-cli/target/classes/';'scribble-test/target/classes';'scribble-parser/target/classes';'scribble-parser/lib/antlr-3.5.2-complete.jar';'scrib-core/src/test/scrib \
       scrib-core/src/test/scrib/test/test3/Proto1*.java -d test/doc/test3\n\n\
       module test.test3.Test3;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\"  from \"rt.jar\" as Str;\n\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1(Int) from A to B;  //@ statename=\"mystatename\"\n\
       \t\t2(Int) from B to C;\n\
       \t\tdo Proto1(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3(Str) from A to B;\n\
       \t\t4(Str) from B to C;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/test/test5/Test5.scr"
    , "//$ bin/scribblec.sh \
       scribble-test/src/test/scrib/test/test5/Test5.scr -d \
       scribble-test/src/test/scrib/ -cbapi Proto1 A -cbapi Proto1 B -cbapi \
       Proto1 C\n\
       //$ javadoc -cp \
       scribble-cli/target/classes/';'scribble-test/target/classes';'scribble-parser/target/classes';'scribble-parser/lib/antlr-3.5.2-complete.jar';'scrib-core/src/test/scrib \
       scrib-core/src/test/scrib/test/test5/Proto1*.java -d test/doc/test5\n\n\
       module test.test5.Test5;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\"  from \"rt.jar\" as Str;\n\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1(Int) from A to B;\n\
       \t\t2(Int) from A to C;\n\
       \t\tdo Proto1(A, B, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t3(Str) from A to C;\n\
       \t\t4(Str) from A to B;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/test/test4/Test4.scr"
    , "//$ bin/scribblec.sh \
       scribble-test/src/test/scrib/test/test4/Test4.scr -d \
       scribble-test/src/test/scrib/ -cbapi Proto1 A -cbapi Proto1 B\n\
       //$ javadoc -cp \
       scribble-cli/target/classes/';'scribble-test/target/classes';'scribble-parser/target/classes';'scribble-parser/lib/antlr-3.5.2-complete.jar';'scrib-core/src/test/scrib \
       scrib-core/src/test/scrib/test/test4/Proto1*.java -d test/doc/test4\n\n\
       module test.test4.Test4;\n\n\
       sig <java> \"test.test4.sig.Foo\" from \"test/test4/sig/Foo.java\" \
       as Foo;\n\
       sig <java> \"test.test4.sig.Bar\" from \"test/test4/sig/Bar.java\" \
       as Bar;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\tFoo from A to B;\n\
       \t\tdo Proto1(A, B);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tBar from A to B;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/test/test8/Test8.scr"
    , "//$ bin/scribblec.sh \
       scribble-test/src/test/scrib/test/test8/Test8.scr -d \
       scribble-test/src/test/scrib/ -cbapi Proto1 A -cbapi Proto1 B -cbapi \
       Proto1 C\n\
       //$ javadoc -cp \
       scribble-cli/target/classes/';'scribble-test/target/classes';'scribble-parser/target/classes';'scribble-parser/lib/antlr-3.5.2-complete.jar';'scrib-core/src/test/scrib \
       scrib-core/src/test/scrib/test/test8/Proto1*.java -d test/doc/test8\n\n\
       module test.test8.Test8;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\"  from \"rt.jar\" as Str;\n\n\
       global protocol Proto1(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1(Int) from A to B;\n\
       \t\t1(Int) from A to B;\n\
       \t\t3() from A to C;\n\
       \t\t//1(Int) from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\t\n\
       \t\t2(Int) from A to C;\n\
       \t\t3(Int) from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t4(Str) from A to C;\n\
       \t\t4(Str) from A to B;\n\
       \t}\t\n\
       }\n" )
  ; ( "from-scribble-java/test/test1/Test1.scr"
    , "//http://sandbox.kidstrythisathome.com/erdos/\n\n\
       //$ bin/scribblec.sh \
       modules/core/src/test/scrib/test/test1/Test1.scr -ip \
       modules/core/src/test/scrib/ -d modules/core/src/test/scrib/ \
       -session Proto1 -api Proto1 C -V\n\
       //$ javadoc -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar;'modules/core/src/test/scrib \
       modules/core/src/test/scrib/test/test1/Proto1*.java -d test/doc/test1\n\n\
       module test.test1.Test1;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Integer;\n\n\
       global protocol Proto1(role C, role S)\n\
       {\n\
       \trec X\n\
       \t{\n\
       \t\t1() from C to S;\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\t2(Integer) from C to S;\n\
       \t\t\t3(Integer) from S to C;\n\
       \t\t\tcontinue X;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\t4() from C to S;\n\
       \t\t}\n\
       \t}\n\
       }\n\n\
       global protocol Proto2(role C, role S)\n\
       {\n\
       \t1() from C to S;\n\
       \t2() from C to S;\n\
       }\n" )
  ; ( "from-scribble-java/test/test6/Test6.scr"
    , "//$ bin/scribblec.sh \
       scribble-test/src/test/scrib/test/test6/Test6.scr -d \
       scribble-test/src/test/scrib/ -cbapi Proto1 A -cbapi Proto1 B\n\
       //$ javadoc -cp \
       scribble-cli/target/classes/';'scribble-test/target/classes';'scribble-parser/target/classes';'scribble-parser/lib/antlr-3.5.2-complete.jar';'scrib-core/src/test/scrib \
       scrib-core/src/test/scrib/test/test6/Proto1*.java -d test/doc/test6\n\n\
       module test.test6.Test6;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\"  from \"rt.jar\" as Str;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \t1(Int) from A to B;\n\
       \t2(Str) from B to A;\n\
       \t1(Int) from A to B;\n\
       \t2(Str) from B to A;\n\
       }\n" )
  ; ( "from-scribble-java/test/foo/Foo.scr"
    , "//Raymond@HZHL3 ~/code/scribble-java/scribble-java\n\
       //$ java -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar' \
       org.scribble.cli.CommandLine -path modules/core/src/test/scrib \
       modules/core/src/test/scrib/test/foo/Foo.scr -session Foo -d \
       modules/core/src/test/scrib\n\
       //$ java -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar' \
       org.scribble.cli.CommandLine -path modules/core/src/test/scrib \
       modules/core/src/test/scrib/test/foo/Foo.scr -api Foo A -d \
       modules/core/src/test/scrib\n\n\
       //$ javadoc -cp \
       modules/cli/target/classes/';'modules/core/target/classes';'modules/trace/target/classes';'modules/parser/target/classes';c:\Users\Raymond\.m2\repository\org\antlr\antlr-runtime\3.2\antlr-runtime-3.2.jar;'modules/validation/target/classes/';'modules/projection/target/classes/';C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-mapper-asl\1.9.9\jackson-mapper-asl-1.9.9.jar;C:\Users\Raymond\.m2\repository\org\codehaus\jackson\jackson-core-asl\1.9.9\jackson-core-asl-1.9.9.jar;'modules/core/src/test/scrib \
       modules/core/src/test/scrib/test/foo/Foo*.java -d test/javadoc/test\n\n\
       //$ bin/scribblec.sh modules/core/src/test/scrib/test/foo/Foo.scr \
       -ip modules/core/src/test/scrib/ -d modules/core/src/test/scrib/ \
       -session Foo -api Foo A -V\n\n\
       module test.foo.Foo;\n\n\n\
       //type <java> \"java.lang.Integer\" from \"rt.jar\" as Integer;\n\n\n\
       global protocol Foo(role A, role B, role C)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1() from A to B;\n\
       \t\t2() from A to C;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t1() from A to C;\n\
       \t\t2() from A to B;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/test/test7/Test7.scr"
    , "//$ bin/scribblec.sh \
       scribble-test/src/test/scrib/test/test7/Test7.scr -d \
       scribble-test/src/test/scrib/ -cbapi Proto1 A -cbapi Proto1 B\n\
       //$ javadoc -cp \
       scribble-cli/target/classes/';'scribble-test/target/classes';'scribble-parser/target/classes';'scribble-parser/lib/antlr-3.5.2-complete.jar';'scrib-core/src/test/scrib \
       scrib-core/src/test/scrib/test/test7/Proto1*.java -d test/doc/test7\n\n\
       module test.test7.Test7;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\"  from \"rt.jar\" as Str;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1(Int) from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2(Str) from A to B;\n\
       \t}\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1(Int) from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2(Str) from A to B;\n\
       \t}\n\
       }\n" )
  ; ( "from-scribble-java/test/test2/Test2.scr"
    , "//$ bin/scribblec.sh \
       scribble-test/src/test/scrib/test/test2/Test2.scr -d \
       scribble-test/src/test/scrib/ -cbapi Proto1 A -cbapi Proto1 B\n\
       //$ javadoc -cp \
       scribble-cli/target/classes/';'scribble-test/target/classes';'scribble-parser/target/classes';'scribble-parser/lib/antlr-3.5.2-complete.jar';'scrib-core/src/test/scrib \
       scrib-core/src/test/scrib/test/test2/Proto1*.java -d test/doc/test2\n\n\
       module test.test2.Test2;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\"  from \"rt.jar\" as Str;\n\n\
       global protocol Proto1(role A, role B)\n\
       {\n\
       \tchoice at A\n\
       \t{\n\
       \t\t1(Int) from A to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t2(Str) from A to B;\n\
       \t}\n\
       }\n" )
  ; ( "annot/test100.scr"
    , "module test100; \n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol Test100(role S, role C)\n\
       {\n\
       hello(x1:int) from C to S;\n\
       hello(x2:int) from S to C; @\"x2 > x1\"\n\
       hello(x3:int) from C to S;\n\
       hello(x4:int) from S to C; @\"x4 > x3\"\n\
       hello(x5:int) from C to S;\n\
       hello(x6:int) from S to C; @\"x6 > x5\"\n\
       hello(x7:int) from C to S;\n\
       hello(x8:int) from S to C; @\"x8 > x7\"\n\
       hello(x9:int) from C to S;\n\
       hello(x10:int) from S to C; @\"x10 > x9\"\n\
       hello(x11:int) from C to S;\n\
       hello(x12:int) from S to C; @\"x12 > x11\"\n\
       hello(x13:int) from C to S;\n\
       hello(x14:int) from S to C; @\"x14 > x13\"\n\
       hello(x15:int) from C to S;\n\
       hello(x16:int) from S to C; @\"x16 > x15\"\n\
       hello(x17:int) from C to S;\n\
       hello(x18:int) from S to C; @\"x18 > x17\"\n\
       hello(x19:int) from C to S;\n\
       hello(x20:int) from S to C; @\"x20 > x19\"\n\
       hello(x21:int) from C to S;\n\
       hello(x22:int) from S to C; @\"x22 > x21\"\n\
       hello(x23:int) from C to S;\n\
       hello(x24:int) from S to C; @\"x24 > x23\"\n\
       hello(x25:int) from C to S;\n\
       hello(x26:int) from S to C; @\"x26 > x25\"\n\
       hello(x27:int) from C to S;\n\
       hello(x28:int) from S to C; @\"x28 > x27\"\n\
       hello(x29:int) from C to S;\n\
       hello(x30:int) from S to C; @\"x30 > x29\"\n\
       hello(x31:int) from C to S;\n\
       hello(x32:int) from S to C; @\"x32 > x30\"\n\
       hello(x33:int) from C to S;\n\
       hello(x34:int) from S to C; @\"x34 > x33\"\n\
       hello(x35:int) from C to S;\n\
       hello(x36:int) from S to C; @\"x36 > x35\"\n\
       hello(x37:int) from C to S;\n\
       hello(x38:int) from S to C; @\"x38 > x37\"\n\
       hello(x39:int) from C to S;\n\
       hello(x40:int) from S to C; @\"x40 > x39\"\n\
       hello(x41:int) from C to S;\n\
       hello(x42:int) from S to C; @\"x42 > x41\"\n\
       hello(x43:int) from C to S;\n\
       hello(x44:int) from S to C; @\"x44 > x43\"\n\
       hello(x45:int) from C to S;\n\
       hello(x46:int) from S to C; @\"x46 > x45\"\n\
       hello(x47:int) from C to S;\n\
       hello(x48:int) from S to C; @\"x48 > x47\"\n\
       hello(x49:int) from C to S;\n\
       hello(x50:int) from S to C; @\"x50 > x49\"\n\
       }" )
  ; ( "annot/Smtp.scr"
    , "//$ ./scribblec.sh -ip scribble-demos/scrib/smtp/src -d \
       scribble-demos/scrib/smtp/src \
       scribble-demos/scrib/smtp/src/smtp/Smtp.scr -subtypes -api Smtp C\n\
       //$ javadoc -cp \
       scribble-core/target/classes:scribble-runtime/target/classes:scribble-demos/scrib/smtp/src/ \
       scribble-demos/scrib/smtp/src/smtp/*.java -subpackages \
       smtp.Smtp.Smtp -d scribble-demos/scrib/smtp/javadoc\n\n\n\n\
       module icse18.Smtp;\n\n\n\
       type <dotnet> \"System.UInt32\" from \"...\" as string;\n\n\n\
       global protocol Smtp(role S, role C)\n\
       {\n\
       \t220() from S to C;\n\
       \tdo Ehlo(S, C);\n\
       }\n\n\
       aux global protocol Ehlo(role S, role C)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tEhlo1() from C to S; \n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at S\n\
       \t\t\t{\n\
       \t\t\t\t250d() from S to C;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t250() from S to C;\n\
       \t\t\t\tdo StartTls(S, C);\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tQuit1() from C to S;\n\
       \t}\n\
       }\n\n\
       aux global protocol StartTls(role S, role C)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tStartTls() from C to S;\n\
       \t\t220() from S to C;\n\
       \t\t// Do TLS handshake here: level below the application level \
       protocol (like regular TCP handshake)\n\
       \t\tdo SecureEhlo(S, C);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tQuit2() from C to S;\n\
       \t}\n\
       }\n\n\
       aux global protocol SecureEhlo(role S, role C)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tEhlo2() from C to S;\n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at S\n\
       \t\t\t{\n\
       \t\t\t\t250d1() from S to C;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t2501() from S to C;\n\
       \t\t\t\tdo Auth(S, C);\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tQuit3() from C to S;\n\
       \t}\n\
       }\n\n\
       aux global protocol Auth(role S, role C)\n\
       {\n\
       \trec Y\n\
       \t{\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\tAuth() from C to S;\n\
       \t\t\tchoice at S\n\
       \t\t\t{\n\
       \t\t\t\t235() from S to C;\n\
       \t\t\t\tdo Mail(S, C);\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t535() from S to C;\n\
       \t\t\t\tcontinue Y;\n\
       \t\t\t}\n\
       \t\t\t//.. 501 Invalid base64 Data \n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tQuit4() from C to S;\n\
       \t\t}\n\
       \t}\n\
       }\n\n\
       aux global protocol Mail(role S, role C)\n\
       {\n\
       \trec Z1\n\
       \t{\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\tMail() from C to S; //Mail from:<a@b.com>\n\
       \t\t\tchoice at S\n\
       \t\t\t{\n\
       \t\t\t\t501() from S to C;\n\
       \t\t\t\tcontinue Z1;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t2502() from S to C;\n\
       \t\t\t\t\n\
       \t\t\t\trec Z2\n\
       \t\t\t\t{\n\
       \t\t\t\t\tchoice at C\n\
       \t\t\t\t\t{\n\
       \t\t\t\t\t\tRcpt() from C to S; //Rcpt to:<c@d.com>\n\
       \t\t\t\t\t\tchoice at S\n\
       \t\t\t\t\t\t{\n\
       \t\t\t\t\t\t\t2503() from S to C;\n\
       \t\t\t\t\t\t\tcontinue Z2;\n\
       \t\t\t\t\t\t}\n\
       \t\t\t\t\t}\n\
       \t\t\t\t\tor\n\
       \t\t\t\t\t{\n\
       \t\t\t\t\t\tData() from C to S;\n\
       \t\t\t\t\t\t354() from S to C;\n\
       \t\t\t\t\t\t//too from C to S; //to:<you>\n\
       \t\t\t\t\t\t//froom from C to S; //from:<me>\n\
       \t\t\t\t\t\trec Z3\n\
       \t\t\t\t\t\t{\n\
       \t\t\t\t\t\t\tchoice at C\n\
       \t\t\t\t\t\t\t{\n\
       \t\t\t\t\t\t\t\tDataLine() from C to S;\n\
       \t\t\t\t\t\t\t\tDataLine() from C to S;\n\
       \t\t\t\t\t\t\t\tcontinue Z3;\n\
       \t\t\t\t\t\t\t}\n\
       \t\t\t\t\t\t\tor\n\
       \t\t\t\t\t\t\t{\n\
       \t\t\t\t\t\t\t\tSubject() from C to S; //Subject:<my Subject>\n\
       \t\t\t\t\t\t\t\tSubject() from C to S; //Subject:<my Subject>\n\
       \t\t\t\t\t\t\t\tcontinue Z3;\n\
       \t\t\t\t\t\t\t}\n\
       \t\t\t\t\t\t\tor\n\
       \t\t\t\t\t\t\t{\n\
       \t\t\t\t\t\t\t\tEndOfData() from C to S; // CRLF.CRLF\n\
       \t\t\t\t\t\t\t\t2504() from S to C;\n\
       \t\t\t\t\t\t\t\tcontinue Z1;\n\
       \t\t\t\t\t\t\t}\n\
       \t\t\t\t\t\t}\t\n\
       \t\t\t\t\t}\n\
       \t\t\t\t}\n\
       \t\t\t}\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tQuit5() from C to S;\n\
       \t\t\t221() from S to C;\n\
       \t\t}\n\
       \t}\n\
       }" )
  ; ( "annot/Mcrsrv.scr"
    , "module src.Mcrsrv;\n\n\
       type <xsd> \"UserName\" from \"AUTH.xsd\" as username;\n\
       type <xsd> \"Password\" from \"AUTH.xsd\" as password;\n\
       type <xsd> \"UUID\" from \"AUTH.xsd\" as uuid;\n\
       type <xsd> \"/retailhub/supplierdetails\" from \"Retailer.xsd\" as \
       supplierdetails;\n\
       type <xsd> \"/retailhub/contractdetails\" from \"Retailer.xsd\" as \
       contractdetails;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\n\
       global protocol PartnershipSupplier(\trole loginsvc,\n\
       \t\t\t\t\t\t\t\t\t\trole requestor,\n\
       \t\t\t\t\t\t\t\t\t\trole authorisersvc,\n\
       \t\t\t\t\t\t\t\t\t\trole filtersvc,\n\
       \t\t\t\t\t\t\t\t\t\trole suppliersvc,\n\
       \t\t\t\t\t\t\t\t\t\trole contractsvc)\n\
       {\n\
       \tlogin(int, int) from requestor to loginsvc;\n\
       \tchoice at loginsvc\n\
       \t{\n\
       \t\tloginfailure() from loginsvc to requestor;\n\
       \t} or {\n\
       \t\tloginsuccess() from loginsvc to requestor;\n\
       \t\trec mainBlock\n\
       \t\t{\n\
       \t\t\tchoice at requestor\n\
       \t\t\t{\t\t\t\t\t// GET SUPPLIER INFO\n\
       \t\t\t\tgetsuppliers(int) from requestor to authorisersvc;\n\
       \t\t\t\tchoice at authorisersvc\n\
       \t\t\t\t{\t\t\t\t\n\
       \t\t\t\t\tdeny() from authorisersvc to requestor;\n\
       \t\t\t\t} or {\t\t\t\n\
       \t\t\t\t\tgetsuppliers() from authorisersvc to suppliersvc;\n\
       \t\t\t\t\tsuppliers() from suppliersvc to authorisersvc;\n\
       \t\t\t\t\tfilter(int, int, int) from authorisersvc to filtersvc;\n\
       \t\t\t\t\tfiltered() from filtersvc to authorisersvc;\n\
       \t\t\t\t\tsuppliers() from authorisersvc to requestor;\n\
       \t\t\t\t}\n\
       \t\t\t\tcontinue mainBlock;\n\
       \t\t\t} or {\t\t\t\t\n\
       \t\t\t\tgetcontracts() from requestor to authorisersvc;\n\
       \t\t\t\tchoice at authorisersvc\n\
       \t\t\t\t{\t\t\t\t\n\
       \t\t\t\t\tdeny() from authorisersvc to requestor;\n\
       \t\t\t\t} or {\n\
       \t\t\t\t\tgetcontracts() from authorisersvc to contractsvc;\n\
       \t\t\t\t\tsuppliers() from contractsvc to authorisersvc;\n\
       \t\t\t\t\tfilter(int, int, int) from authorisersvc to filtersvc;\n\
       \t\t\t\t\tfiltered() from filtersvc to authorisersvc;\n\
       \t\t\t\t\tcontracts() from authorisersvc to requestor;\n\
       \t\t\t\t}\n\
       \t\t\t\tcontinue mainBlock;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\n" )
  ; ( "annot/Fib.scr"
    , "module Fib;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\n\
       global protocol Adder(role C, role S)\n\
       {\n\
       \tHELLO(u:int) from C to S; @\"u < 3\"\n\n\
       \tchoice at C\n\
       \t{\n\
       \t\tADD(y:int) from C to S;\n\
       \t\tRES(v:int) from S to C; @\"v > 0\"\n\
       \t\tdo Adder(C, S);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tBYE() from C to S;\n\
       \t\tBYE() from S to C;\n\
       \t}\n\
       }\n" )
  ; ( "annot/Nego1.scr"
    , "//$ ./scribblec.sh -ip scribble-demos/scrib/nego/src -d \
       scribble-demos/scrib/nego/src \
       scribble-demos/scrib/nego/src/nego/Nego1.scr\n\n\
       module nego.Nego1;\n\n\
       type <java> \"test.nego.SAP\" from \"test/nego/...\" as int;\n\
       type <java> \"test.nego.SAP\" from \"test/nego/SAP.java\" as SAP;\n\n\
       // C = Consumer, P = Producer\n\
       global protocol Negotiation(role C, role P)\n\
       {\n\
       \tpropose(int) from C to P;\n\
       \trec X\n\
       \t{\n\
       \t\tHELLO() from P to C;\n\
       \t\tchoice at P\n\
       \t\t{\n\
       \t\t\taccpt() from P to C;\n\
       \t\t\tconfirm() from C to P;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\treject() from P to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tpropose(int) from P to C;\n\
       \t\t\tchoice at C\n\
       \t\t\t{\n\
       \t\t\t\taccpt() from C to P;\n\
       \t\t\t\tconfirm() from P to C;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\treject() from C to P;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tpropose(int) from C to P;\n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\n" )
  ; ( "annot/test100NoAss.scr"
    , "module test100NoAss; \n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\
      \ global protocol Test100(role S, role C)\n\
       {\n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       rec Loop {\n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       hello(int) from C to S;\n\
       hello(int) from S to C; \n\
       continue Loop; \n\
       }\n\
       }\n" )
  ; ( "annot/LoanApplication.scr"
    , "module LoanApplication;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as Int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\
       type <java> \"java.lang.Boolean\" from \"rt.jar\" as Bool;\n\
      \ \n\
       explicit global protocol BuyerBrokerSupplier(role Applicant, role \
       ApplicationPortal, role ProcessingDept)\n\
       {\n\
       \tconnect Applicant to ApplicationPortal;\n\
       \tconnect ApplicationPortal to ProcessingDept;\n\n\
       \tapplyForLoan(String, String, Int, Int) from Applicant to \
       ApplicationPortal;\n\
       \tcheckEligibility(Int, Int) from ApplicationPortal to ProcessingDept;\n\
       \trespond(Int) from ProcessingDept to ApplicationPortal;\n\n\
       \t/*choice at ApplicationPortal\n\
       \t{ \n\
       \t\tgetLoanAmount(Int) from ApplicationPortal to FinanceDept;\n\
       \t\tsendLoanAmount(Int) from FinanceDept to ApplicationPortal;\n\
       \t\trequestConfirmation(Int) from ApplicationPortal to \
       Applicant;        \n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\treject() from ApplicationPortal to FinanceDept;\n\
       \t\treject() from ApplicationPortal to Applicant;\n\
       \t}*/\n\n\
       }" )
  ; ( "annot/AnotherHttp.scr"
    , "module demo;\n\n\
       type <fsharp> \"System.String\" from \"fs.dll\" as string; \n\
       type <fsharp> \"System.Int32\" from \"fs.dll\" as int;\n\
       type <fsharp> \"System.DateTime\" from \"fs.dll\" as date;\n\n\
       global protocol AnotherHttp(role C, role S)\n\
       {\n\
      \  do Request(C, S);\n\
      \  do Response(C, S);\n\
       }\n\n\
       global protocol Request(role C, role S)\n\
       {\n\
      \  choice at C {\n\
      \    GET(string) from C to S; \n\
      \  } or {\n\
      \    POST(string) from C to S;\n\
      \  }\n\n\
      \  rec X\n\
      \  {\n\
      \      choice at C\n\
      \      {\n\
      \        HOST(string) from C to S; \n\
      \        continue X;\n\
      \      }\n\
      \      or\n\
      \      {\n\
      \        BODY(string) from C to S;\n\
      \      }\n\
      \    }\n\
       }\n\n\
       global protocol Response(role C, role S)\n\
       {\n\
      \  HTTP(string) from S to C; \n\
      \  choice at S\n\
      \  {\n\
      \    d200(string) from S to C; \n\
      \  }\n\
      \  or\n\
      \  {\n\
      \    d404(string) from S to C; \n\
      \  }\n\n\
      \  rec Y\n\
      \  {\n\
      \    choice at S\n\
      \    {\n\
      \      ContentLength(int) from S to C;\n\
      \      continue Y;\n\
      \    }\n\
      \    or\n\
      \    {\n\
      \      CONTENTType(string) from S to C;\n\
      \      continue Y;\n\
      \    }\n\
      \    or\n\
      \    {\n\
      \      BODY(string) from S to C;\n\
      \    }\n\
      \  }\n\
       }\n" )
  ; ( "annot/SHNew.scr"
    , "module assrt.sh.SHNew;\n\n\
       type <dotnet> \"System.UInt32\" from \"...\" as int;\n\n\
       global protocol SH(role P, role R)\n\
       {\thello(s:int) from P to R; \n\
       \thello(f:int) from R to P; \n\
       \tplane(x1:int) from P to R;\n\
       \tdo Loop(P, R);\n\
       }\n\n\
       aux global protocol Loop(role P, role R)\n\
       {\n\
       \tchoice at P\n\
       \t{\n\
       \t\tAbove(v1:int) from P to R;   \n\
       \t\tRes(b1:int) from R to P;  \n\
       \t\tAbove(v2:int) from P to R;   \n\
       \t\tRes(b2:int) from R to P; \n\
       \t\tchoice at P\n\
       \t\t{\n\
       \t\t\tBothIn() from P to R; //@\"b1=1 && b2=1\"\n\
       \t\t\tdo Loop(P, R);\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tBothOut() from P to R;// @\"b1=0 && b2=0\"\n\
       \t\t\tdo Loop(P, R);\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tInersect() from P to R; //@\"(b1=1 && b2=0) || (b1=0 && b2=1)\"\n\
       \t\t\tRes(i:int) from R to P;\n\
       \t\t\tdo Loop(P, R);\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tClose() from P to R;   \n\
       \t}\n\
       }\n\n" )
  ; ( "annot/SHHello.scr"
    , "module assrt.sh.SHHello;\n\n\
       // This is version of the algorithm with initial hellp messages, \
       used mainly for testing. \n\
       // See SH.scr for the pure version of the protocol without auxiliary \
       messages\n\n\
       type <dotnet> \"System.UInt32\" from \"...\" as int;\n\n\
       global protocol SH(role P, role R, role C)\n\
       {\n\
       \thello() from P to R; \n\
       \thello() from R to P; \n\
       \thello() from P to C;\n\
       \thello() from C to P; \n\
       \tPlane(x1:int, x2:int, x3:int, x4:int) from P to R;\n\
       \tdo Loop(P, R, C);\n\
       }\n\n\
       aux global protocol Loop(role P, role R, role C)\n\
       {\n\
       \tchoice at P\n\
       \t{\n\
       \t\tIsAbove(v1:int) from P to R;   \n\
       \t\tRes(b1:int) from R to P; // @\"b1=0 || b1=1\"\n\
       \t\tIsAbove(v2:int) from P to R;   \n\
       \t\tRes(b2:int) from R to P; // @\"b2=0 || b2=1\"\n\
       \t\tchoice at P\n\
       \t\t{\n\
       \t\t\tBothIn() from P to R; // @\"b1=1 && b2=1\"\n\
       \t\t\tBothIn(r1:int) from P to C;\n\
       \t\t\tdo Loop(P, R, C);\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tBothOut() from P to R; // @\"b1=0 && b2=0\"\n\
       \t\t\tBothOut() from P to C;\n\
       \t\t\tdo Loop(P, R, C);\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tIntersct(y1:int, y2:int) from P to R; // @\"(b1=1 && b2=0) || \
       (b1=0 && b2=1)\" // && y1=v1 && y2:v2\n\
       \t\t\tRes(i:int) from R to P;\n\
       \t\t\tchoice at P\n\
       \t\t\t{\n\
       \t\t\t\tSecOut(r2:int) from P to C; // @\"b2=0\" // && r2=i\n\
       \t\t\t\tdo Loop(P, R, C);\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tSecIn(r3:int, r4:int) from P to C; // @\"b2=1\" //  && (r3=i \
       && r4=v2)\n\
       \t\t\t\tdo Loop(P, R, C);\n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tClose() from P to R;   \n\
       \t\tClose() from P to C;\n\
       \t}\n\
       }\n\n\n" )
  ; ( "annot/TravelAgency.scr"
    , "//$ ./scribblec.sh -ip scribble-demos/scrib/travel/src -d \
       scribble-demos/scrib/travel/src \
       scribble-demos/scrib/travel/src/travel/Travel.scr -api Booking C\n\
       //$ javadoc -cp \
       scribble-core/target/classes:scribble-runtime/target/classes:scribble-demos/scrib/travel/src \
       scribble-demos/scrib/travel/src/travel/*.java -subpackages \
       travel.Travel.Booking -d scribble-demos/scrib/travel/javadoc\n\n\
       module travel.Travel;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\n\n\
       //*\n\
       global protocol Booking(role C, role A, role S)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tQuery(int) from C to A;\n\
       \t\tQuote(int) from A to C;\n\
       \t\tDummy() from A to S;   // Dummy\n\
       \t\tdo Booking(C, A, S);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\tYes() from C to A;\n\
       \t\t\tYes() from A to S;\n\
       \t\t\tPayment(int) from C to S;\n\
       \t\t\tAck() from S to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tNo() from C to A;\n\
       \t\t\tNo() from A to S;\n\
       \t\t}\n\
       \t\tBye() from C to A;\n\
       \t}\n\
       }" )
  ; ( "annot/ThreeBuyer.scr"
    , "/**\n\
      \ * Taken from [MSCS16] Global Progress for Dynamically Interleaved \
       Sessions\n\
      \ */\n\n\
       module threebuyer.ThreeBuyer;\n\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\
       type <java> \"test.twobuyer.Date\" from \"test/twobuyer/Date.java\" \
       as Date;\n\n\n\
       // 1 = B, 2 = A, 3 = S\n\
       global protocol TwoBuyer(role A, role B, role S)\n\
       {\n\
       \tempty1(int) from A to S;\n\
       \tempty2(int) from S to A;\n\
       \tempty3(int) from S to B;\n\
       \tempty4(int) from A to B;\n\n\
       \tchoice at B\n\
       \t{\n\
       \t\tok(int) from B to A;\n\
       \t\tok(int) from B to S;\n\
       \t\tempty5(int) from S to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tquit() from B to A;\n\
       \t\tquit() from B to S;\n\
       \t}\n\
       }\n\n\n\n" )
  ; ( "annot/Adder.scr"
    , "module Adder;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol Adder(role C, role S)\n\
       {\n\
       \trec Loop {\n\
       \t\tHELLO(u:int) from C to S;\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\tADD(w:int) from C to S; \n\
       \t\t\tADD(v:int) from C to S; \n\
       \t\t\tRES(f:int) from S to C;\n\
       \t\t\tcontinue Loop; \n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tBYE() from C to S;\n\
       \t\t\tBYE() from S to C;\n\
       \t\t}\n\
       \t}\n\
       }\n" )
  ; ( "annot/FibnoAss.scr"
    , "module FibnoAss;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\n\
       global protocol Adder(role C, role S)\n\
       {\n\
       \tHELLO(s:int) from C to S;\n\
       \tHELLO(f:int) from S to C;\n\
       \t\n\
       \trec Loop {\n\
       \t\tHELLO(u:int) from C to S;\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\tADD(y:int) from C to S; \n\
       \t\t\tRES(w:int) from S to C;\n\
       \t\t\tcontinue Loop; \n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tBYE() from C to S;\n\
       \t\t\tBYE() from S to C;\n\
       \t\t}\n\
       }\n\
       }\n" )
  ; ( "annot/Http.scr"
    , "//$ ./scribblec.sh -ip scribble-demos/scrib/http/src -d \
       scribble-demos/scrib/http/src \
       scribble-demos/scrib/http/src/http/longvers/HttpLong.scr -api Http C\n\
       //$ javadoc -cp \
       scribble-core/target/classes:scribble-runtime/target/classes:scribble-demos/scrib/http/src/ \
       scribble-demos/scrib/http/src/http/longvers/*.java -subpackages \
       http.longvers.HttpLong.Http -d \
       scribble-demos/scrib/http/javadoc/longvers\n\n\n\
       module icse18.Http;\n\n\
       global protocol Http(role C, role S)\n\
       {\n\
       \tdo Request(C, S); \n\
       \tdo Response(C, S);\n\
       }\n\n\
       aux global protocol Request(role C, role S)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tREQUESTL() from C to S;  \n\
       \t\trec X\n\
       \t\t{\n\
       \t\t\tchoice at C\n\
       \t\t\t{\n\
       \t\t\t\tHOST() from C to S; \n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tUSERA() from C to S;  \n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tACCEPT() from C to S; \n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tACCEPTL() from C to S; \n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tACCEPTE() from C to S; \n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tDNT() from C to S; \n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tCONNECTION() from C to S; \n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\tUPGRADEIR() from C to S; \n\
       \t\t\t\tcontinue X;\n\
       \t\t\t}\n\
       \t\t\tor\n\
       \t\t\t{\n\
       \t\t\t\t//CRLF from C to S;  \n\
       \t\t\t\tBODY() from C to S; \n\
       \t\t\t}\n\
       \t\t}\n\
       \t}\n\
       }\n\
       \t\n\
       aux global protocol Response(role C, role S)\n\
       {\n\
       \tHTTPV() from S to C;  \n\
       \tchoice at S\n\
       \t{\n\
       \t\t200() from S to C;  \n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\t404() from S to C;\n\
       \t}\n\n\
       \trec Y\n\
       \t{\n\
       \t\tchoice at S\n\
       \t\t{\n\
       \t\t\tDATE() from S to C; \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tSERVER() from S to C; \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tSTRICTTS() from S to C;  \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tLASTM() from S to C;  \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tETAG() from S to C; \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tACCEPTR() from S to C;  \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tCONTENTL() from S to C;  \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tVARY() from S to C;  \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tCONTENTT() from S to C; \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tVIA() from S to C;  \n\
       \t\t\tcontinue Y;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tBODY() from S to C;  \n\
       \t\t}\n\
       \t}\n\
       }\n" )
  ; ( "annot/SH.scr"
    , "module SH; \n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\n\
       global protocol SH(role S, role C, role P)\n\
       {\t\n\
       send(x1:int) from S to C;\n\
       rec Loop {\n\
       \tchoice at S { \n\
      \      vertex(p1:int) from S to C;\n\
      \      vertex(p2:int) from S to C;   \n\
      \      choice at C { \n\
      \      \t\tInOrOut(res:int) from C to S; @\"res=0\" \n\
      \      \t\t\tchoice at S { \n\
       \t\t \t\t\taddpoint(z:int) from S to P; @\"z=p2\"\n\
       \t\t \t\t\tcontinue Loop; \n\
       \t  \t\t\t} or {\n\
       \t\t\t\t\tnone() from S to P;  @\"res=1\"\n\
       \t\t\t\t\tcontinue Loop; \n\
       \t\t\t\t}\n\
       \t\t\t} or {\n\
       \t\t\t\tintersection(p3:int) from C to S;\n\
       \t\t\t\taddpoint(p4:int) from S to P; @\"p3=p4\"\n\
       \t\t\t\tcontinue Loop;\n\
       \t   \t\t}\n\
       \t } or {\n\
      \      close() from S to C; \n\
      \      close() from S to P;\n\
      \ \t}\n\
      \  }\n\
       }\n\n" )
  ; ( "annot/Travel.scr"
    , "//$ ./scribblec.sh -ip scribble-demos/scrib/travel/src -d \
       scribble-demos/scrib/travel/src \
       scribble-demos/scrib/travel/src/travel/Travel.scr -api Booking C\n\
       //$ javadoc -cp \
       scribble-core/target/classes:scribble-runtime/target/classes:scribble-demos/scrib/travel/src \
       scribble-demos/scrib/travel/src/travel/*.java -subpackages \
       travel.Travel.Booking -d scribble-demos/scrib/travel/javadoc\n\n\
       module travel.Travel;\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\n\n\
       //*\n\
       global protocol Booking(role C, role A, role S)\n\
       {\n\
       \tchoice at C\n\
       \t{\n\
       \t\tQuery(int) from C to A;\n\
       \t\tQuote(int) from A to C;\n\
       \t\tDummy() from A to S;   // Dummy\n\
       \t\tdo Booking(C, A, S);\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tchoice at C\n\
       \t\t{\n\
       \t\t\tYes() from C to A;\n\
       \t\t\tYes() from A to S;\n\
       \t\t\tPayment(int) from C to S;\n\
       \t\t\tAck() from S to C;\n\
       \t\t}\n\
       \t\tor\n\
       \t\t{\n\
       \t\t\tNo() from C to A;\n\
       \t\t\tNo() from A to S;\n\
       \t\t}\n\
       \t\tBye() from C to A;\n\
       \t}\n\
       }" )
  ; ( "annot/TwoBuyer.scr"
    , "module twobuyer.TwoBuyer;\n\n\n\
       type <java> \"java.lang.Integer\" from \"rt.jar\" as int;\n\
       type <java> \"java.lang.String\" from \"rt.jar\" as String;\n\
       type <java> \"test.twobuyer.Address\" from \
       \"test/twobuyer/Address.java\" as Address;\n\
       type <java> \"test.twobuyer.Date\" from \"test/twobuyer/Date.java\" \
       as Date;\n\n\n\
       global protocol TwoBuyer(role A, role B, role S)\n\
       {\n\
       \ttitle(int) from A to S;\n\
       \t//quote(int) from S to A, B;  // EFSM building for multicast not \
       currently supported\n\
       \tquote(x: int) from S to A;\n\
       \tquote(y: int) from S to B; \n\
       \tquoteByTwo(z:int) from A to B;\n\
       \tchoice at B\n\
       \t{\n\
       \t\tok(int) from B to S; @\"z > y - 2\"  \n\
       \t\tempty1(int) from S to B;\n\
       \t}\n\
       \tor\n\
       \t{\n\
       \t\tquit() from B to S; @\"z <= y - 2\"\n\
       \t}\n\
       }\n" ) ]
