import munit.FunSuite


class Nats extends FunSuite:
  test("basic") {
    val S = StringLiteral("Successor")
    val Z = StringLiteral("Zero")

    val initial = State(
      Space(Expr(StringLiteral("Add"), Expr.nest(S, S, S, Z), Expr.nest(S, Z))),
      Space(
        Expr(===, Expr(StringLiteral("Add"), Var("xz"), Z), Var("xz")),
        Expr(===, Expr(StringLiteral("Add"), Var("xs"), Expr(S, Var("ys"))), Expr(StringLiteral("Add"), Expr(S, Var("xs")), Var("ys"))),
      ),
      Space(),
      Space()
    )

    val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(Expr.nest(S, S, S, S, Z))
    )

     assert(execute_print(initial, allRules) == resulting)
  }

class SealedTest extends FunSuite:
  test("basic") {

    /*
    We introduce a new unify, written unify+, defined by

unify+( t, u ) = unify( t, u )
unify+( ( sealed x1 … xN t ), u ) = unify( t, u )
Intuitively,

(= (sealed x (f x)) (prim x))

distinguishes between the x inside the seal and the x outside the seal.

(= (f asldkfjadflkajsdflkajsdf) (prim x))

(sealed x (= (f x) (prim x)))

(and (sealed x (= (f x) (prim x))) (log filename x))

Example 1.
< {(f a)}, {(= (sealed x (f x)) (prim x))}, {}, {} >
=> (by query rule)
< {}, {(= (sealed x (f x)) (prim x))}, {(prim x)}, {} >
=> (by builtin rules)
< {}, {(= (sealed x (f x)) (prim x))}, {prim(x)}, {} >
=> (x is not bound)
< {}, {(= (sealed x (f x)) (prim x))}, {error("x is not bound")}, {} >
=> (by output rule)
< {}, {(= (sealed x (f x)) (prim x))}, {}, {error("x is not bound")} >

Example 2.
< {(f a)}, {(sealed x (= (f x) (prim x)))}, {}, {} >
=> (by query rule for sealed terms)
< {}, {(sealed x (= (f x) (prim x)))}, {(prim a)}, {} > 
=> (by builtin rules)
< {}, {(sealed x (= (f x) (prim x)))}, {prim(a)}, {} > 
=> (by output rule)
< {}, {(sealed x (= (f x) (prim x)))}, {}, {prim(a)} > 

Example 3.
< {(= (= (f x) y) (= (f x) (and y (log filename x)))), (f a)}, {(sealed x (= (f x) (prim x)))}, {}, {} >
=> (by query rule for sealed terms)
< {(f a)}, {(sealed x (= (f x) (prim x)))}, {(= (f x) (and (prim x) (log filename x')))}, {} >
=> (by query rule for sealed terms)
< {}, {(sealed x (= (f x) (prim x)))}, {(prim a), (= (f x) (and (prim x) (log filename x')))}, {} >
=> (by builtin rules)
< {}, {(sealed x (= (f x) (prim x)))}, {prim(a), (= (f x) (and (prim x) (log filename x')))}, {} >
=> (by output rule)
< {}, {(sealed x (= (f x) (prim x)))}, {(= (f x) (and (prim x) (log filename x')))}, {prim(a)} >
    */

    // val x = Expr(===, Expr(StringLiteral("F"), Var("x")))
    // System.out.println(x);
    // val y = Expr(===, Expr(StringLiteral("Add"), Var("x")), Var("y"));
    // System.out.println(y);

    // val expTest = Expr(`sealedVars`, Var("x"), Var("y"), Expr(===, Expr(StringLiteral("Add"), Var("z")), Var("y")))
    // System.out.println("expTest = " + expTest );
    // val expTest2 = Expr(===, Expr(`sealedVars`, Var("x"), Var("y")))
    // System.out.println("expTest2 = " + expTest2 );
    val expTest3 = Expr(Expr(===, Expr(`sealedVars`, Sealed("x"), Sealed("y"), Var("x"), Var("y"))),Expr(`sealedVars`, Sealed("y"), Sealed("z"), Var("x"), Var("y"), Expr(===, Expr(StringLiteral("Add"), Var("z")), Var("y"))))
    System.out.println("expTest3 = " + expTest3 );
    // val expTest4 = Expr(===, Expr(`sealedVars`, Expr(StringLiteral("F"), Var("x"))), Expr(StringLiteral("prim"), Var("x")))
    // System.out.println("expTest4 = " + expTest4 );

    // val expTest5 = Expr(`sealedVars`, Var("x"), Expr(StringLiteral("prim"), Var("x")))
    // System.out.println("expTest5 = " + expTest5 );
    // val expTest5 = Expr(Expr(===, Expr(`sealedVars`, Var("x"), Var("y"))),Expr(`sealedVars`, Var("x"), Var("y"), Expr(===, Expr(StringLiteral("Add"), Var("z")), Var("y"))))
    // System.out.println("expTest5 = " + expTest5 );
    

    val initial = State(
      Space(Expr(StringLiteral("F"), StringLiteral("a"))), //query
      Space(
        Expr(===, Expr(`sealedVars`, Sealed("x"), StringLiteral("F"), Var("x")), Expr(StringLiteral("prim"), Var("x"))),
      ),
      Space(),
      Space()
    )

    // // val initial = State(
    // //   Space(Expr(StringLiteral("F"), StringLiteral("a"))), //query
    // //   Space(
    // //     Expr(===, Expr(StringLiteral("F"), Var("x")), Expr(StringLiteral("prim"), Var("x"))),
    // //     Expr(Expr(`sealedVars`, Var("x"), Var("y"), Expr(===, Expr(StringLiteral("Add"), Var("z")), Var("y")))),
    // //   ),
    // //   Space(),
    // //   Space()
    // // )

    val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(Expr(StringLiteral("prim"),StringLiteral("x")))
    )

    // //execute_print(initial, allRules) == resulting

    assert(execute_print(initial, allRules).w == resulting.w)
  }

class Inc extends FunSuite:
  test("basic") {
    val val1 = StringLiteral("VAL_1")
    val val2 = StringLiteral("VAL_2")
    val val3 = StringLiteral("VAL_3")
    val val4 = StringLiteral("VAL_4")

    val initial = State(
      Space(Expr(StringLiteral("match1"), Expr.nest(val1), Expr.nest(val2, val3))), //query
      Space(
        Expr(===, Expr(StringLiteral("match2"), Var("a"), val3), Expr(StringLiteral("match3"), Var("x"), val3)),
        Expr(===, Expr(StringLiteral("match1"), Var("x"), Expr(val2, Var("y"))), Expr(StringLiteral("match2"), Expr(Var("x"), val4), Var("y"))),
        Expr(===, Expr(StringLiteral("match3"), val1, val3), Expr(StringLiteral("match4"), val1)),
      ),
      Space(),
      Space()
    )

    val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(Expr(StringLiteral("match4"), val1))
    )

    //execute_print(initial, allRules) == resulting

    assert(execute_print(initial, allRules) == resulting)
  }


class FrogUnification extends FunSuite:
  test("basic") {
    val initial = State(
      Space(Expr(StringLiteral("if"), Expr(StringLiteral("croaks"), Var("x")), Var("x"))),
      Space(
        Expr(===, Expr(StringLiteral("if"), BoolLiteral(true), Var("then")), Var("then")),
        Expr(===, Expr(StringLiteral("frog"), Var("f")), Expr(Mul, Expr(StringLiteral("croaks"), Var("f")), Expr(StringLiteral("eat_flies"), Var("f")))),
        Expr(===, Expr(StringLiteral("croaks"), StringLiteral("Fritz")), BoolLiteral(true)),
        Expr(===, Expr(StringLiteral("eat_flies"), StringLiteral("Fritz")), BoolLiteral(true)),
        Expr(===, Expr(StringLiteral("green"), Var("g")), Expr(StringLiteral("frog"), Var("g")))
      ),
      Space(),
      Space()
    )

    val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(StringLiteral("Fritz"))
    )
    
    assertEquals(executeWithContext_print(initial, allInContext), resulting)
  }


class Socrates extends FunSuite:
  test("basic") {
    val TV = StringLiteral("TruthValue")
    val Human = StringLiteral("Human")

    val initial = State(
      Space(Expr(StringLiteral("And"), Expr(Human, StringLiteral("Socrates")), Expr(Human, StringLiteral("Sam")))),
      Space(
        Expr(===, Expr(Human, StringLiteral("Socrates")), Expr(TV, DoubleLiteral(0.9))),
        Expr(===, Expr(Human, StringLiteral("Sam")), Expr(TV, DoubleLiteral(0.7))),

        Expr(===, Expr(StringLiteral("And"), Expr(TV, Var("p1")), Expr(TV, Var("p2"))),
          Expr(TV, Expr(Mul, Var("p1"), Var("p2")))),
      ),
      Space(),
      Space()
    )

    lazy val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(Expr(TV, DoubleLiteral(0.9 * 0.7)))
    )

    assertEquals(executeWithContext(initial, allInContext), resulting)
  }
