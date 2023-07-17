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

    val expTest3 = Expr(Expr(===, Expr(Sealed(Vector[String]("x", "y")), Var("x"), Var("y"))),Expr(Sealed(Vector[String]("y", "z")), Var("x"), Var("y"), Expr(===, Expr(StringLiteral("Add"), Var("z")), Var("y"))))
    System.out.println("expTest3 = " + expTest3 );


    val initial = State(
      Space(Expr(StringLiteral("F"), StringLiteral("a"))), //query
      Space(
        Expr(===, Expr(Sealed(Vector[String]("x")), StringLiteral("F"), Var("x")), Expr(StringLiteral("prim"), Var("x"))),
      ),
      Space(),
      Space()
    )

    val resulting = State(
      Space(),
      initial.k,
      Space(),
      Space(Expr(StringLiteral("prim"),StringLiteral("x")))
    )

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
