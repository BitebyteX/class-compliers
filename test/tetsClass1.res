open Class1
module TestName = {
  open Name
  let testName = () => {
    list{
      (Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))), 9),
      (
        Let("y", Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))), Mul(Var("y"), Var("y"))),
        81,
      ),
    }->Belt.List.forEach(((expr, result)) => assert (eval(expr, list{}) == result))

    exception Unreached
    list{
      Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("y"))),
      Let("y", Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))), Mul(Var("y"), Var("x"))),
    }->Belt.List.forEach(expr =>
      try {
        eval(expr, list{})->ignore
        raise(Unreached)
      } catch {
      | Unreached => assert false
      | _ => 0
      }
    )
    Js.log(`TestName:testName Passed`)
  }
}

TestName.testName()

module TestNameless = {
  open Nameless
  let testNameless = () => {
    list{
      (Let(Add(Cst(1), Cst(2)), Mul(Var(0), Var(0))), 9),
      (Let(Mul(Cst(1), Cst(2)), Add(Var(0), Var(0))), 4),
    }->Belt.List.forEach(((expr, result)) => assert (eval(expr, list{}) == result))

    Js.log(`TestNameless:testNameless Passed`)
  }
}

TestNameless.testNameless()

module TestName2Nameless = {
  open Name2Nameless
  list{
    (
      Name.Let("x", Add(Cst(1), Cst(2)), Mul(Var("x"), Var("x"))),
      Nameless.Let(Add(Cst(1), Cst(2)), Mul(Var(0), Var(0))),
    ),
    (
      Name.Let("x", Cst(1), Let("y", Cst(2), Add(Var("x"), Var("y")))),
      Nameless.Let(Cst(1), Let(Cst(2), Add(Var(1), Var(0)))),
    ),
    (
      Name.Let(
        "x",
        Cst(1),
        Let("y", Cst(2), Let("z", Cst(3), Mul(Add(Var("x"), Var("y")), Var("z")))),
      ),
      Nameless.Let(Cst(1), Let(Cst(2), Let(Cst(3), Mul(Add(Var(2), Var(1)), Var(0))))),
    ),
  }->Belt.List.forEach(((expr, result)) => assert(comp(expr,list{}) == result))

  Js.log(`TestName2Nameless:testName2Nameless Passed`)
}

module TestNameless2StackVM = {
  open Nameless2StackVM
  list{
      (
        Nameless.Let(Add(Cst(1), Cst(2)), Mul(Var(0), Var(0))),
        list{StackVM.Cst(1), Cst(2), Add, Var(0), Var(1), Mul, Swap, Pop},
      ),
      (
        Nameless.Let(Cst(1), Mul(Cst(2), Add(Cst(3), Var(0)))),
        list{StackVM.Cst(1), Cst(2), Cst(3), Var(2), Add, Mul, Swap, Pop},
      ),
      (
        Nameless.Let(Cst(1), Let(Cst(2), Add(Var(1), Var(0)))),
        list{StackVM.Cst(1), Cst(2), Var(1), Var(1), Add, Swap, Pop, Swap, Pop},
      ),
      (
        Nameless.Add(Cst(1), Let(Cst(2), Add(Var(0), Cst(7)))),
        list{StackVM.Cst(1), Cst(2), Var(0), Cst(7), Add, Swap, Pop, Add},
      ),
    }->Belt.List.forEach(((expr, result)) => assert (comp(expr,list{}) == result))

    Js.log(`TestNameless2StackVM:testNameless2StackVM Passed`)
}
