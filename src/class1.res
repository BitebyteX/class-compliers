
//Name => Nameless => StackVM    (comp)
module Name = {
    type rec expr = 
        | Cst(int) 
        | Add(expr,expr)
        | Mul(expr,expr)
        | Var(string)
        | Let(string, expr, expr)

    type env = list<(string, int)>

    let rec eval = (expr, env) => {
        switch expr {
            | Cst(i) => i
            | Add(a,b) => eval(a, env) + eval(b, env)
            | Mul(a,b) => eval(a, env) * eval(b, env)
            | Var(x) => List.assoc(x, env)
            | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
        }
    }
}

module Nameless = {
    type rec expr = 
        | Cst(int) 
        | Add(expr,expr)
        | Mul(expr,expr)
        | Var(int)
        | Let(expr, expr)

    type env = list<int>
     
    let rec eval = (expr, env) => {
        switch expr {
            | Cst(i) => i
            | Add(a,b) => eval(a, env) + eval(b, env)
            | Mul(a,b) => eval(a, env) * eval(b, env)
            | Var(n) => List.nth(env, n)
            | Let(e1, e2) => eval(e2, list{(eval(e1, env)), ...env})
        }
    }
}

//lowering Name.expr to Nameless.expr
module Name2Nameless = {
    type cenv = list<string>

    let rec index = (cenv, x): int => {
        switch cenv {
            | list{} => assert false
            | list{a, ...rest} => 
              if a == x {
                0
              } else {
                index(rest, x) + 1
              }
        }
    }

    let rec comp = (expr : Name.expr, cenv): Nameless.expr => {
        switch expr {
            | Cst(i) => Cst(i)
            | Add(a,b) => Add(comp(a, cenv), comp(b, cenv))
            | Mul(a,b) => Mul(comp(a, cenv), comp(b, cenv))
            | Var(x) => Var(index(cenv, x))
            | Let(x, e1, e2) => Let(comp(e1, cenv), comp(e2, list{x, ...cenv}))
        }
    }

}

module StackVM = {
    type instr = 
        | Cst(int) 
        | Add
        | Mul
        | Var(int)                                              
        | Pop 
        | Swap
    type instrs = list<instr>
    type operand = int
    type stack = list<operand>

    let rec eval = (instrs : instrs, stk : stack) => {
        switch (instrs, stk) {
            | (list{Cst(i), ...rest},_) => eval(rest, list{i, ...stk})
            | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
            | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
            | (list{Var(i), ...rest}, _) => eval(rest, list{List.nth(stk, i), ...stk})
            | (list{Pop, ...rest}, list{_, ...stk}) => eval(rest, stk)
            | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
            | (list{}, list{a, ..._stk}) => a
            | _ => assert false
        }
    }
}

//lowering Nameless.expr to StackVM.instrs
module Nameless2StackVM = {

    type sv = Slocal | Stmp
    type senv = list<sv>

    let rec sindex = (senv, i): int => {
        switch senv {
        | list{} => assert false
        | list{Slocal, ...senv} => 
            if i == 0{
                0
            }else{
                sindex(senv, i - 1) + 1
            }
        | list{Stmp, ...senv} => sindex(senv, i) + 1
        }
    }
    
    let rec comp = (expr : Nameless.expr, senv: senv): StackVM.instrs => {
        switch expr {
            | Cst(i) => list{Cst(i)}
            | Var(i) => list{Var(sindex(senv, i))}
            // when local var reference appear in the right of a binary operator,
            // the stack will push the left temp value
            | Add(a, b) => Belt.List.concatMany([comp(a, senv), comp(b,list{Stmp, ...senv}), list{Add}])
            | Mul(a, b) => Belt.List.concatMany([comp(a, senv), comp(b,list{Stmp, ...senv}), list{Mul}])
            | Let(a, b) => Belt.List.concatMany([comp(a, senv), comp(b,list{Slocal, ...senv}), list{Swap, Pop}])
        }
    }

}


