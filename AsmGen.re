open TinyLisp;

type functionSpec =
  { name: string
  , target: Ptx.RegisterType.t
  , params: list(Ptx.RegisterType.t)
  , generate: (list(Ptx.OperandSpec.t), Ptx.RegisterSpec.t) => list(Ptx.Statement.Instruction.t)
  };

type sectionSpec =
  { label: string
  , body: list(Ptx.Statement.Instruction.t)
  };

type regMap = list((Ptx.RegisterSpec.t, Ptx.RegisterSpec.t));

type t =
  | Function(functionSpec, regMap)
  | Section(sectionSpec, regMap);

let reserveFuncRegs(fSpec) = {
  open Ptx;
  open RegisterSpec;
  let pRegs =
    List.map(
      (typ) => RegisterSpec.generate(typ),
      fSpec.params);
  let tReg = RegisterSpec.generate(fSpec.target);
  (pRegs, tReg)
};

/*let mapFuncToRegs(fSpec, pRegSet, tReg) = {
  let regMap = List.combine(fSpec.params, pRegSet);
  let regMap = [(fSpec.target, tReg), ...regMap];
  { name: fSpec.name
  , target: tReg
  , params: pRegSet
  , body: 
  }
};*/

let searchVar(ctx, nam) = {
  let rec aux(ls) =
    switch (ls) {
    | [`Function(_), ...tl]
    | [`Parameter(_), ...tl] => aux(tl)
    | [`Variable(name, regSpec), ...tl] when (name == nam) => Some(regSpec: Ptx.RegisterSpec.t)
    | [`Variable(name, regSpec), ...tl] => aux(tl)
    | [] => None
    };
  aux(ctx)
};

let searchFun(ctx, nam) = {
  let rec aux(ls) =
    switch (ls) {
    | [`Parameter(_), ...tl]
    | [`Variable(_), ...tl] => aux(tl)
    | [`Function({ name } as x), ...tl] when (name == nam) => Some(x)
    | [`Function({ name } as x), ...tl] => aux(tl)
    | [] => None
    };
  aux(ctx)
};

let intrinsics = {
  open Ptx;
  let p0 = RegisterSpec.{id: "0", rType: RegisterType.Pred};
  let bd0 = RegisterSpec.{id: "0", rType: RegisterType.B64};
  let bd1 = RegisterSpec.{id: "1", rType: RegisterType.B64};
  [ { name: "="
    , target: RegisterType.Pred
    , params: [ RegisterType.B64 , RegisterType.B64 ]
    , generate:
        ([bd0, bd1], p0) =>
          [ Statement.Instruction.(
              `SetPredicate(
                  `Equal
                , p0
                , bd0
                , bd1))
          ]
    }
  , { name: "*"
    , target: RegisterType.Any
    , params: [ RegisterType.F64 , RegisterType.F64 ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `Multiply(
                  fd2
                , fd0
                , fd1))
          ]
    }
  , { name: "/"
    , target: RegisterType.F64
    , params: [ RegisterType.F64 , RegisterType.F64 ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `Divide(
                  fd2
                , fd0
                , fd1))
          ]
    }
  , { name: "+"
    , target: RegisterType.Any
    , params: [ RegisterType.F64 , RegisterType.F64 ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `Add(
                  fd2
                , fd0
                , fd1))
          ]
    }
  , { name: "-"
    , target: RegisterType.Any
    , params: [ RegisterType.F64 , RegisterType.F64 ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `Subtract(
                  fd2
                , fd0
                , fd1))
          ]
    }
  , { name: "and"
    , target: RegisterType.Pred
    , params: [ RegisterType.Pred, RegisterType.Pred ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `And(
                  fd2
                , fd0
                , fd1))
          ]
    }
  , { name: ">"
    , target: RegisterType.Pred
    , params: [ RegisterType.F64 , RegisterType.F64 ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `SetPredicate(
                  `GreaterThan
                , fd2
                , fd0
                , fd1))
          ]
    }
  , { name: "<"
    , target: RegisterType.Pred
    , params: [ RegisterType.F64 , RegisterType.F64 ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `SetPredicate(
                  `LessThan
                , fd2
                , fd0
                , fd1))
          ]
    }
  , { name: ">="
    , target: RegisterType.Pred
    , params: [ RegisterType.F64 , RegisterType.F64 ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `SetPredicate(
                  `GreaterThanOrEqual
                , fd2
                , fd0
                , fd1))
          ]
    }
  , { name: "sqrt"
    , target: RegisterType.F64
    , params: [ RegisterType.F64 ]
    , generate:
        ([fd0], fd1) =>
          [ Statement.Instruction.(
              `SquareRoot(
                  fd1
                , fd0))
          ]
    }
  , { name: "rsqrt"
    , target: RegisterType.F64
    , params: [ RegisterType.F64 ]
    , generate:
        ([fd0], fd1) =>
          [ Statement.Instruction.(
              `ReverseSquareRoot(
                  fd1
                , fd0))
          ]
    }
  ];
};

let genLitOp(op) =
  switch op {
  | Float(f) => `FloatLiteral(f, Ptx.RegisterType.F64)
  | Int(n) => `IntLiteral(n, Ptx.RegisterType.S64)
  | Char(c) => `IntLiteral(Char.code(c), Ptx.RegisterType.S8)
  };

type context = [ `Function(functionSpec) | `Variable(string, Ptx.RegisterSpec.t) ];

open Ptx.OperandSpec;

/** generate' takes a context filled with function and variable bindings,
 * and returns a tuple containing:
 * (option(the register with the expression's value), Ptx instructions to be emitted)
 */
let rec generate'(~ctx:list(context), prog): (option(Ptx.OperandSpec.t), list(Ptx.Statement.Instruction.t), list(context)) =
switch prog {
| Assign(name, value) =>
    let rSpec =
      switch (searchVar(ctx, name)) {
      | Some(x) => x
      | None => raise(Failure("No such variable: " ++ name))
      };
    let (Some(resReg), instrs, _) = generate'(~ctx, value);
    let body = {
      open Ptx.Statement.Instruction;
      instrs @ [ `Move(rSpec, resReg) ]
    };
    (Some(`Register(rSpec)), body, ctx)
| IfElse(cond, thn, els) =>
    let cond = generate'(~ctx, cond);
    /** TODO: make this less fucky */
    let sec1 = generate'(~ctx, els);
    let sec2 = generate'(~ctx, thn);
    let endLabel = Ptx.generateLabel();
    let sec2Label = Ptx.generateLabel();
    switch cond {
    | (Some(`Register({Ptx.RegisterSpec.rType: Pred} as predReg)), instrs, ctx) =>
        let rType =
          switch (sec1, sec2) {
          | ((Some(x), _, _), _)
          | (_, (Some(x), _, _)) => Ptx.OperandSpec.getType(x)
          | ((None, _, _), (None, _, _)) => predReg.rType
          };
        let unionReg = Ptx.RegisterSpec.generate(rType);
        let (rSec2, sec2, _) = sec2;
        let nop = `Move(predReg, `Register(predReg));
        let sec2h = try (List.hd(sec2)) {
          | _ => nop
        };
        let (rTypeSec1, instrsSec1, _) = sec1;
        let body =
          [ instrs
          , [ `Branch(Some(predReg), sec2Label) ]
          , instrsSec1
          , (switch (rTypeSec1) { | Some(rSec1) => [`Move(unionReg, rSec1)] | None => [] })
          , [`Branch(None, endLabel) ]
          , [`Label(sec2Label, sec2h)]
          , try (List.tl(sec2)) { | _ => [] }
          , switch rSec2 { | Some(rSec2) => [`Move(unionReg, rSec2)] | None => [] }
          , [ `Label(endLabel, nop) ]
          ];
        let body = List.concat(body);
        List.map(Ptx.Statement.Instruction.emit, body);
        (Some(`Register(unionReg)), body, ctx)
    | _ => raise(Failure("Please use a logical expression in your conditional"))
    }
| FCall(name, params) =>
    let Some(func) = searchFun(ctx, name);
    let (pRegs, preamble) =
      List.fold_left(
        ((pRegs, acc), param) => {
          let (Some(op), instrs, ctx) = generate'(~ctx, param);
          (pRegs @ [op], acc @ instrs)
        },
        ([], []),
        params
      );
    let tReg = {
      let rType = 
        switch (func.target) {
        | Ptx.RegisterType.Any => Ptx.OperandSpec.getType(List.hd(pRegs))
        | x => x
        };
      Ptx.RegisterSpec.generate(rType)
    };
    let call:list(Ptx.Statement.Instruction.t) =
      preamble @ func.generate(pRegs, tReg);
    /*let call = func.generate(params, tReg);*/
    (Some(`Register(tReg)), call, ctx)
| Symb(s) =>
    let regSpec =
      switch (searchVar(ctx, s)) {
      | Some(regSpec) =>
          regSpec
      | None =>
          raise(Failure("Unrecognized symbol: " ++ s))
          /*Ptx.RegisterSpec.{rType: Ptx.RegisterType.F64, id: s}*/
      };
    (Some(`Register(regSpec)), [], ctx)
| Literal(Float(f)) =>
    (Some(`FloatLiteral(f, F64)), [], ctx)
| Literal(Int(n)) =>
    (Some(`IntLiteral(n, S64)), [], ctx)
| Literal(Char(c)) =>
    let r = Ptx.RegisterSpec.generate(S8);
    let body = [ `Move(r, `IntLiteral(Char.code(c), r.rType)) ];
    (Some(`Register(r)), body, ctx)
| Declaration(name, typeName) =>
    let rType =
      switch (typeName) {
      | "int" => Ptx.RegisterType.S64
      | "float" | "real" => Ptx.RegisterType.F64
      | "char" => Ptx.RegisterType.S8
      | s => raise(Failure("Unexpected type: " ++ s))
      };
    open Ptx.RegisterSpec;
    let regSpec = {
      rType: rType,
      id: "variable_" ++ name
    };
    (Some(`Register(regSpec)), [], [`Variable(name, regSpec), ...ctx])
| While(cond, stmts) =>
    let (Some(rReg), geny, _) =
      List.fold_left(
        ((rReg, instrs, ctx), stmt) => {
          let (rReg', instrs', ctx') = generate'(~ctx, stmt);
          (rReg', instrs @ instrs', ctx')
        },
        (None, [], ctx),
        stmts
      );
    let (Some(predReg), geny', _) = generate'(~ctx, cond);
    let predReg =
      switch predReg {
      | `Register(x) => x
      | `Label(_) | `IntLiteral(_) | `FloatLiteral(_) =>
          raise(Failure("Please use a valid logical expression as your loop condition"))
      };
    let condLabel = Ptx.generateLabel();
    let jmpLabel = Ptx.generateLabel();
    let geny = geny @ [`Label(jmpLabel, `Branch(Some(predReg), condLabel))];
    let geny' = [ `Label(condLabel, List.hd(geny')), ...List.tl(geny') ];
    let geny' = geny' @ [ `AntiPredicated(predReg, `Branch(None, jmpLabel)) ];
    let body = geny' @ geny;
    (Some(rReg), body, ctx)
| SExp(_) => raise(Failure("unmatched SExp"))
| Symb(_) => raise(Failure("unmatched Symb"))
| Literal(_) => raise(Failure("unmatched Literal"))
| Func(name, _, _) => raise(Failure("unmatched Func: " ++ name))
| FCall(_) => raise(Failure("unmatched FCall"))
| IfElse(_) => raise(Failure("unmatched IfElse"))
| Binding(_) => raise(Failure("unmatched Binding"))
| Declaration(_) => raise(Failure("unmatched Declaration"))
| _ => raise(Failure("generate' couldn't get a match"))
};

let generate(~ctx, prog) =
switch prog {
| Func(name, params, bod) =>
    let paramDecls =
      List.map(
        (TinyLisp.Declaration(name, typeName)) => {
            open Ptx;
            switch typeName {
            | "real" | "float" =>
                `Declaration(StateSpace.Parameter, RegisterType.F64, name, None)
            | "char" =>
                `Declaration(StateSpace.Parameter, RegisterType.U8, name, None)
            | "int" =>
                `Declaration(StateSpace.Parameter, RegisterType.S64, name, None)
            | s => raise(Failure("Invalid type name " ++ s ++ ", how'd you do that?!"))
            }
          },
        params
      );
    let regMap =
      List.map(
        (`Declaration(_, rType, name, _)) =>
          (name, Ptx.RegisterSpec.generate(rType)),
        paramDecls
      );
    let regInits =
      List.map(
        ((name, reg)) => {
          open Ptx;
          open RegisterSpec;
          open Statement.Instruction;
          let paramSpec = `Register({id: name, rType: reg.rType});
          `Load(StateSpace.Parameter, reg, `Dereference(paramSpec, reg.rType))
        },
        regMap
      );
    let argTypes = {
      open Ptx.RegisterSpec;
      List.map((`Declaration(_, rType, id, _)) => rType, paramDecls)
    };
    let ctx =
      List.fold_left(
        (acc, (name, reg)) =>
          [`Variable(name, reg), ...acc],
        ctx,
        regMap);
    let (Some(rOp), geny, _) =
      List.fold_left(
        ((rReg, instrs, ctx), stmt) => {
          let (rReg', instrs', ctx') = generate'(~ctx, stmt);
          (rReg', instrs @ instrs', ctx')
        },
        (None, regInits, ctx),
        bod
      );
    let rReg =
      switch rOp {
      | `Register(x) => x
      | `Label(_) | `IntLiteral(_) | `FloatLiteral(_) =>
          raise(Failure("Error: please return something that is stored in a register"))
      };
    let rRegEmit = Ptx.RegisterSpec.emit(rReg);
    let fSpec =
      Ptx.Statement.Directive.{
        name: name
      , return: `Declaration(Ptx.StateSpace.Register, rReg.rType, rRegEmit, None)
      , parameters: paramDecls
      , declarations: Ptx.Statement.declareRegisters(~pleaseDoNot=[rReg], geny)
      , body: geny
      };
    (Some(rReg), fSpec, argTypes)
| _ => raise(Failure("Statement in bad place"))
};


let parseProgram(s) =
  TinyLisp.build(s);

let compileProgram(src, otherFuncs) = {
  /*let src = "(if (= 16 16) 12.3 32.1)"; */
  open Ptx.Statement.Directive;
  let prog = parseProgram(src);
  let ctx = List.map((x) => `Function(x), intrinsics);
  let nonKernelProgs = List.filter((Func(name, _, _)) => (name == "kernel") == false, prog);
  let kernelProg = List.filter((Func(name, _, _)) => name == "kernel", prog);
  let newFunctions = List.map(generate(~ctx), nonKernelProgs);
  let newFunctionBuilders =
    List.map(
      ((Some(rRegSpec), f, fParams)) => `Function({
        name: f.name,
        target: rRegSpec.Ptx.RegisterSpec.rType,
        params: fParams,
        generate: (params, rReg) =>
          Ptx.Statement.Instruction.[`Call(rReg, f.name, params)]
      }),
      newFunctions
    );
  let ctx = newFunctionBuilders @ ctx;
  let (_, fSpec, _) = generate(~ctx, List.hd(kernelProg));
  let func = `Function(fSpec);
  let whole = {
    let kernel = {
      open Ptx;
      let kName = "myTestKernel";
      open Statement;
      open Directive;
      let params =
        [ `Declaration(StateSpace.Parameter, RegisterType.U64, "paramX", None)
        ];
      open Instruction;
      open RegisterSpec;
      open OperandSpec;
      open RegisterType;
      let body =
        [ `Load(StateSpace.Parameter, {rType: U64, id: "0"}, `Dereference(`Label("paramX"), U64))
        , `Move({rType: U32, id: "0"}, `Register({rType: Special(Ntid(`x)), id: "-1"}))
        , `Move({rType: U32, id: "1"}, `Register({rType: Special(Ctaid(`x)), id: "-1"}))
        , `Move({rType: U32, id: "2"}, `Register({rType: Special(Tid(`x)), id: "-1"}))
        , `MultiplyAndAdd(
            {rType: U32, id: "3"}
          , `Register({rType: U32, id: "0"})
          , `Register({rType: U32, id: "1"})
          , `Register({rType: U32, id: "2"}))
        , `Multiply({rType: U64, id: "3"}, `Register({rType: U32, id: "3"}), `IntLiteral(RegisterType.getBytes(F64), U32))
        , `ConvertAddress(StateSpace.Global, {rType: U64, id: "1"}, `Register({rType: U64, id: "0"}))
          /* get the address of the i-th element of the array and put it in rd2 */
        , `Add({rType: U64, id: "2"}, `Register({rType: U64, id: "1"}), `Register({rType: U64, id: "3"}))
          /* put the value of the i-th array element in fd9999 */
        , `Load(StateSpace.Global, {rType: F64, id: "9999"}, `Dereference(`Register({rType: U64, id: "2"}), B64))
        , `Call({rType: F64, id: "10000"}, "kernel", [`Register({rType: F64, id: "9999"})])
        /*, `Store(StateSpace.Global, `Dereference(`Register({rType: U64, id: 2}), U32), `Register({rType: U32, id: 3}))*/
        , `Store(StateSpace.Global, `Dereference(`Register({rType: U64, id: "2"}), F64), `Register({rType: F64, id: "10000"}))
        /*, `Store(StateSpace.Global, `Dereference(`Register({rType: U64, id: 1}), F64), `FloatLiteral(42.0, F64))*/
        ];
      let declarations = Statement.declareRegisters(body);
      `Entry(kName, params, declarations, body)
    };
    open Ptx.Statement.Directive;
    let newFunctionsGeny = List.map(((_, x, _)) => `Function(x), newFunctions);
    let whole =
      [ `Version{major: 4, minor: 3}
      , `Target("sm_20")
      , `AddressSize(64)
      ]
      @ newFunctionsGeny
      @
      [ func
      , kernel
      ];
    whole
  };
  Ptx.Statement.emit(whole)
}