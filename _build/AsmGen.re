open TinyLisp;

type functionSpec =
  { name: string
  , target: Ptx.RegisterType.t
  , params: list(Ptx.RegisterType.t)
  , generate: (list(Ptx.RegisterSpec.t), Ptx.RegisterSpec.t) => list(Ptx.Statement.Instruction.t)
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

let intrinsics = {
  open Ptx;
  let p0 = RegisterSpec.{id: 0, rType: RegisterType.Pred};
  let bd0 = RegisterSpec.{id: 0, rType: RegisterType.B64};
  let bd1 = RegisterSpec.{id: 1, rType: RegisterType.B64};
  [ { name: "="
    , target: RegisterType.Pred
    , params: [ RegisterType.B64 , RegisterType.B64 ]
    , generate:
        ([bd0, bd1], p0) =>
          [ Statement.Instruction.(
              `SetPredicate(
                  `Equal
                , p0
                , `Register(bd0)
                , `Register(bd1)))
          ]
    }
  , { name: "*"
    , target: RegisterType.F64
    , params: [ RegisterType.F64 , RegisterType.F64 ]
    , generate:
        ([fd0, fd1], fd2) =>
          [ Statement.Instruction.(
              `Multiply(
                  fd2
                , `Register(fd0)
                , `Register(fd1)))
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

let rec generate(~ctx, prog) =
switch prog {
| IfElse(cond, thn, els) =>
    let cond = generate(~ctx, cond);
    let sec1 = generate(~ctx, thn);
    let sec2 = generate(~ctx, els);
    let endLabel = Ptx.generateLabel();
    let sec2Label = Ptx.generateLabel();
    switch cond {
    | (Some({Ptx.RegisterSpec.rType: Pred} as predReg), instrs) =>
        let body =
          [ instrs
          , [ `Branch(Some(predReg), sec2Label) ]
          , snd(sec1)
          , [`Branch(None, endLabel) ]
          , [`Label(sec2Label)]
          , snd(sec2)
          , [ `Label(endLabel) ]
          ];
        let body = List.concat(body);
        List.map(Ptx.Statement.Instruction.emit, body);
        (Some(predReg), body)
    | _ => raise(Failure("Please use a logical expression in your conditional"))
    }
| FCall(name, params) =>
    Printf.printf("Handling function call: %s\n", name);
    let `Function(func) =
      try 
        (List.find(
          (x) => 
            switch x {
            | `Function f => f.name == name
            | _ => false
            },
          ctx
          ))
      {
      | _ => raise(Failure("No such function: " ++ name))
      };
    let (pRegs, tReg) = reserveFuncRegs(func);
    let params = List.map(generate(~ctx), params);
    let body =
      List.map2(
        ((reg, instr), targ) => {
          let after: list(Ptx.Statement.Instruction.t) =
            switch reg {
            | Some(reg) => [`Move(targ, `Register(reg))]
            | None => []
            };
          List.concat([instr, after])
        },
        params,
        pRegs
      );
    let pRegs' = List.map((r) => `Register(r), pRegs);
    let call:list(Ptx.Statement.Instruction.t) = {
      open Ptx.Statement.Instruction;
      [ `Call(tReg, name, pRegs') ]
    };
    let body = List.flatten(body @ [ call ]);
    (Some(tReg), body)
/*| Func(name, params, def) =>
    let body = {
      open Ptx.Statement;
      let fSpec =
        Directive.{
            name: name
          , return: `Declaration(Ptx.StateSpace.Register, )};
      [ `Function({}) ]
    };*/
| Symb(s) =>
    (Some(Ptx.RegisterSpec.{rType: Ptx.RegisterType.F64, id: 9999}), [])
| Literal(Float(f)) =>
    let r = Ptx.RegisterSpec.generate(F64);
    let body = [ `Move(r, `FloatLiteral(f, r.rType)) ];
    (Some(r), body)
| Literal(Int(n)) =>
    let r = Ptx.RegisterSpec.generate(S64);
    let body = [ `Move(r, `IntLiteral(n, r.rType)) ];
    (Some(r), body)
| Literal(Char(c)) =>
    let r = Ptx.RegisterSpec.generate(S8);
    let body = [ `Move(r, `IntLiteral(Char.code(c), r.rType)) ];
    (Some(r), body)
};

let parseProgram(s) =
  TinyLisp.build(s);

let compileProgram(src) = {
  /*let src = "(if (= 16 16) 12.3 32.1)"; */
  let prog = parseProgram(src);
  let ctx = List.map((x) => `Function(x), intrinsics);
  let (Some(lastReg), geny) = generate(~ctx, prog);
  let rReg = Ptx.RegisterSpec.{rType: Ret64, id: 0};
  let body = geny @ [ `Move(rReg, `Register(lastReg)) ];
  let func = {
    open Ptx.Statement.Directive;
    let rRegEmit = Ptx.RegisterSpec.emit(rReg);
    let argDecl = `Declaration(Ptx.StateSpace.Register, Ptx.RegisterType.F64, "%fd9999", None);
    let fSpec: functionSpec =
      { name: "cuCamlKernelImpl"
      , return: `Declaration(Ptx.StateSpace.Register, rReg.rType, rRegEmit, None)
      , parameters: [argDecl]
      , declarations:
          List.filter(
            (decl) =>
              /* don't want to declare the return or parameter */
              switch decl {
              | `Declaration(_, _, "%rvald0", _)
              | `Declaration(_, _, "%fd9999", _) =>
                  false
              | _ => true
              },
              Ptx.Statement.declareRegisters(body)
          )
      , body: body};
    `Function(fSpec)
  };
  let whole = {
    let kernel = {
      open Ptx;
      let kName = "myTestKernel";
      open Statement;
      open Directive;
      let params =
        [ `Declaration(StateSpace.Parameter, RegisterType.U64, "paramX", None)
        , `Declaration(StateSpace.Parameter, RegisterType.U64, "sz", None)
        ];
      open Instruction;
      open RegisterSpec;
      open OperandSpec;
      open RegisterType;
      let body =
        [ /*`Declaration(StateSpace.Register, U32, "%r", Some(5))
        , `Declaration(StateSpace.Register, U64, "%rd", Some(4))
        , `Declaration(StateSpace.Register, F64, "%fd", Some(1))
        ,*/ `Load(StateSpace.Parameter, {rType: U64, id: 0}, `Dereference(`Label("paramX"), U64))
        , `Move({rType: U32, id: 0}, `Register({rType: Special(Ntid(`x)), id: -1}))
        , `Move({rType: U32, id: 1}, `Register({rType: Special(Ctaid(`x)), id: -1}))
        , `Move({rType: U32, id: 2}, `Register({rType: Special(Tid(`x)), id: -1}))
        , `MultiplyAndAdd(
            {rType: U32, id: 3}
          , `Register({rType: U32, id: 0})
          , `Register({rType: U32, id: 1})
          , `Register({rType: U32, id: 2}))
        , `Multiply({rType: U64, id: 3}, `Register({rType: U32, id: 3}), `IntLiteral(RegisterType.getBytes(F64), U32))
        , `ConvertAddress(StateSpace.Global, {rType: U64, id: 1}, `Register({rType: U64, id: 0}))
          /* get the address of the i-th element of the array and put it in rd2 */
        , `Add({rType: U64, id: 2}, `Register({rType: U64, id: 1}), `Register({rType: U64, id: 3}))
          /* put the value of the i-th array element in fd9999 */
        , `Move({rType: F64, id: 9999}, `Dereference(`Register({rType: U64, id: 2}), F64))
        , `Call({rType: F64, id: 10000}, "cuCamlKernelImpl", [`Register({rType: F64, id: 9999})])
        /*, `Store(StateSpace.Global, `Dereference(`Register({rType: U64, id: 2}), U32), `Register({rType: U32, id: 3}))*/
        , `Store(StateSpace.Global, `Dereference(`Register({rType: U64, id: 2}), F64), `Register({rType: F64, id: 10_000}))
        ];
      let declarations = Statement.declareRegisters(body);
      `Entry(kName, params, declarations, body)
    };
    open Ptx.Statement.Directive;
    let whole =
      [ `Version{major: 5, minor: 0}
      , `Target("sm_20")
      , `AddressSize(64)
      , func
      , kernel
      ];
    whole
  };
  Ptx.Statement.emit(whole)
}