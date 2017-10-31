open TinyLisp;

type functionSpec =
  { name: string
  , target: Ptx.RegisterSpec.t
  , params: list(Ptx.RegisterSpec.t)
  , body: list(Ptx.Statement.Instruction.t)
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
      (x) => {
        let typ = x.rType;
        RegisterSpec.generate(typ)
      },
      fSpec.params);
  let tReg = RegisterSpec.generate(fSpec.target.rType);
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
    , target: p0
    , params: [ bd0 , bd1 ]
    , body:
      [ Statement.Instruction.(
          `SetPredicate(
              `Equal
            , p0
            , `Register(bd0)
            , `Register(bd1)))
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

let prog = parseProgram("(if (= 16 16) 12.3 32.1)");
let ctx = List.map((x) => `Function(x), intrinsics);
let (Some(lastReg), geny) = generate(~ctx, prog);
let body = geny @ [ `Move(Ptx.RegisterSpec.{rType: Ret64, id: 0}, `Register(lastReg)) ];
let entry: Ptx.Statement.Directive.t = `Entry("myTestKernel", [], [], geny);
let whole = [entry];
print_endline(Ptx.Statement.emit(whole))