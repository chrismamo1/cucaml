Random.self_init();

let generateLabel() =
  Printf.sprintf("_cudaCamlSection_%d", Random.int(8_192));

module StateSpace = {
  type t =
    | Register
    | Global
    | Parameter
    | Shared;
  
  let emit(t) =
    switch t {
    | Register => ".reg"
    | Global => ".global"
    | Parameter => ".param"
    | Shared => ".shared"
    }
};

module RegisterType = {
  type specialRegister =
    | Ntid([ `x | `y | `z ])
    | Tid([ `x | `y | `z ])
    | Ctaid([ `x | `y | `z ]);

  type t =
    | Any
    | S8
    | U8
    | S32
    | S64
    | U32
    | U64
    | F32
    | F64
    | B32
    | B64
    | Pred
    | Ret64
    | Special(specialRegister);
  
  let getBytes(t) =
    switch t {
    | Any | Pred => raise(Invalid_argument("We don't know the width of Any or Pred registers"))
    | S8 | U8 => 1
    | S32 | U32 | F32 | B32 => 4
    | S64 | U64 | F64 | B64 | Ret64 => 8
    | Special(_) => raise(Failure("TODO: how wide are the special registers?"))
    };
  
  /* the prefix to use when naming registers, e.g. 64bit int registers are
     typically named "rd0" to "rdN" */
  let getPrefix(t) =
    switch t {
    | Any => "rany"
    | S8 => "sq"
    | S32 => "s"
    | S64 => "sd"
    | U32 => "r"
    | U64 => "rd"
    | F32 => "f"
    | F64 => "fd"
    | B32 => "b"
    | B64 => "bd"
    | Ret64 => "rvald"
    | Pred => "p"
    | Special(_) => raise(Invalid_argument("the getPrefix function is meaningless for special registers"))
    };

  let toString = (t) =>
    switch t {
    | Any => "rany"
    | S8 => "s8"
    | U8 => "u8"
    | S32 => "s32"
    | S64 => "s64"
    | U32 => "u32"
    | U64 => "u64"
    | F32 => "f32"
    | F64 => "f64"
    | B32 => "b32"
    | B64 => "b64"
    | Ret64 => "f64"
    | Pred => "pred"
    | Special(sType) =>
        let (sType, dim) =
          switch sType {
          | Ntid(dim) => ("ntid", dim)
          | Tid(dim) => ("tid", dim)
          | Ctaid(dim) => ("ctaid", dim)
          };
        let dim =
          switch dim {
          | `x => "x"
          | `y => "y"
          | `z => "z"
          };
        Printf.sprintf("%s.%s", sType, dim)
    };
  
  let emit(t) =
    Printf.sprintf(".%s", toString(t))
};

module RegisterSpec = {
  type t = {
    id: string,
    rType: RegisterType.t
  };

  let compare(a, b) =
    if (a.rType != b.rType) {
      let sa = RegisterType.toString(a.rType);
      let sb = RegisterType.toString(b.rType);
      String.compare(sa, sb)
    } else if (a.id != b.id) {
      compare(a.id, b.id)
    } else {
      0
    };

  let generate = {
    let s8Counter = ref(0);
    let u8Counter = ref(0);
    let s32Counter = ref(0);
    let s64Counter = ref(0);
    let u32Counter = ref(0);
    let u64Counter = ref(0);
    let f32Counter = ref(0);
    let f64Counter = ref(0);
    let b32Counter = ref(0);
    let b64Counter = ref(0);
    let predCounter = ref(0);
    open RegisterType;
    (rType) => {
      let counter =
        switch rType {
        | S8 => s8Counter
        | U8 => u8Counter
        | S32 => s32Counter
        | S64 => s64Counter
        | U32 => u32Counter
        | U64 => u64Counter
        | F32 => f32Counter
        | F64 => f64Counter
        | B32 => b32Counter
        | B64 => b64Counter
        | Pred => predCounter
        | Any
        | Ret64
        | Special(_) => raise(Invalid_argument("Can't arbitrarily generate special registers"))
        };
      let id = counter.contents;
      incr(counter);
      let id = string_of_int(id);
      {rType, id}
    }
  };

  let emit(~regMap=?, t) = {
    let t =
      switch regMap {
      | None | Some([]) => t
      | Some(regMap: list((t, t))) =>
          try
            (snd(List.find(((o, n)) => compare(o, n) == 0, regMap)))
            {
            | _ => t
            }
      };
    open RegisterType;
    try {
      let nid = int_of_string(t.id);
      switch t.rType {
      | Special(sType) =>
          let (sType, dim) = switch sType {
          | Ntid(dim) => ("ntid", dim)
          | Tid(dim) => ("tid", dim)
          | Ctaid(dim) => ("ctaid", dim)
          };
          let dim =
            switch dim {
            | `x => "x"
            | `y => "y"
            | `z => "z"
            };
          Printf.sprintf("%%%s.%s", sType, dim)
      | _ =>
          let prefix = RegisterType.getPrefix(t.rType);
          Printf.sprintf("%%%s%d", prefix, nid)
      };
    } {
    | _ => t.id
    }
  };
};

module OperandSpec = {
  type basicOperand =
    [ `FloatLiteral(float, RegisterType.t)
    | `IntLiteral(int, RegisterType.t)
    | `Label(string)
    | `Register(RegisterSpec.t)
    ];
  
  type t =
    [ `Dereference(basicOperand, RegisterType.t)
    | basicOperand
    ];
  
  let getType(t: [> t]) =
    switch t {
    | `Label(_) => raise(Invalid_argument("Labels don't really have a type"))
    | `Register{RegisterSpec.rType}
    | `IntLiteral(_, rType)
    | `FloatLiteral(_, rType)
    | `Dereference(_, rType) => rType
    };
  
  let getRegisterSpec(t: t) =
    switch t {
    | `Register(x) => Some(x)
    | _ => None
    };

  let rec emit(t: [> t]) =
    switch t {
    | `Label(s) => s
    | `Register(rSpec) => RegisterSpec.emit(rSpec)
    | `IntLiteral(n, RegisterType.S32 | RegisterType.U32 | RegisterType.S64 | RegisterType.U64 | RegisterType.B32) => string_of_int(n)
    | `FloatLiteral(f, RegisterType.F32 | RegisterType.F64) => Printf.sprintf("%05.3f", f)
    | `Dereference(expr, _) => Printf.sprintf("[%s]", emit(expr: basicOperand :> t))
    | `IntLiteral(_, rType)
    | `FloatLiteral(_, rType) =>
        let cls =
          switch t {
          | `IntLiteral(_) => "IntLiteral"
          | `FloatLiteral(_) => "FloatLiteral"
          };
        let msg = Printf.sprintf("%s doesn't take %s", cls, RegisterType.toString(rType));
        raise(Invalid_argument("numeric literal type mismatch: " ++ msg))
    };
};

module Statement = {
  module Instruction = {
    type comparison =
      [ `Equal
      | `NotEqual
      | `LessThan
      | `LessThanOrEqual
      | `GreaterThan
      | `GreaterThanOrEqual
      ];
    /* keep in mind that PTX assembly syntax is [instr.dstType.srcType dst, src] */
    type t =
      [ `Add(RegisterSpec.t, OperandSpec.t, OperandSpec.t)
      | `And(RegisterSpec.t, OperandSpec.t, OperandSpec.t)
      | `AntiPredicated(RegisterSpec.t, t)
      | `Branch(option(RegisterSpec.t), string)
      | `Call(RegisterSpec.t, string, list(OperandSpec.t))
      | `Convert(RegisterSpec.t, OperandSpec.t)
      | `ConvertAddress(StateSpace.t, RegisterSpec.t, OperandSpec.t)
      | `Divide(RegisterSpec.t, OperandSpec.t, OperandSpec.t)
      | `Label(string, t)
      | `Return
      | `ReverseSquareRoot(RegisterSpec.t, OperandSpec.t)
      | `ShiftRight(RegisterSpec.t, OperandSpec.t, OperandSpec.t)
      | `ShiftLeft(RegisterSpec.t, OperandSpec.t, OperandSpec.t)
      | `SetPredicate(comparison, RegisterSpec.t, OperandSpec.t, OperandSpec.t)
      | `SquareRoot(RegisterSpec.t, OperandSpec.t)
      | `Load(StateSpace.t, RegisterSpec.t, OperandSpec.t)
      | `Store(StateSpace.t, OperandSpec.t, OperandSpec.t)
      | `Subtract(RegisterSpec.t, OperandSpec.t, OperandSpec.t)
      | `Multiply(RegisterSpec.t, OperandSpec.t, OperandSpec.t)
      | `MultiplyAndAdd(RegisterSpec.t, OperandSpec.t, OperandSpec.t, OperandSpec.t)
      | `Move(RegisterSpec.t, OperandSpec.t)
      | `Nop
      | `Predicated(RegisterSpec.t, t)
      ];

    let rec emit(t: [> t]) =
      switch t {
      | `Add(dst, addend1, addend2) =>
          open RegisterSpec;
          Printf.sprintf(
              "add%s\t%s,%s,%s"
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(addend1)
            , OperandSpec.emit(addend2))
      | `And(dst, addend1, addend2) =>
          open RegisterSpec;
          Printf.sprintf(
              "and%s\t%s,%s,%s"
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(addend1)
            , OperandSpec.emit(addend2))
      | `AntiPredicated(predReg, instr) =>
          Printf.sprintf("@!%s\t%s", RegisterSpec.emit(predReg), emit(instr))
      | `Branch(pred, lName) =>
          switch pred {
          | Some(pred) =>
              Printf.sprintf(
                "@%s\tbra\t%s"
              , RegisterSpec.emit(pred)
              , lName)
          | None =>
              Printf.sprintf(
                "bra\t%s"
              , lName)
          }
      | `Call(rv, fName, params) =>
          let params = {
            let strings = List.map(OperandSpec.emit, params);
            String.concat(", ", strings)
          };
          Printf.sprintf(
              "call\t(%s),\t%s,\t(%s)"
            , RegisterSpec.emit(rv)
            , fName
            , params)
      | `Convert(dst, src) =>
          let mods = {
            open RegisterType;
            open RegisterSpec;
            switch dst.rType {
            | F32 | F64 => ".rz"
            | _ => ""
            }
          };
          let instr = {
            open RegisterSpec;
            open RegisterType;
            Printf.sprintf("cvt%s.%s.%s", mods, toString(dst.rType), toString(OperandSpec.getType(src)))
          };
          Printf.sprintf("%s\t%s,%s", instr, RegisterSpec.emit(dst), OperandSpec.emit(src));
      | `ConvertAddress(targetStateSpace, dst, src) =>
          open RegisterSpec;
          Printf.sprintf(
              "cvta.to%s%s\t%s,%s"
            , StateSpace.emit(targetStateSpace)
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(src))
      | `Divide(dst, num, den) =>
          open RegisterSpec;
          let mods = {
            open RegisterType;
            switch (dst.rType, OperandSpec.getType(num)) {
            | (F64, F64) => ".rn"
            | _ => raise(Invalid_argument("invalid register types for div"))
            }
          };
          Printf.sprintf(
              "div%s%s\t%s,%s,%s"
            , mods
            , RegisterType.emit(OperandSpec.getType(num))
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(num)
            , OperandSpec.emit(den))
      | `Label(lName, t) =>
          Printf.sprintf("%s:\t%s", lName, emit(t))
      | `Return =>
          "ret"
      | `ReverseSquareRoot(dst, src) =>
          open RegisterSpec;
          let mods = {
            open RegisterType;
            switch (dst.rType, OperandSpec.getType(src)) {
            | (F64, F64) => ".approx"
            | _ => raise(Invalid_argument("invalid register types for rsqrt"))
            }
          };
          Printf.sprintf(
              "rsqrt%s%s\t%s,%s"
            , mods
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(src))
      | `ShiftRight(dst, src, n) =>
          let instr = {
            open RegisterSpec;
            open RegisterType;
            Printf.sprintf("shr.%s", toString(dst.rType))
          };
          let n = OperandSpec.emit(n);
          Printf.sprintf("%s\t%s,%s,%s", instr, RegisterSpec.emit(dst), OperandSpec.emit(src), n);
      | `ShiftLeft(dst, src, n) =>
          let instr = {
            open RegisterSpec;
            open RegisterType;
            Printf.sprintf("shl.%s", toString(dst.rType))
          };
          let n = OperandSpec.emit(n);
          Printf.sprintf("%s\t%s,%s,%s", instr, RegisterSpec.emit(dst), OperandSpec.emit(src), n);
      | `SetPredicate(comparison, { RegisterSpec.rType } as dst, opA, opB)
            when (OperandSpec.getType(opA) == OperandSpec.getType(opB) && rType == RegisterType.Pred) =>
          open RegisterType;
          let comparison =
            switch (comparison, OperandSpec.getType(opA)) {
            | (`Equal, _) => "eq"
            | (`NotEqual, _) => "ne"
            | (`LessThan, (S8 | S32 | S64 | F32 | F64 | Any)) => "lt"
            | (`LessThanOrEqual, (S8 | S32 | S64 | F32 | F64 | Any)) => "le"
            | (`GreaterThan, (S8 | S32 | S64 | F32 | F64 | Any)) => "gt"
            | (`GreaterThanOrEqual, (S8 | S32 | S64 | F32 | F64 | Any)) => "ge"
            | (`LessThan, (U32 | U64)) => "lo"
            | (`LessThanOrEqual, (U32 | U64)) => "ls"
            | (`GreaterThan, (U32 | U64)) => "hi"
            | (`GreaterThanOrEqual, (U32 | U64)) => "hs"
            | ((`LessThan | `LessThanOrEqual | `GreaterThan | `GreaterThanOrEqual), (B32 | B64 | Pred | Special(_))) =>
                raise(Invalid_argument("The bSZ, pred, and special types are not ordered"))
            };
          let opType = RegisterType.emit(OperandSpec.getType(opA));
          Printf.sprintf(
              "setp.%s%s\t%s,%s,%s"
            , comparison
            , opType
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(opA)
            , OperandSpec.emit(opB))
      | `SetPredicate(_, _, _, _) =>
          raise(
            Invalid_argument(
              "SetPredicate must take a predicate register as the destination and the types of the operands must match"))
      | `SquareRoot(dst, src) =>
          open RegisterSpec;
          let mods = {
            open RegisterType;
            switch (dst.rType, OperandSpec.getType(src)) {
            | (F64, F64) => ""
            | _ => raise(Invalid_argument("invalid register types for sqrt"))
            }
          };
          Printf.sprintf(
              "sqrt%s%s\t%s,%s"
            , mods
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(src))
      | `Load(sSpace, dst, src) =>
          open RegisterSpec;
          Printf.sprintf(
              "ld%s%s\t%s,%s"
            , StateSpace.emit(sSpace)
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(src))
      | `Store(sSpace, dst, src) =>
          open RegisterSpec;
          Printf.sprintf(
              "st%s%s\t%s,%s"
            , StateSpace.emit(sSpace)
            , RegisterType.emit(OperandSpec.getType(dst))
            , OperandSpec.emit(dst)
            , OperandSpec.emit(src))
      | `Subtract(dst, subend1, subend2) =>
          open RegisterSpec;
          Printf.sprintf(
              "sub%s\t%s,%s,%s"
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(subend1)
            , OperandSpec.emit(subend2))
      | `Multiply(dst, factor1, factor2) =>
          open RegisterSpec;
          let mods = {
            open RegisterType;
            switch (dst.rType, OperandSpec.getType(factor1)) {
            | (F64, F64) => ""
            | (a, b) when a == b => ".lo"
            | (U64, U32)
            | (S64, S32) => ".wide"
            | _ => raise(Invalid_argument("invalid register types for mul"))
            }
          };
          Printf.sprintf(
              "mul%s%s\t%s,%s,%s"
            , mods
            , RegisterType.emit(OperandSpec.getType(factor1))
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(factor1)
            , OperandSpec.emit(factor2))
      | `MultiplyAndAdd(dst, factor1, factor2, addend) =>
          open RegisterSpec;
          let mods = {
            open RegisterType;
            switch (dst.rType, OperandSpec.getType(factor1)) {
            | (a, b) when a == b => ".lo"
            | (U64, U32)
            | (S64, S32) => ".wide"
            | _ => raise(Invalid_argument("invalid register types for mul"))
            }
          };
          Printf.sprintf(
              "mad%s%s\t%s,%s,%s,%s"
            , mods
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(factor1)
            , OperandSpec.emit(factor2)
            , OperandSpec.emit(addend))
      | `Move(dst, src) =>
          open RegisterSpec;
          Printf.sprintf(
              "mov%s\t%s,%s"
            , RegisterType.emit(dst.rType)
            , RegisterSpec.emit(dst)
            , OperandSpec.emit(src))
      | `Nop =>
          "nop.s"
      | `Predicated(predReg, instr) =>
          Printf.sprintf("@%s\t%s", RegisterSpec.emit(predReg), emit(instr))
      | _ =>
          raise(Invalid_argument("Can't emit that, is it an instruction?"))
      };
  };

  module Directive = {
    type versionSpec =
      { major: int
      , minor: int
      };

    type declaration =
      [ `Declaration(StateSpace.t, RegisterType.t, string, option(int)) ];

    type normalDirective =
      [ `Version(versionSpec) /* should be 6.0 */
      | `AddressSize(int) /* should be 64 */
      | `Target(string) /* should be sm_50 */
      | declaration
      ];

    /*type bodyStatement = [ normalDirective | Instruction.t ];*/

    type functionSpec =
      { name: string
      , return: declaration
      , parameters: list(declaration)
      , declarations: list(declaration)
      , body: list(Instruction.t)
      };

    type t =
      [ `Entry(string, list(declaration), list(declaration), list(Instruction.t))
      | `Function(functionSpec)
      | normalDirective
      ];
    
    let rec emit(t: t) =
      switch t {
      | `Version({major, minor}) =>
          Printf.sprintf(".version\t%d.%d", major, minor)
      | `AddressSize(bits) =>
          Printf.sprintf(".address_size\t%d", bits)
      | `Target(tName) =>
          Printf.sprintf(".target\t%s", tName)
      | `Declaration(sSpace, rType, name, None) =>
          Printf.sprintf(
            "%s\t%s\t%s", StateSpace.emit(sSpace), RegisterType.emit(rType), name)
      | `Declaration(sSpace, rType, name, Some(amount)) =>
          Printf.sprintf(
            "%s\t%s\t%s<%d>", StateSpace.emit(sSpace), RegisterType.emit(rType), name, amount)
      | `Entry(kName, params, decls, body) =>
          let params = {
            let strings = List.map((x) => emit(x: declaration :> t), params);
            String.concat(",\n\t", strings)
          };
          let decls = {
            let strings = List.map((x) => emit(x: declaration :> t), decls);
            String.concat(";\n\t", strings)
          };
          let body = {
            let strings = List.map(Instruction.emit, body);
            String.concat(";\n\t", strings)
          };
          Printf.sprintf(
              ".visible .entry\t%s(\n\t%s\n)\n{\n\t%s;\n\t%s;\n}"
            , kName
            , params
            , decls
            , body)
      | `Function({name: fName, return: retParam, parameters, declarations, body}) =>
          let parameters = {
            let strings = List.map((x) => emit(x: declaration :> t), parameters);
            String.concat(",\n\t", strings)
          };
          let declarations = {
            let strings = List.map((x) => emit(x: declaration :> t), declarations);
            String.concat(";\n\t", strings)
          };
          let body = {
            let strings = List.map(Instruction.emit, body);
            String.concat(";\n\t", strings)
          };
          Printf.sprintf(
              ".func\t(%s)\t%s(\n\t%s\n)\n{\n\t%s;\n\t%s;\n}"
            , emit(retParam: declaration :> t)
            , fName
            , parameters
            , declarations
            , body)
      | _ =>
          raise(Invalid_argument("Can't emit that kind of expression"))
      };
  };

  type t = list(Directive.t);

  module RegisterSpecSet = Set.Make(RegisterSpec);

  let addOptionToList(opt, ls) =
    switch opt {
    | Some(x) => [x, ...ls]
    | None => ls
    };

  /** TODO: write this */
  let declareRegisters(~pleaseDoNot=?, instructions: list(Instruction.t)) = {
    let rec collectRegisters(ls, acc) =
      switch ls {
      | [] => acc
      | [(hd: Instruction.t), ...tl] =>
          let rec getRegs(x) =
            switch x {
            | `Add(a, b, c)
            | `And(a, b, c)
            | `Divide(a, b, c)
            | `ShiftRight(a, b, c)
            | `ShiftLeft(a, b, c)
            | `SetPredicate(_, a, b, c)
            | `Subtract(a, b, c)
            | `Multiply(a, b, c) =>
                [a]
                |> addOptionToList(OperandSpec.getRegisterSpec(b))
                |> addOptionToList(OperandSpec.getRegisterSpec(c))
            | `Convert(a, b)
            | `ConvertAddress(_, a, b)
            | `Load(_, a, b)
            | `ReverseSquareRoot(a, b)
            | `SquareRoot(a, b)
            | `Store(_, `Register(a), b)
            | `Move(a, b) =>
                [a]
                |> addOptionToList(OperandSpec.getRegisterSpec(b))
            | `Store(_, _, a) =>
                []
                |> addOptionToList(OperandSpec.getRegisterSpec(a))
            | `MultiplyAndAdd(a, b, c, d) =>
                [a]
                |> addOptionToList(OperandSpec.getRegisterSpec(b))
                |> addOptionToList(OperandSpec.getRegisterSpec(c))
                |> addOptionToList(OperandSpec.getRegisterSpec(d))
            | `Predicated(a, instr)
            | `AntiPredicated(a, instr) =>
                [a, ...getRegs(instr)]
            | `Call(a, _, args) =>
                let regs =
                  List.map(OperandSpec.getRegisterSpec, args)
                  |> List.fold_left(
                      (acc, x) =>
                        switch x {
                        | Some(x) => [x, ...acc]
                        | None => acc
                        },
                      []);
                [a, ...regs]
            | `Branch(Some(a), _) =>
                [a]
            | `Branch(None, _) =>
                []
            | `Return
            | `Label(_) | `Nop =>
                []
            };
          let regs = getRegs(hd);
          let acc = List.fold_left((acc, x) => RegisterSpecSet.add(x, acc), acc, regs);
          collectRegisters(tl, acc)
      };
    let acc = RegisterSpecSet.empty;
    open RegisterSpec;
    let uniqueRegisters =
      RegisterSpecSet.filter(
        (x) =>
          switch x.rType {
          | RegisterType.Special(_) => false
          | _ => true
          },
        collectRegisters(instructions, acc));
    let uniqueRegisters' =
      switch (pleaseDoNot) {
      | Some(ls) =>
          List.fold_left(
            (acc, el) => RegisterSpecSet.remove(el, acc),
            uniqueRegisters, ls)
      | None => uniqueRegisters
      };
    List.map(
      (x) => {
        let regName =
          try {
            Printf.sprintf("%%%s%d", RegisterType.getPrefix(x.rType), int_of_string(x.id))
          } {
          | _ => x.id
          };
        `Declaration(
            StateSpace.Register
          , x.rType
          , regName
          , None)
      },
      RegisterSpecSet.elements(uniqueRegisters'))
  };

  let emit(t) = {
    let strings = List.map(Directive.emit, t);
    String.concat("\n", strings)
  };
};

let examplePtx = {
  let kernel = {
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
      ,*/ `Load(StateSpace.Parameter, {rType: U64, id: "0"}, `Dereference(`Label("paramX"), U64))
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
      , `Add({rType: U64, id: "2"}, `Register({rType: U64, id: "1"}), `Register({rType: U64, id: "3"}))
      , `Convert({rType: F64, id: "0"}, `Register({rType: U32, id: "3"}))
      /*, `Store(StateSpace.Global, `Dereference(`Register({rType: U64, id: 2}), U32), `Register({rType: U32, id: 3}))*/
      , `Store(StateSpace.Global, `Dereference(`Register({rType: U64, id: "2"}), F64), `FloatLiteral(69.0, F64))
      ];
    let declarations = Statement.declareRegisters(body);
    `Entry(kName, params, declarations, body)
  };
  open Statement.Directive;
  let whole =
    [ `Version{major: 4, minor: 3}
    , `Target("sm_20")
    , `AddressSize(64)
    , kernel
    ];
  let code = Statement.emit(whole);
  /*print_endline(code);*/
  code
};