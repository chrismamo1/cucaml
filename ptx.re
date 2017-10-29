/*module RegisterType = {
  type u32;
  type u64;
  type t(_) =
    | U32: t(u32)
    | U64: t(u64);

  let generateName = {
    let u32Counter = ref(0);
    let u64Counter = ref(0);
    let rv: type a. t(a) => string = (rType) =>
      switch rType {
      | U32 =>
          incr(u32Counter);
          Printf.sprintf("r%d", u32Counter^ - 1)
      | U64 =>
          incr(u64Counter);
          Printf.sprintf("rd%d", u64Counter^ - 1)
      };
    rv
  };

  let toString: type a. t(a) => string = (t) =>
    switch t {
    | U32 => "u32"
    | U64 => "u64"
    };
};*/

module RegisterSpec = {
  type u32;
  type u64;

  type rType(_) =
    | U32: rType(u32)
    | U64: rType(u64);

  type t('phantom) = {
    name: string,
    t: rType('phantom)
  };
  let emit: type a. t(a) => string = (t) => { let name = t.name; name };
};

module Literal = {
  type t('phantom) =
    | Value(int): t('phantom);
  
  let toString: type a. t(a) => string = (t) => {
    let s = switch t {
    | Value(n) => n;
    };
    string_of_int(s)
  };
};

/*module UnaryOperation = {
  /* keep in mind that PTX assembly syntax is [instr.dstType.srcType dst, src] */
  type t('dst, 'src) =
    | Convert((RegisterSpec.t('dst), RegisterSpec.t('src)))
    | ShiftRight((RegisterSpec.t('dst), RegisterSpec.t('src), int))
    | ShiftLeft((RegisterSpec.t('dst), RegisterSpec.t('src), int));
  let rec emit = (t) =>
    switch t {
    | Convert((dst, src)) =>
      open RegisterType;
      let instr = Printf.sprintf("cvt.%s.%s", RegisterType.toString(dst.t), RegisterType.toString(src.t));
      open RegisterSpec;
      Printf.sprintf("%s %s,%s;", instr, dst.name, src.name)
    | ShiftRight((dst, src, n)) =>
      open RegisterType;
      open RegisterSpec;
      let instr = Printf.sprintf("shr.%s", RegisterType.toString(dst.t));
      Printf.sprintf("%s %s,%s,%d", instr, dst.name, src.name, n);
    | ShiftLeft((dst, src, n)) =>
      open RegisterType;
      open RegisterSpec;
      let instr = Printf.sprintf("shr.%s", RegisterType.toString(dst.t));
      Printf.sprintf("%s %s,%s,%d", instr, dst.name, src.name, n);
    };
};*/

type t('phantom) =
  | Register(string, RegisterSpec.rType('phantom)): t(RegisterSpec.t('phantom))
  | Literal('phantom): t('phantom)
  /*| UnaryOperation(t('phantom), t('phantom));*/
  | Convert(RegisterSpec.rType('phantom), t(_)): t(RegisterSpec.t('phantom));

let test1 = Register("r", RegisterSpec.U32);

let rec emit: type phantom. t(phantom) => phantom = (t) =>
  switch t {
  | Register(name, rType) =>
      /*switch rType {
      | U32 => 
      | U64 => RegisterSpec.{name: "rd", t: rType}
      };*/
      RegisterSpec.{name: "r", t: rType}
  | Literal(value) => value
  | Convert(rtype, src) =>
      let x = emit(src);
      /*let rv: type phantom'. t(phantom') => string = (src) => {
        Printf.sprintf("cvt %s,%s;", emit(dst), emit(src))
      };*/
      RegisterSpec.{name: "rd", t: rtype}
  | _ => raise(Failure("Invalid parameters"));
  };

let test2 = Convert(RegisterSpec.U64, test1);

let test3 = emit(test1);
/*module OperandSpec = {
    type t =
      | Register(RegisterSpec.t)
      | Literal(int);
    let emit = (t) =>
      switch t {
      | Register(spec) => RegisterSpec.emit(spec)
      | Literal(n) => string_of_int(n)
      };
  };

  module Operation = {
    module PseudoOperation = {
      type t =
        | RegisterSpaceDeclaration((RegisterType.t, int, string))
        | AddressSizeDeclaration(int);
      let emit = (t) =>
        switch t {
        | RegisterSpaceDeclaration((regType, sz, name)) =>
          let sz = string_of_int(sz);
          ".reg ."
          ++ (
            RegisterType.toString(regType)
            ++ (" %" ++ (name ++ ("<" ++ (sz ++ ">"))))
          )
        | AddressSizeDeclaration(n) =>
          let n = string_of_int(n);
          ".address_size " ++ n
        };
    };
    /* an operation that operates on 1 register and stores its result in another,
       but may take other non-register arguments */
    module UnaryOperation = {
      /* keep in mind that PTX assembly syntax is [instr.dstType.srcType dst, src] */
      type t =
        | Convert((RegisterSpec.t, RegisterSpec.t))
        | ShiftRight((RegisterSpec.t, RegisterSpec.t, int))
        | ShiftLeft((RegisterSpec.t, RegisterSpec.t, int));
      let rec emit = (t) =>
        switch t {
        | Convert((dst, src)) =>
          open RegisterType;
          let instr =
            "cvt."
            ++ (
              RegisterType.toString(dst.t)
              ++ ("." ++ RegisterType.toString(src.t))
            );
          RegisterSpec.(instr ++ (" %" ++ (dst.name ++ (" %" ++ src.name))))
        | ShiftRight((dst, src, n)) =>
          open RegisterType;
          let instr = "shr." ++ RegisterType.toString(dst.t);
          let n = string_of_int(n);
          RegisterSpec.(
            instr ++ (" %" ++ (dst.name ++ (", %" ++ (src.name ++ (", " ++ n)))))
          )
        | ShiftLeft((dst, src, n)) =>
          open RegisterType;
          let instr = "shl." ++ RegisterType.toString(dst.t);
          let n = string_of_int(n);
          RegisterSpec.(
            instr ++ (" %" ++ (dst.name ++ (", %" ++ (src.name ++ (", " ++ n)))))
          )
        };
    };
    type t =
      | Pseudo(PseudoOperation.t)
      | Unary(UnaryOperation.t);
    let emit = (t) =>
      switch t {
      | Pseudo(x) => PseudoOperation.emit(x)
      | Unary(x) => UnaryOperation.emit(x)
      };
  };
  /*type reg('t) = string;

    type t('dst, 'src) =
      | Convert((reg('dst), reg('src)))
      | Sequence((t('dst, 'a), t('a, 'src)));

    let rec emit = (t) =>
      switch t {
      | Convert((dst, src)) => "convert " ++ (dst ++ (", " ++ src))
      | Sequence((cur, prev)) =>
        let prev = emit(prev);
        let cur = emit(cur);
        "Hello, world!"
      };*/*/
