module RegisterSpace = {
  module U32 = {
    type z =
      | Z;
    type s('n) =
      | S('n): s('n);
  };
  module U64 = {
    type z =
      | Z;
    type s('n) =
      | S('n): s('n);
  };
  type z =
    | Z;
  type t('n) =
    | U64('n): t(U64.s('n))
    | U32('n): t(U32.s('n));
  let firstU64 = U64(Z);
};

module RegisterType = {
  type u32;
  type u64;
  type t = [ | `u32 | `u64];
  let toString = (t) =>
    switch t {
    | `u32 => "u32"
    | `u64 => "u64"
    };
};

module RegisterSpec = {
  type t = {
    name: string,
    t: RegisterType.t
  };
  let emit = (t) => "%" ++ t.name;
};

module OperandSpec = {
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
        ".reg ." ++ (RegisterType.toString(regType) ++ (" %" ++ (name ++ ("<" ++ (sz ++ ">")))))
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
    /* Sequence(lst, fst) is a special constructor for a block of unary
        instructions. E.g.
           Sequence(
               ShiftLeft(
                   {name: "rd1", t: U64},
                   {name: "rd0", t: U64},
                   32),
               Sequence(
                   Convert(
                       {name: "rd0", t: U64},
                       {name: "r0", t: U32}),
                   Nop))
       should have the type [t (RegisterType.t u64) (RegisterType.t u32)]
       and should emit the assembly code
           cvt.u64.u32 %rd0, r0;
           shl.u64     %rd1, %rd0, 32; */
    /*| Nop: t 'dst 'src
      | Sequence (t 'dst 'src, t 'src 'src): t (t 'dst 'src) (t 'src 'src);*/
    let rec emit = (t) =>
      switch t {
      | Convert((dst, src)) =>
        open RegisterType;
        let instr =
          "cvt." ++ (RegisterType.toString(dst.t) ++ ("." ++ RegisterType.toString(src.t)));
        RegisterSpec.(instr ++ (" %" ++ (dst.name ++ (" %" ++ src.name))))
      | ShiftRight((dst, src, n)) =>
        open RegisterType;
        let instr = "shr." ++ RegisterType.toString(dst.t);
        let n = string_of_int(n);
        RegisterSpec.(instr ++ (" %" ++ (dst.name ++ (", %" ++ (src.name ++ (", " ++ n))))))
      | ShiftLeft((dst, src, n)) =>
        open RegisterType;
        let instr = "shl." ++ RegisterType.toString(dst.t);
        let n = string_of_int(n);
        RegisterSpec.(instr ++ (" %" ++ (dst.name ++ (", %" ++ (src.name ++ (", " ++ n))))))
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

/*module Declaration = {
    type t =
      | Kernel(string, list (RegisterType.t 'a, string), list (Operation.t 'b)): t;

    let emit t =>
      switch t {
      | Kernel(name, params, operations) =>
          let params = {
            let stringifyParameter (t, n) => {
              let t = RegisterType.toString t;
              ".param ." ^ t ^ " _Z6" ^ name ^ "Pic_param_" ^ n
            };
            List.fold_left
              (fun acc p => acc ^ ",\n\t" ^ stringifyParameter p)
              ("\t" ^ stringifyParameter (List.hd params))
              (List.tl params)
          };
          ".visible .entry _Z6" ^ name ^ "pic(\n" ^ params ^ ")\n{\n" ^ operations ^ "\n}"
      };
  };*/
/*type t = list Operation.t;*/
type reg('t) = string;

type t('dst, 'src) =
  | Convert((reg('dst), reg('src)))
  /*| ShiftRight(reg 'dst, reg 'src, int)
    | ShiftLeft(reg 'dst, reg 'src, int)*/
  | Sequence((t('dst, 'a), t('a, 'src)));

let rec emit = (t) =>
  switch t {
  | Convert((dst, src)) => "convert " ++ (dst ++ (", " ++ src))
  | Sequence((cur, prev)) =>
    let prev = emit(prev);
    let cur = emit(cur);
    "Hello, world!"
  };
