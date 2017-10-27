module RegisterType = {
  type u32;
  type u64;

  type t 'phantom =
    | U32: t u32
    | U64: t u64;

  type enum =
    [ `u32 | `u64 ];

  let toString (type a) (t: t a) =>
    switch t {
    | U32 => "u32"
    | U64 => "u64"
    };
};

module RegisterSpec = {
  type t 't = {
      name: string,
      t: RegisterType.t 't
  };
};

module Operation = {
  type fromToSpec 'dst 'src = {
      src: RegisterType.t 'src,
      dst: RegisterType.t 'dst
  };

  module PseudoOperation = {
    type t 'a =
      | RegisterSpaceDeclaration(RegisterType.t 'a, int, string): t 'a
      | AddressSizeDeclaration(int): t int;
    let emit (type a) (t: t a) =>
      switch t {
      | RegisterSpaceDeclaration(regType, sz, name) =>
          let sz = string_of_int sz;
          ".reg ." ^ (RegisterType.toString regType) ^ " %" ^ name ^ "<" ^ sz ^ ">"
      | AddressSizeDeclaration(n) =>
          let n = string_of_int n;
          ".address_size " ^ n
      };
  };

  /* an operation that operates on 1 register and stores its result in another,
     but may take other non-register arguments */
  module UnaryOperation = {
    /* keep in mind that PTX assembly syntax is [instr.dstType.srcType dst, src] */
    type t 'dst 'src =
      | Convert (RegisterSpec.t 'dst, RegisterSpec.t 'src)
      | ShiftRight (RegisterSpec.t 'dst, RegisterSpec.t 'dst, int);

    let emit t =>
      switch t {
      | Convert(dst, src) =>
          let open RegisterType;
          let instr = "cvt." ^ (RegisterType.toString dst.t) ^ "." ^ (RegisterType.toString src.t);
          let open RegisterSpec;
          instr ^ " %" ^ dst.name ^ " %" ^ src.name
      | ShiftRight(dst, src, n) =>
          let open RegisterType;
          let instr = "shr." ^ (RegisterType.toString dst.t);
          let n = string_of_int n;
          let open RegisterSpec;
          instr ^ " %" ^ dst.name ^ ", %" ^ src.name ^ ", " ^ n
      };
  };

  type t 'a =
    | Pseudo(PseudoOperation.t 'a): t 'a
    | Unary(UnaryOperation.t 'dst 'src): t 'a;

  let emit (type a) (t: t a) =>
    switch t {
    | Pseudo(x: PseudoOperation.t a) => PseudoOperation.emit x
    | Unary(x) => UnaryOperation.emit x
    }
};

module Declaration = {
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
};

type t = list Operation.t;