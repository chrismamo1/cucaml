open Ast_convenience;
open Ast_mapper;
open Ast_helper;
open Asttypes;
open Parsetree;
open Location;
open Longident;

let programIds: ref(list((string, int))) = ref([]);

let addProgram(src) = {
  let hash = Hashtbl.hash(src);
  let id =
    try (List.assoc(src, programIds.contents)) {
    | Not_found => Hashtbl.hash(src)
    };
  let isHashCollision =
    List.fold_left(
      (acc, (src, id')) => (id == id' || acc),
      false,
      programIds.contents);
  if (isHashCollision) {
    let id = id + 1;
    programIds := [(src, id), ...programIds.contents] /* TODO: handle multiple collisions */
  } else {
    programIds := [(src, id), ...programIds.contents]
  }
};

let remapper(argv) =
{ ...default_mapper,
  expr: (mapper, expr) => {
    switch(expr) {
    | ({pexp_desc: Pexp_constant(Pconst_string(src, Some("cucamlcl")))} as expr) =>
        let ptx = AsmGen.compileProgram(src, []);
        { ...expr, pexp_desc: Pexp_constant(Pconst_string(ptx, None)) }
    | x => default_mapper.expr(mapper, x);
    }
  }
};

register("ppx_cucaml", remapper);