(* Contains all the stuff that isn't being done with the ABT library *)

structure Base =
struct

  datatype proc = Proc of int (* >= 0 *)
  datatype tp = TInt | TArr of tp * tp | TBool | TList of tp
  datatype binop = BPlus | BTimes | BAnd | BOr | BLeq | BLt
  datatype unop  = UNeg | UNot

  fun typeToString tp =
    case tp of
      TInt => "int"
    | TArr (t1, t2) =>
        "(" ^ (typeToString t1) ^ ") -> (" ^ (typeToString t2) ^ ")"
    | TBool => "bool"
    | TList t => "(" ^ (typeToString t) ^ ") list"

  fun binopToString bop =
    case bop of
      BPlus => "+"
    | BTimes => "*"
    | BAnd => "and"
    | BOr => "or"
    | BLeq => "<="
    | BLt => "<"

  fun unopToString uop =
    case uop of
      UNeg => "~"
    | UNot => "not"

end
