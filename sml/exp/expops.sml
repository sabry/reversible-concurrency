structure ExpOps =
struct

  open TopUtils
  open Base

  datatype t =
    ENum of int
  | EBool of bool
  | EBinop of binop
  | EIf
  | EUnop of unop
  | ELam of tp
  | EApp
  | ENil
  | ECons
  | EListRec
  | ELet
  | ESeq
  | ESend of proc * tp
  | ERecv of proc * tp
  | EChoose of cont (* Choice over a list *)
  | EBack of cont
  
  val eq : t -> t -> bool = curry (op =)

  fun arity t =
    case t of
      ENum _ => #[]
    | EBool _ => #[]
    | EIf => #[0, 0, 0]
    | EBinop _ => #[0, 0]
    | EUnop _ => #[0]
    | ELam _ => #[1]
    | EApp => #[0, 0]
    | ENil => #[]
    | ECons => #[0, 0]
    | EListRec => #[0, 0, 2]
    | ELet => #[0, 1]
    | ESeq => #[0, 0]
    | ESend _ => #[0]
    | ERecv _ => #[]
    | EChoose _ => #[0]
    | EBack _ => #[]

  fun toString t =
    case t of
      ENum n => Int.toString n
    | EBool b => if b then "True" else "False"
    | EIf => "If"
    | EBinop bop => binopToString bop
    | EUnop uop => unopToString uop
    | ELam tp => "Lambda [" ^ typeToString tp ^ "]"
    | EApp => "App"
    | ENil => "Nil"
    | ECons => "Cons"
    | EListRec => "ListRec"
    | ELet => "Let"
    | ESeq => "Seq"
    | ESend _ => "Send"
    | ERecv _ => "Recv"
    | EChoose (Cont k) => "Choice (" ^ "k#" ^ (Int.toString k) ^ ")"
    | EBack (Cont k) => "Back (" ^ "k#" ^ (Int.toString k) ^ ")"

end
