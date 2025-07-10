V1: TINY Interpreter, closely based on Robert D. Cameron's code.
V2: 
Developed by Aaron Win.

> import TINYParser

> data Value = Numeric Integer | Boolean Bool | ERROR
>              deriving Show

> data MemVal = Stored Value | Unbound
>               deriving Show

> type Memory = ([Ide], Ide -> MemVal)

> type Input = [Value]

V1:
type Output = [Value]

V1:
type State = (Memory, Input, Output)

> data ExpVal = OK Value State | Error

> data CmdVal = OKc State | Errorc

V1:
exp_semantics :: Exp -> State -> ExpVal
V3:
exp_semantics :: Exp -> Econt -> Cont

V1:
cmd_semantics :: Cmd -> State -> CmdVal
V3:
cmd_semantics :: Cmd -> Cont -> Cont

> display :: Memory -> String

> display (l, m) = concat [i ++ " = " ++ show (m i) ++ ", " | i <- l]

> isNum :: Value -> Bool

> isNum v = 
>   case v of 
>     Numeric v0 -> True
>     Boolean v0 -> False

> isBool v = 
>   case v of 
>     Boolean v0 -> True
>     Numeric v0 -> False

> condC :: ((State -> CmdVal), (State -> CmdVal)) -> Bool -> (State -> CmdVal)

> condC (a1, a2) b = if b then a1 else a2

> result :: Value -> State -> ExpVal

> result = \v -> \s -> 
>   case v of
>     ERROR -> Error
>     v1 -> OK v1 s

> donothing :: State -> CmdVal

> donothing = \s -> OKc s

> checkNum :: Value -> State -> ExpVal

> checkNum = \v -> \s -> if isNum v then OK v s else Error

> checkBool :: Value -> State -> ExpVal

> checkBool = \v -> \s -> if isBool v then OK v s else Error

V1:
exp_semantics Zero s = OK (Numeric 0) s
V2:
exp_semantics Zero s = result (Numeric 0) s
V3:
exp_semantics Zero k s = k (Numeric 0) s

V1:
exp_semantics One s = OK (Numeric 1) s
V2:
exp_semantics One s = result (Numeric 1) s
V3:
exp_semantics One k s = k (Numeric 1) s

V1:
exp_semantics TT s = OK (Boolean True) s
V2:
exp_semantics TT s = result (Boolean True) s
V3:
exp_semantics TT k s = k (Boolean True) s

V1:
exp_semantics FF s = OK (Boolean False) s
V2:
exp_semantics FF s = result (Boolean False) s
V3:
exp_semantics FF k s = k (Boolean False) s

V1:
exp_semantics Read (m, [], o) = error (display m ++ "Input: " ++ "[] " ++ "Output: " ++ show o)
 exp_semantics Read (m, (i:is), o) = OK i (m, is, o)
V3:
exp_semantics Read k s = 
  case s of 
    (m, [], o) -> Errorc
    (m, (i:is), o) -> k i (m, is, o)

V1:
exp_semantics (I ident) ((l, m), i, o) =
 case (m ident) of
   Stored v -> OK v ((l, m), i, o)
   Unbound -> error (display (l, m) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
V3:
exp_semantics (I ident) k s = 
  case (m ident) of 
    Stored v -> k v ((l, m), i, o)
    Unbound -> Errorc
  where ((l, m), i, o) = s

V1:
exp_semantics (Not exp) s =  
  case (exp_semantics exp s) of
     OK (Boolean v) s1 -> OK (Boolean (not v)) s1                             
     _ -> Error
V2:
exp_semantics (Not exp) s =
  case (exp_semantics exp s) of
    OK e s1 -> 
      case (checkBool e s1) of 
        OK (Boolean v) s2 -> result (Boolean (not v)) s2
        Error -> Error
    _ -> Error
V3:
exp_semantics (Not exp) k s = exp_semantics exp k0 s
                              where k0 = \v -> \s1 -> 
                                         case v of 
                                           Boolean v1 -> k (Boolean (not v1)) s1
                                           _ -> Errorc

V1:
exp_semantics (Equal exp1 exp2) s =
 case (exp_semantics exp1 s) of
   OK (Numeric v1) s1 -> 
     case (exp_semantics exp2 s1) of 
       OK (Numeric v2) s2 -> OK (Boolean (v1 == v2)) s2
       OK (Boolean v2) s2 -> OK (Boolean False) s2
     Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
              where (m,i,o) = s1
   OK (Boolean v1) s1 -> 
     case (exp_semantics exp2 s1) of 
       OK (Boolean v2) s2 -> OK (Boolean (v1 == v2)) s2
       OK (Numeric v2) s2 -> OK (Boolean False) s2
       Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
                where (m,i,o) = s1
   Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
            where (m,i,o) = s
V2:
exp_semantics (Equal exp1 exp2) s = 
  case (exp_semantics exp1 s) of 
    OK (Numeric v1) s1 -> 
      case (exp_semantics exp2 s1) of 
        OK (Numeric v2) s2 -> result (Boolean (v1 == v2)) s2
        OK (Boolean v2) s2 -> result (Boolean False) s2
        Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
                 where (m,i,o) = s
    OK (Boolean v1) s1 -> 
      case (exp_semantics exp2 s1) of 
        OK (Numeric v2) s2 -> result (Boolean False) s2
        OK (Boolean v2) s2 -> result (Boolean (v1 == v2)) s2
        Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
                 where (m,i,o) = s1
    Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
             where (m,i,o) = s
V3:
exp_semantics (Equal exp1 exp2) k s = exp_semantics exp1 (\v1 -> \s1 -> exp_semantics exp2 (\v2 -> \s2 -> k (f v1 v2) s2) s1) s
                                      where f = \v01 -> \v02 -> 
                                                case (v01, v02) of 
                                                  (Numeric v001, Numeric v002) -> Boolean (v001 == v002)
                                                  (Numeric v001, Boolean v002) -> Boolean False
                                                  (Boolean v001, Numeric v002) -> Boolean False
                                                  (Boolean v001, Boolean v002) -> Boolean (v001 == v002)
                                                  _ -> ERROR

V1:
exp_semantics (Plus exp1 exp2) s = 
   case (exp_semantics exp1 s) of
    OK (Numeric v1) s1 -> 
      case (exp_semantics exp2 s1) of 
        OK (Numeric v2) s2 -> OK (Numeric (v1 + v2)) s2
        OK (Boolean v2) s2 -> Error
        Error -> Error
    OK (Boolean v1) s1 -> Error
    Error -> Error
V2:
exp_semantics (Plus exp1 exp2) s = 
  case (exp_semantics exp1 s) of
    OK v1 s1 -> 
      case (checkNum v1 s1) of 
        OK (Numeric v01) s2 -> 
          case (exp_semantics exp2 s2) of
            OK v2 s3 -> 
              case (checkNum v2 s3) of 
                OK (Numeric v02) s4 -> result (Numeric (v01 + v02)) s4
                Error -> Error
            _ -> Error
        Error -> Error
    _ -> Error
V3:
exp_semantics (Plus exp1 exp2) k s = exp_semantics exp1 (\v1 -> \s1 -> exp_semantics exp2 (\v2 -> \s2 -> f v1 v2 s2) s1) s
                                     where f = \v01 -> \v02 -> \s3 -> 
                                               case (v01, v02) of 
                                                 (Numeric v001, Numeric v002) -> k (Numeric (v001 + v002)) s3
                                                 _ -> Errorc

> update m ide val = \ide2 -> if ide == ide2 then Stored val else m ide2

> emptyMem ide = Unbound

V1:
cmd_semantics (Assign ident exp) s =
  case (exp_semantics exp s) of
    OK v1 ((l, m1), i1, o1) -> OKc ((l, update m1 ident v1), i1, o1)
    Error -> Errorc
V3:
cmd_semantics (Assign ident exp) c s = exp_semantics exp (\v -> \((l, m), i, o) -> c ((l, update m ident v), i, o)) s

V1:
cmd_semantics (Output exp) s =
  case (exp_semantics exp s) of
    OK v1 (m1, i1, o1) -> OKc (m1, i1, o1 ++ [v1])
    Error -> Errorc
V3:
cmd_semantics (Output exp) c s = exp_semantics exp (\v -> \(m, i, o) -> c (m, i, o ++ [v])) s

V1:
cmd_semantics (IfThenElse exp cmd1 cmd2) s =
  case (exp_semantics exp s) of
    OK (Boolean True) s1 -> cmd_semantics cmd1 s1
    OK (Boolean False) s1 -> cmd_semantics cmd2 s1
    _ -> Errorc
V2:
cmd_semantics (IfThenElse exp cmd1 cmd2) s =
  case (exp_semantics exp s) of
    OK e s1 -> 
      case (checkBool e s1) of
        OK (Boolean v) s2 -> (cond (cmd_semantics cmd1, cmd_semantics cmd2) v) s2
        Error -> Errorc
    _ -> Errorc
V3:
cmd_semantics (IfThenElse exp cmd1 cmd2) c s = exp_semantics exp k s
                                               where k = \v -> \s0 -> 
                                                         case v of 
                                                           Boolean v1 -> cmd_semantics (if v1 then cmd1 else cmd2) c s0
                                                           _ -> Errorc

V1:
cmd_semantics (WhileDo exp cmd) s =
  case (exp_semantics exp s) of
    OK (Boolean True) s1 -> cmd_semantics (Seq cmd (WhileDo exp cmd)) s1
    OK (Boolean False) s1 -> OKc s1
    _ -> Errorc
V2:
cmd_semantics (WhileDo exp cmd) s =
  case (exp_semantics exp s) of
    OK e s1 -> 
      case (checkBool e s1) of
        OK (Boolean v) s2 -> (cond (cmd_semantics (Seq cmd (WhileDo exp cmd)), donothing) v) s2
        Error -> Errorc
    _ -> Errorc
V3:
cmd_semantics (WhileDo exp cmd) c s = exp_semantics exp k s
                                      where k = \v -> \s0 -> 
                                                case v of 
                                                  Boolean v1 -> (if v1 then cmd_semantics cmd (cmd_semantics (WhileDo exp cmd) c) else c) s0
                                                  _ -> Errorc

V1:
cmd_semantics (Seq cmd1 cmd2) s = 
  case (cmd_semantics cmd1 s) of 
    OKc s1 -> cmd_semantics cmd2 s1
    Errorc -> Errorc
V3:
cmd_semantics (Seq cmd1 cmd2) c s = cmd_semantics cmd1 (cmd_semantics cmd2 c) s

V1:
run program input = 
  case (cmd_semantics parsed_program (([], emptyMem), input, [])) of
    OKc (m, i, o) -> o
    Errorc -> [ERROR]
  where parsed_program = cparse program
V3:
run program input =
  case (cmd_semantics parsed_program (\s -> OKc s) (([], emptyMem), input, [])) of
    OKc (m, i, o) -> o
    Errorc -> [ERROR]
  where parsed_program = cparse program

V3:
type Cont = State -> CmdVal

> type Econt = Value -> Cont

> exp_semantics :: Exp -> Econt -> Cont

> cmd_semantics :: Cmd -> Cont -> Cont

> exp_semantics Zero k s = k (Numeric 0) s

> exp_semantics One k s = k (Numeric 1) s

> exp_semantics TT k s = k (Boolean True) s

> exp_semantics FF k s = k (Boolean False) s

> exp_semantics Read k s = 
>   case s of 
>     (m, []) -> Errora
>     (m, (i:is)) -> k i (m, is)

> exp_semantics (I ident) k s = 
>   case (m ident) of 
>     Stored v -> k v ((l, m), i)
>     Unbound -> Errora
>  where ((l, m), i) = s

> exp_semantics (Not exp) k s = exp_semantics exp k0 s
>                               where k0 = \v -> \s1 -> 
>                                          case v of 
>                                            Boolean v1 -> k (Boolean (not v1)) s1
>                                            _ -> Errora

> exp_semantics (Equal exp1 exp2) k s = exp_semantics exp1 (\v1 -> \s1 -> exp_semantics exp2 (\v2 -> \s2 -> k (f v1 v2) s2) s1) s
>                                       where f = \v01 -> \v02 -> 
>                                                  case (v01, v02) of 
>                                                    (Numeric v001, Numeric v002) -> Boolean (v001 == v002)
>                                                    (Numeric v001, Boolean v002) -> Boolean False
>                                                    (Boolean v001, Numeric v002) -> Boolean False
>                                                    (Boolean v001, Boolean v002) -> Boolean (v001 == v002)
>                                                    _ -> ERROR

> exp_semantics (Plus exp1 exp2) k s = exp_semantics exp1 (\v1 -> \s1 -> exp_semantics exp2 (\v2 -> \s2 -> f v1 v2 s2) s1) s
>                                      where f = \v01 -> \v02 -> \s3 -> 
>                                                case (v01, v02) of 
>                                                  (Numeric v001, Numeric v002) -> k (Numeric (v001 + v002)) s3
>                                                  _ -> Errora

> cmd_semantics (Assign ident exp) c s = exp_semantics exp (\v -> \((l, m), i) -> c ((l, update m ident v), i)) s

> cmd_semantics (Output exp) c s = exp_semantics exp (\v1 -> \s0 -> OKa v1 (c s0)) s

> cmd_semantics (IfThenElse exp cmd1 cmd2) c s = exp_semantics exp k s
>                                                where k = \v -> \s0 -> 
>                                                          case v of 
>                                                            Boolean v1 -> cmd_semantics (if v1 then cmd1 else cmd2) c s0
>                                                            _ -> Errora

> cmd_semantics (WhileDo exp cmd) c s = exp_semantics exp k s
>                                       where k = \v -> \s0 -> 
>                                                 case v of 
>                                                   Boolean v1 -> (if v1 then cmd_semantics cmd (cmd_semantics (WhileDo exp cmd) c) else c) s0
>                                                   _ -> Errora

> cmd_semantics (Seq cmd1 cmd2) c s = cmd_semantics cmd1 (cmd_semantics cmd2 c) s

> run program input = output (cmd_semantics parsed_program (\s -> Stop) (([], emptyMem), input))
>                     where
>                       parsed_program = cparse program
>                       output = \a -> 
>                         case a of 
>                           OKa v a0 -> "(" ++ show v ++ ") " ++ (output a0)
>                           a0 -> "(" ++ show a0 ++ ")"

> type State = (Memory, Input)

> type Cont = State -> Ans

> data Ans = OKa Value Ans | Stop | Errora
>            deriving Show

cmd_semantics (Assign ident exp) c s = exp_semantics exp (\v -> \((l, m), i) -> c ((l, update m ident v), i)) s

data Loc = L Ide

data Sv = SVF File | SVR Rv

data Dv = DVL Loc | DVR Rv | DVP Proc | DVF Fun

type Ev = Dv

data Rv = Boolean Bool | Basic Bv

type File = [Rv]

type Env = Ide -> EnvVal

data EnvVal = Denotable Dv | UNBOUND

type Store = Loc -> [StoreVal]

data StoreVal = Storable Sv | Unused

data newVal = New Loc | Errorn

new :: Store -> newVal

new = \s -> 

type Cc = Store -> Ans

type Ec = Ev -> Cc

type Dc = Env -> Cc

type Fun = Ec -> Ec

data Ans = OKa Rv Ans | Stop | Errora
           deriving Show

data fVal = Okf Ev Store | Errorf

mkconfun :: (Ev -> Store -> fVal) -> Ec -> Ec

mkconfun f = -> \k -> \e -> \s -> 
  case (f e s) of 
    OKf e0 s0 -> k e0 s0
    Errorf -> Errora

cont :: Ec -> Ec

cont = \k -> \e -> \s -> 
  case e of 
    Loc -> 
      case (s e) of 
        Storable sv -> k sv s
        Unused -> Errora
    _ -> Errora

update :: Loc -> Cc -> Ec

update = \l -> \c -> \e -> \s -> 
  case e of 
    Sv -> c (s[e/l]) = c (\d -> if d == l then e else s d)
    _ -> Errora

ref :: Ec -> Ec

ref = \k -> \e -> \s -> 
  case (new s) of 
    Error -> Error
    n -> update n (k n) e s

deref :: Ec -> Ec

deref = \k -> \e -> \s -> 
  case e of 
    Loc -> cont k e s
    _ -> k e s

checkD :: Ec -> Ec

checkD = \k -> \e -> \s -> if isD e then k e s else Errora

isLoc :: ? -> Bool

isLoc ? = 
  case ? of 
    Loc -> True
    _ -> False

exp_semantics :: Exp -> Env -> Ec -> Store -> Ans

cmd_semantics :: Cmd -> Env -> Cc -> Store -> Ans

dec_semantics :: Dec -> Env -> Dc -> Store -> Ans

lval_semantics :: Exp -> Env -> Ec -> Cc

rval_semantics :: Exp -> Env -> Ec -> Cc

exp_semantics Zero r k s = k (Numeric 0) s

exp_semantics One r k s = k (Numeric 1) s

exp_semantics TT r k s = k (Boolean True) s

exp_semantics FF r k s = k (Boolean False) s

cmd_semantics (Assign ide exp) r c s = lval_semantics ide r (\i -> rval_semantics exp r (update i c)) s

cmd_semantics (Seq cmd1 cmd2) r c s = cmd_semantics cmd1 r (cmd_semantics cmd2 r c) s

cmd_semantics (ide exp) r c = exp_semantics ide r (Proc? (\p -> exp_semantics exp r (p c)))

Const Ide Exp | Var Ide Exp | Proc Ide Ide Cmd | Fun Ide Ide Exp | Seqd Dec Dec

dec_semantics (Const ide exp) r u s = exp_semantics exp r (\e -> u(e/l) = (\d -> if d == l then e else u d)) s

dec_semantics (Var ide exp) r u s = 

dec_semantics (Proce ide1 ide2 cmd) r u s = u(p/ide1) s
                                           where p = \c -> \e -> cmd_semantics cmd r[e/ide2] (\d -> if d == ide2 then e else r d) c

dec_semantics (Func ide exp1 exp2) r u s = 

dec_semantics (Seqd dec1 dec2) r u s = dec_semantics dec1 r (dec_semantics dec2 r u) s

lval_semantics exp r k = exp_semantics exp r (Loc? k)

rval_semantics exp r k = exp_semantics exp r (deref k)




bas_semantics :: Bas -> Bv

opr_semantics :: (Rv, Rv) -> Ec -> Cc

pro_semantics :: Pro -> File -> Ans

rval_semantics :: Exp -> Env -> Ec -> Cc

exp_semantics :: Exp -> Env -> Ec -> Cc

cmd_semantics :: Cmd -> Env -> Cc -> Cc

dec_semantics :: Dec -> Env -> Dc -> Cc

opr_semantics opr (v1, v2) k s = 
  case opr of 
    ADD -> (v1 + v2)
    SUBTRACT -> (v1 - v2)
    MULTIPLY -> (v1 * v2)
    DIVIDE -> (v1 / v2)
    _ -> Errora

pro_semantics (Program cmd) i = cmd_semantics cmd (\ide -> UNBOUND) (\s -> Stop) ((i/input) = \l -> if l == input then input else Unused)

rval_semantics exp r k s = exp_semantics exp (r (deref (Rv? k))) s

exp_semantics (B bas) r k s = k (bas_semantics bas)

exp_semantics TT r k s = k (Boolean True) s

exp_semantics FF r k s = k (Boolean False) s

exp_semantics Read r k s = 

exp_semantics (I ide) r k s = 
  case (r l) of 
    UNBOUND -> Errora
    v -> k v

exp_semantics (FunE exp1 exp2) r k s = exp_semantics exp1 (r;Fun?;(\f -> exp_semantics exp2 r;f;k)) s

exp_semantics (IfThenElseE exp exp1 exp2) r k s = rval_semantics exp r;Bool?;cond (exp_semantics exp1 r k, exp_semantics exp2 r k) s

exp_semantics (Operate opr exp1 exp2) r k s = rval_semantics exp1 r (\e1 -> rval_semantics exp2 r (\e2 -> opr_semantics opr (e1, e2) k)) s

cmd_semantics (Assign exp1 exp2) r c s = exp_semantics exp1 r;Loc?(\l -> rval_semantics exp2 r;(update l);c) s

cmd_semantics (Output exp) r c s = rval_semantics exp r (\e -> \s0 -> (e, c s0)) s

cmd_semantics (ProcC exp1 exp2) r c s = exp_semantics exp r;Proc?(\p -> exp_semantics exp2 r;p;c) s

cmd_semantics (IfThenElseC exp cmd1 cmd2) r c s = rval_semantics exp r;Bool?cond(cmd_semantics cmd r c, cmd_semantics r c)

cmd_semantics (WhileDo exp cmd) r c s = rval_semantics exp r;Bool?;cond(cmd_semantics cmd r;(cmd_semantics (WhileDo exp cmd) r c), c) s

cmd_semantics (BeginEnd dec cmd) r c s = dec_semantics dec r (\r0 -> cmd_semantics cmd r[r0] c) s

cmd_semantics (SeqC cmd1 cmd2) r c s = cmd_semantics cmd1 r (cmd_semantics cmd2 r c) s

dec_semantics (Const ide exp) r u s = rval_semantics exp r (\e -> u(e/l)) s

dec_semantics (var ide exp) r u s = rval_semantics exp r;ref(\l -> u(l/ide)) s

dec_semantics (ProcD ide ide1 cmd) r u s = u ((\c -> \e -> cmd_semantics cmd r[e/ide1] c)/ide) s

dec_semantics (FunD ide ide1 exp) r u s = u ((\k -> \e -> exp_semantics exp r r[e/ide1] k)/ide) s

dec_semantics (SeqD dec1 dec2) r u s = dec_semantics dec1 r (\r1 -> dec_semantics dec2 r[r1] (\r2 -> u(r1[r2]))) s