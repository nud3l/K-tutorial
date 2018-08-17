module IMP-SYNTAX // syntax
  imports DOMAINS-SYNTAX
  syntax AExp  ::= Int | Id
                 | "-" Int
                 | AExp "/" AExp              [left, strict]
                 > AExp "+" AExp              [left, strict]
                 | "(" AExp ")"               [bracket]
  syntax BExp  ::= Bool
                 | AExp "<=" AExp             [seqstrict, latex({#1}\leq{#2})]
                 | "!" BExp                   [strict]
                 > BExp "&&" BExp             [left, strict(1)]
                 | "(" BExp ")"               [bracket]
  syntax Block ::= "{" "}"
                 | "{" Stmt "}"
  syntax Stmt  ::= Block
                 | Id "=" AExp ";"            [strict(2)]
                 | "if" "(" BExp ")"
                   Block "else" Block         [strict(1)]
                 | "while" "(" BExp ")" Block
                 > Stmt Stmt                  [left]
  syntax Pgm ::= "int" Ids ";" Stmt
  syntax Ids ::= List{Id,","}
endmodule


module IMP // semantics
  imports IMP-SYNTAX
  imports DOMAINS
  syntax KResult ::= Int | Bool // tells K which are valid results

  configuration <T color="yellow"> 
    <k color="green"> $PGM:Pgm </k> // built-in variable where parsed prgram is placed
    <state color="red"> .Map </state>
  </T>

  //AExp - arithmetic expressions
  rule <k> X:Id => I ...</k> <state>... X |-> I ...</state> //variables need to be defined before called
  rule I1 / I2 => I1 /Int I2 when I2 =/=Int 0
  rule I1 + I2 => I1 +Int I2
  rule - I1 => 0 -Int I1

  //BExp - boolean expressions
  rule I1 <= I2 => I1 <=Int I2
  rule ! T => notBool T
  rule true && B => B //why not BExp?
  rule false && _ => false // if first false, always false

  //Block - blocks of code
  //empty block
  rule {} => .    [structural] 
  // execute the statement
  rule {S} => S   [structural] 

  //Stmt - Statements defined in a block
  rule <k> X = I:Int; => . ...</k> <state>... X |-> (_ => I) ...</state> // assignment rule
  rule S1:Stmt S2:Stmt => S1 ~> S2  [structural] // execute S2 when S1 is done
  rule if (true) S else _ => S
  rule if (false) _ else S => S
  rule while (B) S => if (B) {S while (B) S} else {} [structural] // will not be unrolled

  // Pgm - defining the program with set of variables and the statement
  rule <k> int (X,Xs => Xs);_ </k> <state> Rho:Map (.Map => X|->0) </state>
    requires notBool (X in keys(Rho))
  rule int .Ids; S => S [structural] 
  
endmodule
