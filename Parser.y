{
module Parser where
import Eval
import Lexer
}

%monad { Maybe } 
%name parse
%tokentype { Token }
%error { parseError }

%token

    -- Types
	int         { IntT $$ }
	real        { RealT $$ }
    bool        { BoolT $$ }
    string      { StringT $$ }
    var         { VarT $$ }
    stump       { StumpT }
    -- Pairs
    ','         { CommaT }
    ';'         { SemicolonT }
    fst         { FstT }
    snd         { SndT }
    -- Functions
    '->'        { ArrowT }
    pun         { PunT }
    -- Consts
    life        { CLifeT }
    pi          { CPiT }
    tau         { CTauT }
    fogarte     { CFogarteT }
    -- Arithmetic BinOps
	'+'         { PlusT } 
	'-'         { MinusT } -- also an Arithmetic UnOp
	'*'         { MultT }
	'/'         { DivT }
	'//'        { DivIntT }
	'%'         { ModT }
	'^'         { ExpT }
    -- Arithmatic UnOps
    '~'         { RoundT }
    -- Comparison BinOps
    '<'         { LessT }
    '<='        { LessEqT }
    '>'         { GreaterT }
    '>='        { GreaterEqT }
    '!='        { NotEqT }
    '='         { EqT }
    -- Bool ops
    notE        { NotT }
    bothE       { BothT }
    oneE        { OneT }
    -- Parens
	'('         { LParenT }
	')'         { RParenT }
    -- Conditionals
	ifE          { IfT }
	thenE        { ThenT }
	elseE        { ElseT }
    -- Variable declarations
    sir         { SirT }
    dame        { DameT }
    the         { TheT }
    lord        { LordT }
    ofE         { OfT }
    -- References
    fae         { RefT }
    '#'         { DerefT }
    steals      { MutT }
    -- Sequences
    '|'         { SeqT }
    -- While loops
    calm        { WhileT }
    maeb        { DoT }    
    -- Terminators
	'?'         { TermT }

%right '->'
%right IF
%right ofE
%right '|'
%right calm maeb
%right steals
%right oneE bothE notE
%right '<' '<=' '>' '>=' '!=' '='
%left '+' '-'
%left '*' '/' '//' '%'
%right '^'
%right fst snd
%right fae '#'
%nonassoc NEG ROUND COND NOT
%right var
%left '(' ')' ',' ';'
%left APP

%%

P : S                               { $1 }
  | S P                             { $1 }

S : E '?'                           { Ex $1 }
  | sir var the E '?'               { GlobalV $2 $4 }
  | dame var the E '?'              { RecurV $2 $4 }

E : int                             { Int $1 }
  | real                            { Real $1 }
  | string                          { String $1 }
  | bool                            { Bool $1 }
  | var                             { Var $1 }
  | stump                           { Unit }
  -- Functions
  | pun var '->' E                  { Func $2 $4 }
  | E '(' E ')' %prec APP           { App $1 $3 }
  -- Pairs  
  | '(' E ',' E ')'                 { Pair $2 $4 }
  | '(' E ';' EListSemi ')'         { Pair $2 $4 }
  -- Consts
  | life                            { Const LifeC }
  | pi                              { Const PiC }
  | tau                             { Const TauC }
  | fogarte                         { Const FogarteC }
  -- Arithmatic BinOps
  | E '+' E                         { BinOp plusV $1 $3 }
  | E '-' E                         { BinOp minusV $1 $3 }
  | E '*' E                         { BinOp multV $1 $3 }
  | E '/' E                         { BinOp divV $1 $3 }
  | E '//' E                        { BinOp divIntV $1 $3 }
  | E '%' E                         { BinOp modV $1 $3 }
  | E '^' E                         { BinOp expV $1 $3 }
  -- Arithmatic UnOps
  | '~' E %prec ROUND               { UnOp roundV $2 }
  | '-' E %prec NEG                 { UnOp negateV $2}
  -- Bool Ops
  | E oneE E                        { OneOp $1 $3 }
  | E bothE E                       { BothOp $1 $3 }
  | notE E                          { NotOp $2 }
  -- Comparison Ops
  | E '<' E                         { CompOp (<) $1 $3 }
  | E '<=' E                        { CompOp (<=) $1 $3 }
  | E '>' E                         { CompOp (>) $1 $3 }
  | E '>=' E                        { CompOp (>=) $1 $3 }
  | E '!=' E                        { NEqOp $1 $3 }
  | E '=' E                         { EqOp $1 $3 }
  -- Pair Ops
  | fst E                           { PairOp fstP $2 }
  | snd E                           { PairOp sndP $2 }
  -- Parens
  | '(' E ')'                       { Expr $2 }
  -- Conditionals
  | ifE E thenE E elseE E %prec IF  { Cond $2 $4 $6 }
  -- Variable declarations
  | sir var the E lord ofE E        { LocalV $2 $4 $7 }
  | sir var the E ofE E             { LocalV $2 $4 $6 }
  -- References
  | fae E                           { Ref $2 }
  | '#' E                           { Deref $2 }
  | E steals E                      { Mut $1 $3 }
  -- Sequences
  | E '|' E                         { Seq $1 $3 }
  -- Do-while
  | calm E maeb E                   { Loop $2 $4 }

-- Better list declarations
EListSemi : E ';' EListSemi         { Pair $1 $3 }
          | E                       { Pair $1 Unit }


{

parseError :: [Token] -> Maybe a
parseError _ = Nothing

}