{
module Lexer where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$char = $printable # \"

tokens :-

    -- Throw em in the trash!
    $white+                         ;
    "--".*                          ;
    -- BinOps
    \+                              { \s -> PlusT }
    \-                              { \s -> MinusT }
    \*                              { \s -> MultT }
    \/                              { \s -> DivT }
    \/\/                            { \s -> DivIntT }
    \^                              { \s -> ExpT }
    \%                              { \s -> ModT }
    -- Comparison BinOps
    \<                              { \s -> LessT }
    \<\=                            { \s -> LessEqT }
    \>                              { \s -> GreaterT }
    \>\=                            { \s -> GreaterEqT }
    \!\=                            { \s -> NotEqT }
    \=                              { \s -> EqT }
    -- UnOps
    \~                              { \s -> RoundT }
    -- Functions
    \-\>                            { \s -> ArrowT }
    pun                             { \s -> PunT }
    punction                        { \s -> PunT }
    -- Constants
    life                            { \s -> CLifeT }
    pi                              { \s -> CPiT }
    fogarte                         { \s -> CFogarteT }
    tau                             { \s -> CTauT }
    -- Bool ops
    not                             { \s -> NotT }
    both                            { \s -> BothT }
    one                             { \s -> OneT }
    -- Parens
    \(                              { \s -> LParenT }
    \)                              { \s -> RParenT }
    -- Pairs
    stump                           { \s -> StumpT }
    \,                              { \s -> CommaT }
    \;                              { \s -> SemicolonT }
    fst                             { \s -> FstT }
    snd                             { \s -> SndT }
    -- Conditionals
    if                              { \s -> IfT }
    then                            { \s -> ThenT }
    else                            { \s -> ElseT }
    -- Variables
    sir                             { \s -> SirT }
    dame                            { \s -> DameT }
    the                             { \s -> TheT }
    lord                            { \s -> LordT }
    of                              { \s -> OfT }
    \?                              { \s -> TermT }
    -- References
    fae                             { \s -> RefT }
    \#                              { \s -> DerefT }
    steals                          { \s -> MutT }
    -- Sequences
    \|                              { \s -> SeqT }
    -- While loops
    calm                            { \s -> WhileT }
    maeb                            { \s -> DoT }
    -- Types
    $digit*\.$digit+                { \s -> RealT (read s) }
    $digit+                         { \s -> IntT (read s) }
    \" $char* \"                    { \s -> StringT (fromQuotes s) }
    [kK]night                       { \s -> BoolT True }
    [kK]nave                        { \s -> BoolT False }
    ([a-z][a-zA-Z0-9'_]*)+          { \s -> VarT s }

{

data Token =
    IntT Int | RealT Float |                                      -- num types
    PlusT | MinusT | MultT | DivT | DivIntT | ModT | ExpT |       -- binary num ops
    RoundT |                                                      -- unary num ops
    StringT String |                                              -- char types
    BoolT Bool |                                                  -- bool types
    NotT | BothT |                                                -- bool ops
    LessT | LessEqT | GreaterT | GreaterEqT | EqT | NotEqT |      -- binary comparison ops
    OneT |                                                        -- unary bool ops
    CLifeT | CPiT | CTauT | CFogarteT |                           -- constants
    LParenT | RParenT |                                           -- parens
    StumpT |                                                      -- unit values
    CommaT | SemicolonT | FstT | SndT |                           -- pairs
    ArrowT | PunT |                                               -- functions
    IfT | ThenT | ElseT |                                         -- conditionals
    VarT String | SirT | DameT | TheT | LordT | OfT |             -- variables
    RefT | DerefT | MutT |                                        -- references
    WhileT | DoT |                                                -- while loops
    SeqT |                                                        -- sequences
    TermT                                                         -- line terminators
    deriving (Eq,Show)

-- Each action has type :: String -> Token

scanTokens :: String -> Maybe [Token]
scanTokens str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
                AlexEOF -> Just []
                AlexError _ -> Nothing
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap ((act (take len str)):) (go inp')

fromQuotes :: [Char] -> [Char]
fromQuotes str = case (take 1 str, drop (length str - 1) str) of
    ("\"", "\"") -> init $ tail str
    otherwise -> error "String without quotes passed into fromQuotes"

}