{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module UniversalParser where

import Data.Functor (($>), void)
import Data.Maybe (fromMaybe)

import Text.Parsec (satisfy, (<|>), (<?>), choice, try, unexpected, lookAhead, char, notFollowedBy, string)
import Text.Parsec.String (Parser)

import ParsingHelpers

{-- Universal parser for the following grammar of propositional formulas:
  Formula ::= NoFixity | BiImpl

  -- no implicit parenthesis
  NoFixity ::= Basic Op Basic | Basic
  Basic ::= Nested | Neg
  Op ::= ∨ | ∧ | => | <=>

  -- with precedence
  BiImpl ::= Impl <=> BiImpl | Impl
  Impl ::= Ors => Impl | Ors <= Impl | Ors
  Ors ::= Ors ∨ Ands | Ands
  Ands ::= Ands ∧ Neg | Neg

  -- 'leaf' formulas
  Neg ::= ¬ Atom | Atom | ¬ Neg | Nested
  Nested ::= ( Formula )
  Atom ::= A | .. | Z

  Individual grammar productions can be toggled off (at specific nesting depths)
  to parse specialized subsets of formulas.
--}

-- types for universal grammar
data FormulaGrammar = WithPrecedence BiImpls | OfNoFixity NoFixity
  deriving Show

data NoFixity = NoFixity Basic Op Basic | OfBasic Basic
  deriving Show

data Basic = BasicNested Nested | BasicNeg Neg
  deriving Show

data Op = Or | And | Impl | BackImpl | BiImpl
  deriving Show

data BiImpls = BiImpls Impls BiImpls | OfImpls Impls
  deriving Show

data Impls
  = Impls Ors Impls
  | BackImpls Ors Impls
  | OfOrs Ors
  deriving Show

data Ors = Ors Ors Ands | OfAnds Ands
  deriving Show

data Ands = Ands Ands Neg | OfNeg Neg
  deriving Show

data Neg = NegAtom Atom | OfAtom Atom | Neg Neg | OfNested Nested
  deriving Show

newtype Atom = Atom Char
  deriving Show

newtype Nested = Nested FormulaGrammar
  deriving Show

-- folds over binary operators
foldlOrs :: (a -> Ands -> a) -> a -> Ors -> a
foldlOrs f z (Ors x y) = foldlOrs f z x `f` y
foldlOrs f z (OfAnds x) = f z x

foldlAnds :: (a -> Neg -> a) -> a -> Ands -> a
foldlAnds f z (Ands x y) = foldlAnds f z x `f` y
foldlAnds f z (OfNeg x) = f z x

foldrImpl :: (Ors -> a -> a) -> a -> Impls -> a
foldrImpl f z (Impls x y) = f x $ foldrImpl f z y
foldrImpl f z (BackImpls x y) = f x $ foldrImpl f z y
foldrImpl f z (OfOrs x) = f x z

foldrBiImpl :: (Impls -> a -> a) -> a -> BiImpls -> a
foldrBiImpl f z (BiImpls x y) = f x $ foldrBiImpl f z y
foldrBiImpl f z (OfImpls x) = f x z

-- useful pattern synonyms
{-# COMPLETE Ors, NoOrs #-}
pattern NoOrs :: Ands -> Ors
pattern NoOrs x = OfAnds x

{-# COMPLETE Ands, NoAnds #-}
pattern NoAnds :: Neg -> Ands
pattern NoAnds x = OfNeg x

{-# COMPLETE Impls, NoImpls #-}
pattern NoImpls :: Ors -> Impls
pattern NoImpls x = OfOrs x

{-# COMPLETE BiImpls, NoBiImpls #-}
pattern NoBiImpls :: Impls -> BiImpls
pattern NoBiImpls x = OfImpls x

{-# COMPLETE BiImpls, TopLevelImpl, TopLevelBackImpl, NoArrows #-}
pattern TopLevelImpl :: BiImpls
pattern TopLevelImpl <- OfImpls Impls{}

pattern TopLevelBackImpl :: BiImpls
pattern TopLevelBackImpl <- OfImpls BackImpls{}

pattern NoArrows :: Ors -> BiImpls
pattern NoArrows x = NoBiImpls (NoImpls x)

pattern SkipLevel :: FormulaGrammar -> BiImpls
pattern SkipLevel x = NoArrows (NoOrs (NoAnds (OfNested (Nested x))))

-- parser configuration
data LevelSpec = LevelSpec
  { allowOr :: Bool
  , allowAnd :: Bool
  , allowNegation :: AllowNegation
  , allowAtomicProps :: Bool
  , allowImplication :: AllowImplication
  , allowBiImplication :: Bool
  , strictParens :: Bool -- strict parenthesis will trigger excessive nesting and thereby level changes!!!
  , allowSilentNesting :: Bool -- DANGER!!!
  , nextLevelSpec :: Maybe LevelSpec
  }

data AllowImplication = NoImplication | Forwards | Backwards | Both deriving Eq
data AllowNegation = Nowhere | LiteralsOnly | Everywhere

-- parser for operations
orParser :: Parser ()
orParser =
      keyword "or"
  <|> keyword "oder"
  <|> tokenSymbol "∨" <|> tokenSymbol "v" <|> tokenSymbol "\\/"
  <?> "Disjunction"

andParser :: Parser ()
andParser =
      keyword "and"
  <|> keyword "und"
  <|> tokenSymbol "∧" <|> tokenSymbol "^" <|> tokenSymbol "/\\"
  <?> "Conjunction"

implicationParser :: Parser ()
implicationParser = tokenSymbol "=>" <?> "Implication"

backImplicationParser :: Parser ()
backImplicationParser = token (void (string "<=") <* notFollowedBy (char '>')) <?> "(Back-)Implication"

biImplicationParser :: Parser ()
biImplicationParser =
      tokenSymbol "<=>"
  <|> keyword "iff" <|> keyword "gdw" <|> keyword "⇔"
  <?> "BiImplication"

negationParser :: Parser ()
negationParser =
      keyword "not" <|> keyword "nicht"
  <|> tokenSymbol "¬" <|> tokenSymbol "~" <|> tokenSymbol "-" <|> tokenSymbol "￢"
  <?> "Negation"

atomParser :: Parser Char
atomParser = token (satisfy (`elem` ['A'..'Z'])) <?> "atomic formula"

-- parser for individual tokens
logicToken :: Parser ()
logicToken =
      orParser
  <|> andParser
  <|> implicationParser
  <|> backImplicationParser
  <|> biImplicationParser
  <|> negationParser
  <|> void atomParser
  <|> tokenSymbol "("
  <|> tokenSymbol ")"

-- the universal parser
formula :: LevelSpec -> Parser FormulaGrammar
formula LevelSpec{..}
  | strictParens = OfNoFixity <$> noFixity
  | otherwise = WithPrecedence <$> biImpl
  where
  noFixity :: Parser NoFixity
  noFixity = do
    x <- basic
    NoFixity x <$> anyBinaryOp <*> basic <|> pure (OfBasic x)

  anyBinaryOp :: Parser Op
  anyBinaryOp = _noNested *> chooseBinary <* _noSecondOp
    where
    chooseBinary :: Parser Op
    chooseBinary = choice
      (  [ Or <$ orParser | allowOr ]
      ++ [ And <$ andParser | allowAnd ]
      ++ [ Impl <$ implicationParser | allowImplication == Forwards || allowImplication == Both  ]
      ++ [ BackImpl <$ backImplicationParser | allowImplication == Backwards || allowImplication == Both ]
      ++ [ BiImpl <$ biImplicationParser | allowBiImplication ]
      )

    _noNested :: Parser () -- catch the A (B and C) error pattern
    _noNested = (do
      lookAhead $ char '('
      _ <- try nested
      unexpected "formula"
      fail "maybe there is an operator missing")
      <|> pure ()

    _noSecondOp :: Parser ()
    _noSecondOp = (do
      op <- try chooseBinary
      unexpected $ "second operator: " ++ case op of
        Or -> "Disjunction"
        And -> "Conjunction"
        Impl -> "Implication"
        BackImpl -> "(Back)-Implication"
        BiImpl -> "Bi-Implication")
      <|> pure ()

  basic :: Parser Basic
  basic = BasicNested <$> nested <|> BasicNeg <$> neg

  impl :: Parser Impls
  impl
    = case allowImplication of
      Forwards -> infixr1 OfOrs ors (implicationParser $> Impls)
      Backwards -> infixr1 OfOrs ors (try backImplicationParser $> BackImpls)
      Both -> infixr1 OfOrs ors (implicationParser $> Impls <|> try backImplicationParser $> BackImpls)
      NoImplication -> OfOrs <$> ors

  biImpl :: Parser BiImpls
  biImpl
    | allowBiImplication = infixr1 OfImpls impl (biImplicationParser $> BiImpls)
    | otherwise = OfImpls <$> impl

  ors :: Parser Ors
  ors
    | allowOr = infixl1 OfAnds ands (orParser $> Ors)
    | otherwise = OfAnds <$> ands

  ands :: Parser Ands
  ands
    | allowAnd = infixl1 OfNeg neg (andParser $> Ands)
    | otherwise = OfNeg <$> neg

  neg :: Parser Neg
  neg = case allowNegation of
    Everywhere ->
          plainNested
      <|> plainAtom
      <|> negationParser *> nextNeg
      where nextNeg = NegAtom <$> atom <|> Neg . OfNested <$> nested  <|> Neg <$> neg
    LiteralsOnly ->
          plainNested
      <|> plainAtom
      <|> negationParser $> NegAtom <*> atom
    Nowhere -> plainNested <|> plainAtom
    where
      plainAtom = OfAtom <$> atom
      plainNested = OfNested <$> nested

  atom :: Parser Atom
  atom
    | allowAtomicProps = Atom <$> atomParser
    | otherwise = fail "no atomic formulas allowed at this level"

  nested :: Parser Nested
  nested
    | allowSilentNesting = Nested <$> silentNested <|> Nested <$> innerNested
    | otherwise = Nested <$> innerNested
    where
      innerNested =
        maybe
          (fail "term structure does not allow nesting at this point")
          (parens . formula)
          nextLevelSpec
      silentNested =
        maybe
          (fail "term structure does not allow nesting at this point")
          formula
          nextLevelSpec

-- type class for parseable types
class FromGrammar a where
  topLevelSpec :: LevelSpec
  fromGrammar :: FormulaGrammar -> Maybe a

formulaParser :: forall a. FromGrammar a => Parser a
formulaParser =
  fromMaybe (error "contradiction between parser rules and fromGrammar") . fromGrammar <$> formula (topLevelSpec @a)
