{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Formula.Parsing (
  module Formula.Parsing.Type,
  formulaSymbolParser,
  formulaListSymbolParser,
  clauseSetParser,
  clauseFormulaParser,
  resStepsParser,
  resStepParser,
  stepAnswerParser,
  prologClauseFormulaParser,
  prologClauseSetParser
  ) where

import Config hiding (Right)
import qualified Config as DecideChoice (DecideChoice(Right))
import Formula.Util
import ParsingHelpers (caseInsensitive, lexeme, tokenSymbol)
import Formula.Types

import Control.Monad (void)
import Text.ParserCombinators.Parsec (
  Parser,
  (<?>),
  (<|>),
  alphaNum,
  between,
  char,
  digit,
  many,
  many1,
  notFollowedBy,
  optionMaybe,
  satisfy,
  sepBy,
  spaces,
  string,
  try,
  )

import UniversalParser
import Formula.Parsing.Type (Parse(..))
import Trees.Parsing ()

resStepsParser :: Parser Clause -> Parser [ResStep]
resStepsParser parseClause = (lexeme (listParse (resStepParser parseClause)) <?> "List")
      <|> fail (
        "Could not parse a list of values: " ++
        "The elements of a list are enclosed by square brackets '[ ]' and separated by commas."
        )

resStepParser :: Parser Clause -> Parser ResStep
resStepParser parseClause = do
    tokenSymbol "("
    cl1 <- parseEither parseClause parseNum
    tokenSymbol ","
    cl2 <- parseEither parseClause parseNum
    tokenSymbol ","
    cl3 <- parseClause
    index <- optionMaybe indexParse
    tokenSymbol ")"
    pure $ Res (cl1,cl2,(cl3,index))

   where
    indexParse = tokenSymbol "=" >> lexeme parseNum

    parseEither x y = lexeme ((Left <$> try x) <|> (Right <$> y))

    parseNum = do
      i <- many1 digit
      pure (read i)

instance Parse ResStep where
  parser = resStepParser clauseSetParser

notFollowedByElse :: Parser a -> (a -> Parser ()) -> Parser ()
notFollowedByElse p f = try ((try p >>= f) <|> pure ())



listParse :: Parser a -> Parser [a]
listParse p = do
  tokenSymbol "[" <|> fail "could not parse an opening '['"
  xs <- p `sepBy` (tokenSymbol "," <|> fail "parsed a wrong separator: Lists are comma-separated.")
  tokenSymbol "]" <|> fail "could not parse an enclosing ']'"
  pure xs

instance Parse a => Parse [a] where
  parser = (lexeme (listParse parser) <?> "List")
      <|> fail (
        "Could not parse a list of values: " ++
        "The elements of a list are enclosed by square brackets '[ ]' and separated by commas."
        )


instance (Parse a, Parse b) => Parse (a,b) where
  parser = between (tokenSymbol "(") (tokenSymbol ")") $ do
    a <- parser
    tokenSymbol ","
    b <- parser
    pure (a,b)


instance Parse Number where
  parser = (lexeme numParse <?> "Number") <|> fail "Could not parse a number"
    where numParse = do
            result <- optionMaybe $ many1 digit
            pure $ Number $ fmap read result


stepAnswerParser :: Parser Clause -> Parser StepAnswer
stepAnswerParser parseClause = (lexeme stepParse <?> "Resolution Step") <|> fail "Could not parse a tuple"
    where stepParse = do
            result <- optionMaybe parseTuple
            pure $ StepAnswer result
          parseTuple = do
            void $ lexeme $ char '('
            lit <- parser
            void $ lexeme $ char ','
            resolvent <- parseClause
            void $ lexeme $ char ')'
            pure (lit, resolvent)

instance Parse StepAnswer where
  parser = stepAnswerParser clauseFormulaParser




instance Parse TruthValue where
  parser = lexeme truthParse <?> "Truth Value"

    where truthParse = do
            t <- try
             (    parseTrue
              <|> parseFalse
              <|> fail "Could not parse a truth value: Please enter values as described in the exercise description."
             )
              <|> fail "The truth value was mistyped."
            notFollowedByElse alphaNum (\c ->
              fail $ unlines
                [ "unexpected " ++ [c]
                , "Additional characters were appended to this truth value or it was mistyped."
                ]
              )
            pure t
              where
                parseTrue = do
                  string "1" <|> try (single "w") <|> try (single "t")
                    <|> caseInsensitive "wahr"
                    <|> caseInsensitive "true"
                  pure $ TruthValue True
                parseFalse = do
                  string "0" <|> try (single "f") <|> eitherDeEn
                  pure $ TruthValue False

                single :: String -> Parser String
                single s = do
                    res <- caseInsensitive s
                    notFollowedBy alphaNum
                    return res

                eitherDeEn = caseInsensitive "fals" >> -- no-spell-check
                  (try (caseInsensitive "e") <|> caseInsensitive "ch")





instance Parse Literal
instance FromGrammar Literal where
  topLevelSpec = LevelSpec
    { allowOr = False
    , allowAnd = False
    , allowNegation = LiteralsOnly
    , allowAtomicProps = True
    , allowImplication = NoImplication
    , strictParens = False
    , allowBiImplication = False
    , allowSilentNesting = False
    , nextLevelSpec = Nothing
    }

  fromGrammar (WithPrecedence (NoArrows (NoOrs (NoAnds (OfAtom (Atom x)))))) = Just $ Positive x
  fromGrammar (WithPrecedence (NoArrows (NoOrs (NoAnds (NegAtom (Atom x)))))) = Just $ Negative x
  fromGrammar _ = Nothing

clauseSetParser :: Parser Clause
clauseSetParser = mkClause <$> braces (parser `sepBy` tokenSymbol ",")
  where
    braces = between (tokenSymbol "{") (tokenSymbol "}")

clauseFormulaParser :: Parser Clause
clauseFormulaParser = mkClause [] <$ tokenSymbol "{}" <|> formulaParser

instance Parse Clause where
  parser = clauseFormulaParser

instance FromGrammar Clause where
  topLevelSpec = LevelSpec
    { allowOr = True
    , allowAnd = False
    , allowNegation = LiteralsOnly
    , allowAtomicProps = True
    , allowImplication = NoImplication
    , allowBiImplication = False
    , strictParens = False
    , allowSilentNesting = False
    , nextLevelSpec = Nothing
    }

  fromGrammar OfNoFixity{} = Nothing
  fromGrammar (WithPrecedence BiImpls{}) = Nothing
  fromGrammar (WithPrecedence TopLevelImpl) = Nothing
  fromGrammar (WithPrecedence TopLevelBackImpl) = Nothing
  fromGrammar (WithPrecedence (NoArrows f)) =  mkClause <$> foldlOrs phi (Just []) f
    where
      phi :: Maybe [Literal] -> Ands -> Maybe [Literal]
      phi xs (NoAnds (OfAtom (Atom x))) = (Positive x :) <$> xs
      phi xs (NoAnds ((NegAtom (Atom x)))) = (Negative x :) <$> xs
      phi _ (NoAnds (Neg{})) = Nothing
      phi _ (NoAnds (OfNested{})) = Nothing
      phi _ Ands{} = Nothing

instance Parse Con
instance FromGrammar Con where
  topLevelSpec = LevelSpec
    { allowOr = False
    , allowAnd = True
    , allowNegation = LiteralsOnly
    , allowAtomicProps = True
    , allowImplication = NoImplication
    , allowBiImplication = False
    , strictParens = False
    , allowSilentNesting = False
    , nextLevelSpec = Nothing
    }

  fromGrammar (WithPrecedence (NoArrows Ors{})) = Nothing
  fromGrammar OfNoFixity{} = Nothing
  fromGrammar (WithPrecedence (NoArrows (OfAnds f))) = mkCon <$> foldlAnds phi (Just []) f
    where
      phi :: Maybe [Literal] -> Neg -> Maybe [Literal]
      phi xs (OfAtom (Atom x)) = (Positive x :) <$> xs
      phi xs (NegAtom (Atom x)) = (Negative x :) <$> xs
      phi _ Neg{} = Nothing
      phi _ OfNested{} = Nothing
  fromGrammar _ = Nothing

instance Parse Cnf
instance FromGrammar Cnf where
  topLevelSpec = (topLevelSpec @Con) { allowSilentNesting = True, nextLevelSpec = Just $ topLevelSpec @Clause }

  fromGrammar = (mkCnf <$>) . go
    where
      go (WithPrecedence (NoArrows (OfAnds f))) = foldlAnds phi (Just []) f
      go (WithPrecedence (SkipLevel f)) = pure <$> fromGrammar @Clause f
      go (WithPrecedence (NoArrows Ors{})) = Nothing
      go OfNoFixity{} = Nothing
      go (WithPrecedence BiImpls{}) = Nothing
      go (WithPrecedence TopLevelImpl) = Nothing
      go (WithPrecedence TopLevelBackImpl) = Nothing
      phi :: Maybe [Clause] -> Neg -> Maybe [Clause]
      phi xs (OfNested (Nested f)) = (:) <$> fromGrammar f  <*> xs
      phi xs (OfAtom (Atom x)) = (mkClause [Positive x] :) <$> xs
      phi xs (NegAtom (Atom x)) = (mkClause [Negative x] :) <$> xs
      phi _ Neg{} = Nothing

instance Parse Dnf
instance FromGrammar Dnf where
  topLevelSpec = (topLevelSpec @Clause) { allowSilentNesting = True, nextLevelSpec = Just $ topLevelSpec @Con }

  fromGrammar OfNoFixity{} = Nothing
  fromGrammar (WithPrecedence BiImpls{}) = Nothing
  fromGrammar (WithPrecedence TopLevelImpl) = Nothing
  fromGrammar (WithPrecedence TopLevelBackImpl) = Nothing
  fromGrammar (WithPrecedence (NoArrows f)) = mkDnf <$> foldlOrs phi (Just []) f
    where
      phi :: Maybe [Con] -> Ands -> Maybe [Con]
      phi xs (NoAnds (OfNested (Nested x))) = (:) <$> fromGrammar x <*> xs
      phi xs (NoAnds (OfAtom (Atom x))) = (mkCon [Positive x] :) <$> xs
      phi xs (NoAnds (NegAtom (Atom x))) = (mkCon [Negative x] :) <$> xs
      phi _ (NoAnds Neg{}) = Nothing
      phi _ (Ands{}) = Nothing

instance Parse PrologLiteral where
  parser = (lexeme litParse <?> "Literal")
           <|> fail "Could not parse a literal."
    where
      litParse = notNeg <|> operatorNeg <|> posLit

      notNeg = do
        tokenSymbol "not"
        between (tokenSymbol "(") (tokenSymbol ")") (atomParse False)

      operatorNeg = do
        tokenSymbol "¬" <|> tokenSymbol "~" <|> tokenSymbol "-" <|> tokenSymbol "￢"
        atomParse False

      posLit = atomParse True

      atomParse pol = do
        ident <- strParse
        lexeme $ char '('
        facts <- lexeme $ sepBy (lexeme strParse) (lexeme $ char ',')
        lexeme $ char ')'
        pure $ PrologLiteral pol ident facts
        where
          strParse = many1 $ satisfy $ flip elem ['A'..'z']

emptyClauseParser :: Parser PrologClause
emptyClauseParser = do
  char '{'
  spaces
  char '}'
  pure $ mkPrologClause []

prologClauseSetParser :: Parser PrologClause
prologClauseSetParser = (lexeme (try emptyClauseParser <|> clauseParse) <?> "Clause")
          <|> fail "Could not parse a clause: Clauses are a set of literals separated by a comma."
   where
     clauseParse = do
       braces <- lexeme $ optionMaybe $ char '('
       ts <- between (tokenSymbol "{") (tokenSymbol "}") $ sepBy parser (tokenSymbol ",")
       case braces of Nothing -> pure ' '
                      Just _ -> char ')'
       pure $ mkPrologClause ts

prologClauseFormulaParser :: Parser PrologClause
prologClauseFormulaParser = (lexeme (emptyClauseParser <|> clauseParse) <?> "Clause")
          <|> fail "Could not parse a clause: Clauses are composed out of terms and the 'or operator' (\\/)."
   where
     clauseParse = do
       braces <- lexeme $ optionMaybe $ char '('
       ts <- sepBy parser orParser
       case braces of Nothing -> pure ' '
                      Just _ -> char ')'
       pure $ mkPrologClause ts


listSymbolParser :: Parser ()
listSymbolParser = tokenSymbol "[" <|> tokenSymbol "," <|> tokenSymbol "]"

formulaSymbolParser :: Parser ()
formulaSymbolParser = void $ many logicToken

formulaListSymbolParser :: Parser ()
formulaListSymbolParser = void $ many $ logicToken <|> listSymbolParser


instance Parse PrologClause where
 parser = prologClauseFormulaParser

instance Parse DecideAnswer where
  parser = DecideAnswer <$> lexeme (try parseCorrect <|> try parseWrong <|> parseNoAnswer)
    where
      parseCorrect = Just DecideChoice.Right <$
          ( try (caseInsensitive "Richtig")
        <|> caseInsensitive "Right"
          )
      parseWrong = Just Wrong <$
          ( try (caseInsensitive "Fehlerhaft")
        <|> caseInsensitive "Wrong"
          )
      parseNoAnswer = Nothing <$
          ( try (caseInsensitive "Nichts")
        <|> caseInsensitive "None"
          )
