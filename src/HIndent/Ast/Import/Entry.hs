{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Entry
  ( ImportEntries
  , mkImportEntries
  , sortEntriesAndVariants
  ) where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

data ImportEntries = ImportEntries
  { entries :: [Entry]
  , kind :: EntriesKind
  }

instance CommentExtraction ImportEntries where
  nodeComments ImportEntries {} = NodeComments [] [] []

instance Pretty ImportEntries where
  pretty' ImportEntries {..} = do
    when (kind == Hiding) $ string " hiding"
    (space >> hTuple (fmap pretty entries))
      <-|> (newline >> indentedBlock (vTuple $ fmap pretty entries))

data EntriesKind
  = Explicit
  | Hiding
  deriving (Eq)

newtype Entry =
  Entry (GHC.LIE GHC.GhcPs)

instance CommentExtraction Entry where
  nodeComments (Entry _) = NodeComments [] [] []

instance Pretty Entry where
  pretty' (Entry x) = pretty x

mkEntry :: GHC.LIE GHC.GhcPs -> Entry
mkEntry = Entry

mkImportEntries ::
     GHC.ImportDecl GHC.GhcPs -> Maybe (WithComments ImportEntries)
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportEntries GHC.ImportDecl {..} =
  case ideclImportList of
    Just (GHC.Exactly, imports) ->
      Just
        $ (\entries ->
             ImportEntries {entries = fmap mkEntry entries, kind = Explicit, ..})
            <$> mkWithCommentsWithGenLocated imports
    Just (GHC.EverythingBut, imports) ->
      Just
        $ (\entries ->
             ImportEntries {entries = fmap mkEntry entries, kind = Hiding, ..})
            <$> mkWithCommentsWithGenLocated imports
    Nothing -> Nothing
#else
mkImportEntries GHC.ImportDecl {..} =
  case ideclHiding of
    Just (False, imports) ->
      Just
        $ (\entries ->
             ImportEntries {entries = fmap mkEntry entries, kind = Explicit, ..})
            <$> mkWithCommentsWithGenLocated imports
    Just (True, imports) ->
      Just
        $ (\entries ->
             ImportEntries {entries = fmap mkEntry entries, kind = Hiding, ..})
            <$> mkWithCommentsWithGenLocated imports
    Nothing -> Nothing
#endif
sortEntriesAndVariants :: ImportEntries -> ImportEntries
sortEntriesAndVariants = sortExplicitImports . sortVariants'

sortVariants' :: ImportEntries -> ImportEntries
sortVariants' ImportEntries {..} = ImportEntries {entries = sorted, ..}
  where
    sorted = fmap sortVariants entries

-- | This function sorts variants (e.g., data constructors and class
-- methods) in the given explicit import by their names.
sortVariants :: Entry -> Entry
sortVariants (Entry (GHC.L l (GHC.IEThingWith x x' x'' xs))) =
  Entry $ GHC.L l $ GHC.IEThingWith x x' x'' (sortWrappedNames xs)
  where
    sortWrappedNames = sortBy (compare `on` showOutputable)
sortVariants x = x

sortExplicitImports :: ImportEntries -> ImportEntries
sortExplicitImports ImportEntries {..} = ImportEntries {entries = sorted, ..}
  where
    sorted = sortBy compareImportEntities entries

-- | This function sorts the given explicit imports by their names.
-- | This function compares two import declarations by their module names.
compareImportEntities :: Entry -> Entry -> Ordering
compareImportEntities (Entry (GHC.L _ a)) (Entry (GHC.L _ b)) =
  fromMaybe LT $ compareIdentifier <$> getModuleName a <*> getModuleName b

-- | This function compares two identifiers in order of capitals, symbols,
-- and lowers.
compareIdentifier :: String -> String -> Ordering
compareIdentifier as@(a:_) bs@(b:_) =
  case compareChar a b of
    EQ -> compareSameIdentifierType as bs
    x -> x
compareIdentifier _ _ = error "Either identifier is an empty string."

-- | This function returns a 'Just' value with the module name extracted
-- from the import declaration. Otherwise, it returns a 'Nothing'.
getModuleName :: GHC.IE GHC.GhcPs -> Maybe String
getModuleName (GHC.IEVar _ wrapped) = Just $ showOutputable wrapped
getModuleName (GHC.IEThingAbs _ wrapped) = Just $ showOutputable wrapped
getModuleName (GHC.IEThingAll _ wrapped) = Just $ showOutputable wrapped
getModuleName (GHC.IEThingWith _ wrapped _ _) = Just $ showOutputable wrapped
getModuleName _ = Nothing

-- | This function compares two characters by their types (capital, symbol,
-- and lower). If both are the same type, then it compares them by the
-- usual ordering.
compareChar :: Char -> Char -> Ordering
compareChar a b =
  case compare at bt of
    EQ -> compare a b
    x -> x
  where
    at = charToLetterType a
    bt = charToLetterType b

-- | This function returns a 'LetterType' based on the given character.
charToLetterType :: Char -> LetterType
charToLetterType c
  | isLower c = Lower
  | isUpper c = Capital
  | otherwise = Symbol

-- | The letter type of a 'Char'.
--
-- The order of constructors is important. HIndent sorts explicit imports
-- from ones starting from a capital letter (e.g., data constructors),
-- symbol identifiers, and functions.
data LetterType
  = Capital
  | Symbol
  | Lower
  deriving (Eq, Ord)

-- | Almost similar to 'compare' but ignores parentheses for symbol
-- identifiers as they are enclosed by parentheses.
compareSameIdentifierType :: String -> String -> Ordering
compareSameIdentifierType "" "" = EQ
compareSameIdentifierType "" _ = LT
compareSameIdentifierType _ "" = GT
compareSameIdentifierType ('(':as) bs = compareSameIdentifierType as bs
compareSameIdentifierType (')':as) bs = compareSameIdentifierType as bs
compareSameIdentifierType as ('(':bs) = compareSameIdentifierType as bs
compareSameIdentifierType as (')':bs) = compareSameIdentifierType as bs
compareSameIdentifierType (a:as) (b:bs) =
  case compare a b of
    EQ -> compareSameIdentifierType as bs
    x -> x
