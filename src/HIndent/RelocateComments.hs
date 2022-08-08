{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HIndent.RelocateComments
  ( relocateComments
  ) where

import           Generics.SYB    hiding (typeRep)
import           GHC.Hs
import           Type.Reflection

-- | This function collects all comments from the passed 'HsModule', and modifies all 'EpAnn's so that all 'EpAnn's have 'EpaCommentsBalanced's.
--
-- HIndent gathers all comments above a function, an import, a module declaration, etc. For example, HIndent formats the following code
--
-- > f :: Int
-- > f = 1
-- >
-- > -- A comment between f and g
-- >
-- > -- Another comment between f and g
-- >
-- > g :: Int
-- > g = 2
--
-- to
--
-- > f :: Int
-- > f = 1
-- >
-- > -- A comment between f and g
-- > -- Another comment between f and g
-- > g :: Int
-- > g = 2
--
-- AST's nodes must have the information of which comments are above, on the same line, and below. However, the 'HsModule's AST nodes obtained by parsing a module contain only comments after the nodes.
--
-- This function solves the problem by collecting all 'LEpaComment's with 'listify', and iterates all nodes from top to bottom a few times. During the first iteration, this function adds comments above each node from the collected ones to the node. On the next iteration, it adds a comment on the same line. On the last iteration, it adds comments below them.
relocateComments :: HsModule -> HsModule
relocateComments = everywhere (apply id)
  where
    allComments = listify (const True)

-- | This function applies the given function to all 'EpAnn's.
apply ::
     forall b. (Typeable b)
  => (forall a. EpAnn a -> EpAnn a)
  -> (b -> b)
apply f =
  case typeRep @b of
    App g _ ->
      case eqTypeRep g (typeRep @EpAnn) of
        Just HRefl -> f
        Nothing    -> id
    _ -> id
