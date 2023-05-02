{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}

-- | <https://ctan.org/tex-archive/macros/latex/contrib/biblatex BibLaTeX>
--   is a reference-citation package using @.bib@ files (BibTeX) but no extra style-files.
--
module Text.LaTeX.Packages.BibLaTeX
 ( biblatex
 , addbibresource
 , cite
 , printbibliography
 -- * Automatic bibliography retrieval
 -- $autoBibRetr
 -- ** Citing
 , citeDOI
 , citeBib
 , textc
 , textC
 -- ** Use in documents
 , documentWithDOIReferences
 , applyDOIReferenceResolves
 , masterBibFile
 -- ** Types
 , PlainDOI
 , DOIReference, DOIOrBibReference, BibTeX.T
 , ReferenceQueryT
 ) where

import Text.LaTeX.Base.Syntax hiding ((<>))
import Text.LaTeX.Base.Class (LaTeXC(..), liftL, fromLaTeX, comm0, raw)
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Commands (cite, footnote, document)

import Data.Char (toLower)
import qualified Data.Semigroup as SG
import GHC.Generics (Generic)
import qualified Data.Traversable as Tr

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Hashable (hash)
import Numeric (showHex)

import qualified Data.List as List

import Control.Applicative
import Control.Monad (forM)
import Control.Monad.IO.Class

import qualified Text.BibTeX.Entry as BibTeX
import qualified Text.BibTeX.Format as BibTeX
import qualified Text.BibTeX.Parse as BibTeX (file)
import qualified Text.Parsec.String as Parsec

-- | BibLaTeX package. Use it to import it like this:
--
-- > usepackage [] biblatex
biblatex :: PackageName
biblatex = "biblatex"

-- | Use a bibliography file as resource for reference information.
addbibresource :: LaTeXC l => FilePath -> l
addbibresource fp = fromLaTeX $ TeXComm "addbibresource" [FixArg $ TeXRaw $ fromString fp]

printbibliography :: LaTeXC l => l
printbibliography = comm0 "printbibliography"


-- $autoBibRetr
-- The following are convenience tools, for using Haskell as a lightweight reference
-- management system in addition to just directly wrapping LaTeX syntax. The intended
-- use is with references that have a DOI available: a DOI is already sufficient to
-- unambiguously point to a source. Keeping unwieldy BibTeX files is therefore unnecessary
-- and redundant; use instead 'citeDOI', which only requires minimal information about the source.
-- Alternatively, BibTeX entries can be directly specified with 'citeBib', when a DOI
-- is not available.
-- In both cases, a @.bib@ file will be generated automatically for use by the LaTeX process.

-- The recommended way of using these in documents is to give each source a simple Haskell
-- definition, like
-- 
-- @
-- doe1950 = TeX.citeDOI "10.123/456" "J Doe et al 1950: Investigation of a Foo"
-- @
-- 
-- which can then be cited like
--
-- @
--    "It is known that Foos are silly "<>doe1950<>", so let's not talk about them anymore."
-- @
--
-- or
--
-- @
--    ... "according to "<>textc doe1950<>", who did not like Foo very much."
-- @
--
-- See <https://github.com/Daniel-Diaz/HaTeX/blob/master/Examples/biblatexDOI.hs Examples/biblatexDOI>
-- for a full document.

-- | All-inclusive preparation of a document containing DOI references.
--   Uses 'applyDOIReferenceResolves' under the hood.
documentWithDOIReferences :: (MonadIO m, LaTeXC (m ()), SG.Semigroup (m ()))
  => (DOIReference -> m (Maybe BibTeX.T))
                               -- ^ Reference-resolver function, for looking up BibTeX
                               --   entries for a given DOI.
                               --   If the DOI cannot be looked up (@Nothing@), we just
                               --   include a footnote with a synopsis and the DOI in
                               --   literal form. (Mostly intended to ease offline editing.)
  -> ReferenceQueryT DOIOrBibReference m ()
                               -- ^ The document content, possibly containing citations
                               --   in DOI-only form.
  -> m ()                      -- ^ LaTeX rendition. The content will already be wrapped
                               --   in @\\begin…end{document}@ here and an
                               --   automatically-generated @.bib@ file included, but
                               --   you still need to 'usepackage' 'biblatex' yourself.
documentWithDOIReferences resolver docW = do
    (refsMap, docConts) <- applyDOIReferenceResolves resolver docW
    let bibfileConts = unlines $ BibTeX.entry . snd <$> Map.toList refsMap
        bibfileName = showHex (abs $ hash bibfileConts) $ ".bib"
    liftIO $ writeFile bibfileName bibfileConts
    () <- addbibresource bibfileName
    document docConts
    
-- | More manual version of 'documentWithDOIReferences', only retrieving suitable
--   BibTeX entries for the DOI-references contained in the document, but not
--   generating any @.bib@ files or wrapping the content in it.
applyDOIReferenceResolves :: (MonadIO m, LaTeXC (m ()), SG.Semigroup (m ()))
  => (DOIReference -> m (Maybe BibTeX.T))    -- ^ Reference-resolver function.
  -> ReferenceQueryT DOIOrBibReference m ()  -- ^ The document content.
  -> m ( Map.Map String BibTeX.T
       , m ())                 -- ^ All the BibTeX entries found, and a version of the
                               --   document with all its doi-references changed to point to
                               --   the identifiers in that file.
applyDOIReferenceResolves resolver (ReferenceQueryT refq) = do
    (allRefs, (), useRefs) <- refq
    resolved <- fmap catMaybes . forM (allRefs[]) $ \case
      (DOICase r) -> do
       r' <- resolver r
       return $ case r' of
         Just entry -> Just (_referenceDOI r, entry)
         Nothing -> Nothing
      (BibCase b@(BibTeX.Cons typ _ fields)) -> do
         let unique = disambiguateBibLabel b
         return $ Just (unique, BibTeX.Cons typ unique fields)
    let refsMap = Map.fromListWith (error "Unhandled collision in Data.Hashable.hash.")
                       resolved          -- TODO use a better disambiguation strategy.
    return (refsMap, useRefs $ \r flavour -> case r of
       DOICase dr -> case Map.lookup (_referenceDOI dr) refsMap of
         Just a -> genCite flavour . raw . fromString $ BibTeX.identifier a
         Nothing -> makeshift dr
       BibCase b
          -> genCite flavour . raw . fromString $ disambiguateBibLabel b
     )
 where makeshift :: (LaTeXC l, SG.Semigroup l) => DOIReference -> l
       makeshift (DOIReference doi synops) = footnote $
           fromLaTeX synops SG.<> ". DOI:" SG.<> fromString doi
       genCite flavour = liftL $ \l -> (`TeXComm`[FixArg l]) $ case flavour of
          Flavour_cite      -> "cite" 
          Flavour_Cite      -> "Cite"
          Flavour_parencite -> "parencite"
          Flavour_Parencite -> "Parencite"
          Flavour_footcite  -> "footcite"
          Flavour_Footcite  -> "Footcite"
          Flavour_textcite  -> "textcite"
          Flavour_Textcite  -> "Textcite"
          Flavour_smartcite -> "smartcite"
          Flavour_Smartcite -> "Smartcite"
       disambiguateBibLabel b = "bib"<>(showHex . hash $ BibTeX.entry b)""
    

class SupportsDOIReferences r where
  fromDOIReference :: DOIReference -> r

type PlainDOI = String

data DOIReference = DOIReference {
       _referenceDOI :: PlainDOI
     , _referenceSynopsis :: LaTeX
     } deriving (Generic, Show)
instance Eq DOIReference where
  DOIReference doi₀ _ == DOIReference doi₁ _ = doi₀ == doi₁
instance Ord DOIReference where
  compare (DOIReference doi₀ _) (DOIReference doi₁ _) = compare doi₀ doi₁
instance SupportsDOIReferences DOIReference where
  fromDOIReference = id


class SupportsBibTeXReferences r where
  fromBibtexEntry :: BibTeX.T -> r

instance SupportsBibTeXReferences BibTeX.T where
  fromBibtexEntry = id


data DOIOrBibReference = DOICase DOIReference | BibCase BibTeX.T

instance SupportsDOIReferences DOIOrBibReference where
  fromDOIReference = DOICase
instance SupportsBibTeXReferences DOIOrBibReference where
  fromBibtexEntry = BibCase


type DList r = [r] -> [r]

data CitationFlavour
       = Flavour_cite
       | Flavour_Cite
       | Flavour_parencite
       | Flavour_Parencite
       | Flavour_footcite
       | Flavour_Footcite
       | Flavour_textcite
       | Flavour_Textcite
       | Flavour_smartcite
       | Flavour_Smartcite
     deriving (Eq, Ord, Show)

newtype ReferenceQueryT r m a = ReferenceQueryT {
       runReferenceQueryT :: m (DList r, a, (r -> CitationFlavour -> m ()) -> m ())
     }
  deriving (Generic, Functor)

instance Applicative m => Applicative (ReferenceQueryT r m) where
  pure x = ReferenceQueryT . pure $ (id, x, const $ pure ())
  ReferenceQueryT refqf <*> ReferenceQueryT refqx = ReferenceQueryT $
       liftA2 (\(urefsf, f, refref)
                (urefsx, x, refrex)
                  -> ( urefsf . urefsx
                     , f x
                     , \resolv -> mappend <$> refref resolv <*> refrex resolv ) )
              refqf refqx
instance Monad m => Monad (ReferenceQueryT r m) where
  return = pure
  ReferenceQueryT refsx >>= f
     = ReferenceQueryT $ refsx >>= \(urefsx, x, refrex)
           -> case f x of
                ReferenceQueryT refsfx
                  -> (\(urefsfx,fx,refrefx)
                        -> ( urefsx.urefsfx
                           , fx
                           , \resolve -> mappend <$> refrex resolve <*> refrefx resolve ))
                     <$> refsfx
instance MonadIO m => MonadIO (ReferenceQueryT r m) where
  liftIO a = ReferenceQueryT $ (\r -> (id, r, const $ pure ())) <$> liftIO a

instance (Functor m, Monoid (m a), IsString (m ()), a ~ ())
           => IsString (ReferenceQueryT r m a) where
  fromString s = ReferenceQueryT $ (\a -> (id, a, const $ fromString s)) <$> mempty

instance (Applicative m, SG.Semigroup (m a), a ~ ())
             => SG.Semigroup (ReferenceQueryT r m a) where
  ReferenceQueryT p <> ReferenceQueryT q
      = ReferenceQueryT $ liftA2 (\(rp,(),ρp) (rq,(),ρq)
                                     -> (rp.rq,(),liftA2(liftA2 (SG.<>))ρp ρq)) p q

instance (Applicative m, SG.Semigroup (m a), Monoid (m a), a ~ ())
    => Monoid (ReferenceQueryT r m a) where
  mempty = ReferenceQueryT $ (\a -> (id, a, mempty)) <$> mempty
  mappend = (SG.<>)

instance (Applicative m, LaTeXC (m a), SG.Semigroup (m a), a ~ ())
             => LaTeXC (ReferenceQueryT r m a) where
  liftListL f xs = ReferenceQueryT $
    (\components -> case List.unzip3 components of
          (refs, _, rebuilds) -> ( foldr (.) id refs
                                 , ()
                                 , \resolve -> liftListL f $ ($ resolve)<$>rebuilds )
       ) <$> Tr.traverse runReferenceQueryT xs

citeDOI :: (Functor m, Monoid (m ()), IsString (m ()), SupportsDOIReferences r)
        => PlainDOI  -- ^ The unambiguous document identifier.
        -> String    -- ^ Synopsis of the cited work, in the form
                     --   @"J Doe et al 1950: Investigation of a Foo"@;
                     --   this is strictly speaking optional, the synopsis will /not/
                     --   be included in the final document (provided the DOI
                     --   can be properly resolved).
        -> ReferenceQueryT r m ()
citeDOI doi synops = ReferenceQueryT $ (\a -> ( (r :), a, \f -> f r Flavour_cite ))
                       <$> mempty
 where r = fromDOIReference . DOIReference doi $ fromString synops

citeBib :: (Functor m, Monoid (m ()), IsString (m ()), SupportsBibTeXReferences r)
        => BibTeX.T  -- ^ Full BibTeX entry, as would normally be part of a @.bib@ file.
        -> ReferenceQueryT r m ()
citeBib e = ReferenceQueryT $ (\a -> ( (r :), a, \f -> f r Flavour_cite ))
                       <$> mempty
 where r = fromBibtexEntry e

-- | Change citations generated with 'citeDOI' or 'citeBib' from the default
--   @\\cite@ to @\\textcite@, i.e. so as to be used as a noun in a sentence.
--   (In typical BibLaTeX styles, this means the citation appears in a form like
--   “Dow et al. 1950” instead of merely e.g. “[Do50]”.)
textc :: Functor m => ReferenceQueryT r m () -> ReferenceQueryT r m ()
textc (ReferenceQueryT y) = ReferenceQueryT
        $ (\(r,m,a) -> (r, m, \f -> a (\x _ -> f x Flavour_textcite))) <$> y

-- | Like 'textc' but transform a citation into @\\Textcite@, i.e. so that it can
--   be used as the first word in a sentence.
textC :: Functor m => ReferenceQueryT r m () -> ReferenceQueryT r m ()
textC (ReferenceQueryT y) = ReferenceQueryT
        $ (\(r,m,a) -> (r, m, \f -> a (\x _ -> f x Flavour_Textcite))) <$> y

masterBibFile :: MonadIO m
      => FilePath    -- ^ A @.bib@ file containing entries for all relevant literature.
      -> (DOIReference -> m (Maybe BibTeX.T))
                     -- ^ Lookup-function, suitable for 'documentWithDOIReferences'.
masterBibFile master (DOIReference doi _) = do
   entries <- liftIO $ BibTeX.file `Parsec.parseFromFile` master
   return $ case entries of
     Right bibs -> List.find hasThisDOI bibs
     Left err   -> error $ show err
 where hasThisDOI bib = (map toLower <$> List.lookup "doi" (BibTeX.fields bib))
                          == Just (toLower<$>doi)
