module Utils

import Data.Either
import Data.List

import System.File

export
unzipWith4 : (a -> (Maybe b, Maybe c, Maybe d, Maybe g))
           -> List a
           -> ( List (Maybe b)
              , List (Maybe c)
              , List (Maybe d)
              , List (Maybe g) )
unzipWith4 f [] = ([], [], [], [])
unzipWith4 f (x :: xs) = let (b, c, d, g) = f x
                             (bs, cs, ds, gs) = unzipWith4 f xs in
                             (b :: bs, c :: cs, d :: ds, g :: gs)

export
strReplace : (what : String) -> (with_ : String) -> (in_ : String) -> String
strReplace what with_ in_ = pack $ inner (unpack what) (unpack with_) (unpack in_)
  where
    inner : List Char -> List Char -> List Char -> List Char
    inner [] ys zs = zs
    inner _ ys [] = []
    inner xs ys (z::zs) = if isPrefixOf xs (z::zs)
      then ys ++ inner xs ys (drop (length xs) (z::zs))
      else z :: inner xs ys zs

export
catFF : List (String, Either FileError String) -> List (String, String)
catFF = catMaybes . map ((\(x, y) => map (x,) y) . mapSnd eitherToMaybe)
