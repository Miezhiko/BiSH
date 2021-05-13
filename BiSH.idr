module BiSH

import Types

import JSON

import Data.Either
import Data.List
import Data.Strings

import System
import System.Path
import System.File
import System.Directory
import System.Directory.Tree

catFF : List (String, Either FileError String) -> List (String, String)
catFF = catMaybes . map ((\(x, y) => map (x,) y) . mapSnd eitherToMaybe)

getPosts : String -> IO (List Post)
getPosts cwd = do
  let path : String = cwd ++ (Strings.singleton dirSeparator) ++ "posts"
  putStrLn $ "loading posts from: " ++ path
  entriesTree <- explore $ parse path
  let entries = map fileName entriesTree.files
      paths   = map (\x => path ++ (Strings.singleton dirSeparator) ++ x) entries
  postsF <- traverse readFile paths
  let postsWithFnames = zip entries postsF
      posts = map post $ catFF postsWithFnames
  pure posts

getTemplates : String -> IO (List Template)
getTemplates cwd = do
  let path : String = cwd ++ (Strings.singleton dirSeparator) ++ "templates"
  putStrLn $ "loading templates from: " ++ path ++ "\n"
  entriesTree <- explore $ parse path
  let entries = map fileName entriesTree.files
      paths   = map (\x => path ++ (Strings.singleton dirSeparator) ++ x) entries
  templatesF <- traverse readFile paths
  let templatesWithFnames = zip entries templatesF
      templates = map template $ catFF templatesWithFnames
  pure templates

unzipTemplate : Template -> (Maybe Template, Maybe Template, Maybe Template)
unzipTemplate t =
  case t.type of
    IndexTemplate => (Just(t), Nothing, Nothing)
    PostTemplate  => (Nothing, Just(t), Nothing)
    HBS           => (Nothing, Nothing, Just(t))
    Unknown       => (Nothing, Nothing, Nothing)

generate : String -> (List Post) -> (List Template) -> IO ()
generate _ [] _ = putStrLn "No posts"
generate _ _ [] = putStrLn "No templates"
generate cwd posts templates = do
  let root = cwd ++ (Strings.singleton dirSeparator)
  let (indexes, posts, other) = unzipWith3 unzipTemplate templates

  case (catMaybes indexes) of
    [] => putStrLn "Missing index template"
    i::xs => do
      putStrLn $ "processing Index: " ++ i.fname
      let px = map (\p => p.text) (catMaybes posts)
      let pt = unlines px
      -- TODO: need to write replace =_=
      -- let pr = replace "{{ posts }}" pt i.text
      succ <- writeFile i.fname i.text
      pure ()

  for_ (catMaybes other) $ \t => do
    putStrLn $ "processing HBS: " ++ t.fname
    -- let json = encode t
    succ <- writeFile t.fname t.text
    pure ()

  putStrLn "complete"

main : IO ()
main = do
  Just cwd <- currentDir
    | Nothing => pure ()
  posts <- getPosts cwd
  templates <- getTemplates cwd
  generate cwd posts templates
