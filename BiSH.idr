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

catPosts : List (String, Either FileError String) -> List (String, String)
catPosts = catMaybes . map ((\(x, y) => map (x,) y) . mapSnd eitherToMaybe)

getPosts : IO (String, (List Post))
getPosts = do
  Just cwd <- currentDir
    | Nothing => pure ("",[])
  let path : String = cwd ++ (Strings.singleton dirSeparator) ++ "posts"
  putStrLn $ "loading posts from: " ++ path ++ "\n"
  entriesTree <- explore $ parse path
  let entries = map fileName entriesTree.files
      paths   = map (\x => path ++ (Strings.singleton dirSeparator) ++ x) entries
  postsF <- traverse readFile paths
  let postsWithFnames = zip entries postsF
      posts = map post $ catPosts postsWithFnames
  pure (cwd, posts)

doPost : String -> (List Post) -> IO ()
doPost _ [] = putStrLn "No posts"
doPost cwd posts = do
  let root = cwd ++ (Strings.singleton dirSeparator) ++ "static" ++ (Strings.singleton dirSeparator)
  for_ posts $ \p => do
    let newTitle = root ++ p.title ++ ".html"
    let json = encode p
    succ <- writeFile newTitle json
    putStrLn json

main : IO ()
main = do
  args <- getArgs
  (cwd, posts) <- getPosts
  doPost cwd posts
