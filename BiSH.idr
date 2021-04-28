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

getPosts : IO (List Post)
getPosts = do
  Just cwd <- currentDir
    | Nothing => pure []
  let path : String = cwd ++ (Strings.singleton dirSeparator) ++ "posts"
  putStrLn $ "loading posts from: " ++ path ++ "\n"
  entriesTree <- explore $ parse path
  let entries = map fileName entriesTree.files
      paths   = map (\x => path ++ (Strings.singleton dirSeparator) ++ x) entries
  postsF <- traverse readFile paths
  let postsWithFnames = zip entries postsF
      posts = map post $ catPosts postsWithFnames
  pure posts

doPost : (List Post) -> IO ()
doPost [] = putStrLn "No posts"
doPost posts = do
  for_ posts $ \p => putStrLn $ encode p

main : IO ()
main = do
  args <- getArgs
  posts <- getPosts
  doPost posts
