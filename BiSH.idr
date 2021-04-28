module BiSH

import Types

import JSON
import Generics.Derive

import Data.Either
import Data.List
import Data.Strings

import System
import System.Path
import System.File
import System.Directory

getEntries : Directory -> IO (List String)
getEntries d
    = do Right f <- dirEntry d
             | Left err => pure []
         ds <- assert_total $ getEntries d
         pure (f :: ds)

getPosts : IO (List Post)
getPosts = do
  Just cwd <- currentDir
    | Nothing => pure []
  let path : String = cwd ++ (Strings.singleton dirSeparator) ++ "posts"
  putStrLn $ "loading posts from: " ++ path ++ "\n"
  Right dir <- openDir path
    | Left err => pure []
  entries <- getEntries dir
  closeDir dir
  let files = filter (\x => x /= "."
                         && x /= "..") entries
  let paths = map (\x => path ++ (Strings.singleton dirSeparator) ++ x) files
  postsF <- traverse readFile paths
  let postsWithFnames = zip files $ rights postsF
  let posts = map post postsWithFnames
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
