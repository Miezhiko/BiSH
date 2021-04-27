module BiSH

import Types

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
  let posts = map post files
  pure posts

doPost : (List Post) -> IO ()
doPost [] = putStrLn "No posts"
doPost posts = do
  for_ posts $ \p => putStrLn p.text

main : IO ()
main = do
  args <- getArgs
  posts <- getPosts
  doPost posts
