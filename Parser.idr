module Parser

import Types
import Utils

import Data.List
import Data.String

import System
import System.Path
import System.File
import System.Directory
import System.Directory.Tree

export
getPosts : String -> IO (List Post)
getPosts cwd = do
  let path : String = cwd ++ (String.singleton dirSeparator) ++ "posts"
  putStrLn $ "loading posts from: " ++ path
  entriesTree <- explore $ parse path
  let entries = map fileName entriesTree.files
      paths   = map (\x => path ++ (String.singleton dirSeparator) ++ x) entries
  postsF <- traverse readFile paths
  let postsWithFnames = zip entries postsF
      posts = map post $ catFF postsWithFnames
  pure posts

export
getTemplates : String -> IO (List Template)
getTemplates cwd = do
  let path : String = cwd ++ (String.singleton dirSeparator) ++ "templates"
  putStrLn $ "loading templates from: " ++ path ++ "\n"
  entriesTree <- explore $ parse path
  let entries = map fileName entriesTree.files
      paths   = map (\x => path ++ (String.singleton dirSeparator) ++ x) entries
  templatesF <- traverse readFile paths
  let templatesWithFnames = zip entries templatesF
      templates = map template $ catFF templatesWithFnames
  pure templates
