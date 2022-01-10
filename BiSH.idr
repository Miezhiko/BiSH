module BiSH

import Types
import Utils
import Parser

import Data.List
import Data.String

import System
import System.Path
import System.File
import System.Directory

-- TODO: ugly function but I don't know how to do it properly
unzipTemplate : Template -> ( Maybe Template
                            , Maybe Template
                            , Maybe Template
                            , Maybe Template )
unzipTemplate t =
  case t.type of
    IndexTemplate   => ( Just(t), Nothing, Nothing, Nothing )
    PostTemplate    => ( Nothing, Just(t), Nothing, Nothing )
    ArticleTemplate => ( Nothing, Nothing, Just(t), Nothing )
    Unknown         => ( Nothing, Nothing, Nothing, Just(t) )

generatePostsTitles: (List (Maybe Template)) -> (List Post) -> String
generatePostsTitles postTemplates posts = do
  case (catMaybes postTemplates) of
    [] => ""
    pt::xs =>
      let px = map (\p => do
        let tit = strReplace "{{ fname }}" p.title pt.text
        strReplace "{{ text }}" p.text tit) posts
      in unlines px

generatePosts: (List (Maybe Template)) -> (List Post) -> IO ()
generatePosts articleTemplates posts = do
  case (catMaybes articleTemplates) of
    [] => pure ()
    pt::xs =>
      for_ posts $ \p => do
        let tit = strReplace "{{ fname }}" p.title pt.text
        let px = strReplace "{{ text }}" p.text tit
        let genFname = p.title ++ ".html"
        succ <- writeFile genFname px
        pure ()

generate : String -> (List Post) -> (List Template) -> IO ()
generate _ [] _ = putStrLn "No posts"
generate _ _ [] = putStrLn "No templates"
generate cwd posts templates = do
  let root = cwd ++ (String.singleton dirSeparator)
  let ( indexes
      , postTemplates
      , articleTemplates
      , unknown ) = unzipWith4 unzipTemplate templates

  generatePosts articleTemplates posts

  case (catMaybes indexes) of
    [] => putStrLn "Warning: Missing index template"
    i::xs => do
      putStrLn $ "processing template: " ++ i.fname
      let postsString = generatePostsTitles postTemplates posts
          pr = strReplace "{{ posts }}" postsString i.text
      succ <- writeFile i.fname pr
      pure ()

  case (catMaybes unknown) of
    [] => pure ()
    i::xs => do
      putStrLn $ "processing unknown template: " ++ i.fname
      succ <- writeFile i.fname i.text
      pure ()

  putStrLn "complete"

main : IO ()
main = do
  Just cwd <- currentDir
    | Nothing => pure ()
  posts <- getPosts cwd
  templates <- getTemplates cwd
  generate cwd posts templates
