{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

import          Hake

import          Data.List
import          Data.Foldable (for_)

main ∷ IO ()
main = hake $ do
  "clean | clean the project" ∫ do
    idris2 ["--clean"]
    cwd <- getCurrentDirectory
    all <- getDirectoryContents cwd
    let html = filter (isSuffixOf ".html") all
    for_ html $ removeIfExists

  bishExecutable ♯
    idris2 ["--build", "BiSH.ipkg"]

  "run | run BiSH" ◉ [bishExecutable] ∰
    rawSystem bishExecutable []
      >>= checkExitCode

 where
  appName ∷ String
  appName = "BiSH"

  buildPath ∷ String
  buildPath = "build" </> "exec"

  bishExecutable ∷ String
  bishExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> appName ++ "exe"
       | otherwise → buildPath </> appName
