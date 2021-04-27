module Types

%default total

public export
record Post where
  constructor MkPost
  title : String
  text : String
  date : String

export
post : String -> Post
post f =
  MkPost f f f
