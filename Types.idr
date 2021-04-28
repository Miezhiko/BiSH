module Types

import JSON
import Generics.Derive

%language ElabReflection

%default total

export
data PostType = Article | Other

%runElab derive "PostType" [Generic,Meta,Show,Eq,EnumToJSON,EnumFromJSON]

public export
record Post where
  constructor MkPost
  title : String
  text  : String
  date  : String
  type  : PostType

%runElab derive "Post" [Generic,Meta,Show,Eq,RecordToJSON,RecordFromJSON]

export
post : (String, String) -> Post
post (fn, ft) =
  MkPost fn ft "" Article
