module Types

-- import JSON
-- import Generics.Derive

%language ElabReflection

%default total

export
data PostType = Article | Other

-- %runElab derive "PostType" [Generic,Meta,Show,Eq,EnumToJSON,EnumFromJSON]

public export
record Post where
  constructor MkPost
  title : String
  text  : String
  date  : String
  type  : PostType

-- %runElab derive "Post" [Generic,Meta,Show,Eq,RecordToJSON,RecordFromJSON]

export
post : (String, String) -> Post
post (fn, ft) =
  MkPost fn ft "" Article

public export
data TemplateType = IndexTemplate | PostTemplate | ArticleTemplate | Unknown

-- %runElab derive "TemplateType" [Generic,Meta,Show,Eq,EnumToJSON,EnumFromJSON]

public export
record Template where
  constructor MkTemplate
  fname : String
  text  : String
  type  : TemplateType

-- %runElab derive "Template" [Generic,Meta,Show,Eq,RecordToJSON,RecordFromJSON]

export
template : (String, String) -> Template
template (fn, ft) =
  case fn of
    "index.html.hbs"    => MkTemplate "index.html" ft IndexTemplate
    "post.html.hbs"     => MkTemplate "" ft PostTemplate
    "article.html.hbs"  => MkTemplate "" ft ArticleTemplate
    _                   => MkTemplate fn ft Unknown
