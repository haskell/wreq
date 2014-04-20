{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Control.Lens
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Monoid (mempty)
import Network.Wreq (FormParam((:=)), FormValue(..), post)
import Options.Applicative as Opts hiding ((&), header)
import System.FilePath (takeExtension, takeFileName)
import System.IO (IOMode(..), withFile)
import qualified Data.ByteString.Char8 as B

-- A post to lpaste.net can either be private or public (visible in an
-- index).
data Visibility = Private | Public
                deriving (Show)

-- Wreq supports uploading an application/x-www-urlencoded form (see
-- the := operator below), so we tell it how to render a value of a
-- custom datatype.
instance FormValue Visibility where
  renderFormValue = renderFormValue . show

-- The languages that lpaste.net supports.
data Language =
  Haskell | Agda | Assembly | Bash | C | CommonLisp | Coq | Cpp | Cs | Diff |
  Elm | EmacsLisp | Erlang | Go | Idris | Java | JavaScript | LiterateHaskell |
  Lua | OCaml | ObjectiveC | Perl | Prolog | Python | Ruby | SQL | Scala |
  Scheme | Smalltalk | TeX
  deriving (Eq, Show)

instance FormValue Language where
  renderFormValue lang = renderFormValue $
    head [name | (_, name, l) <- languages, lang == l]

-- An association between filename suffixes, the name that lpaste.net
-- expects in a form, and our Language type.
languages :: [([String], String, Language)]
languages = [
  ([".hs"], "haskell", Haskell), ([".agda"], "agda", Agda),
  ([".el"], "elisp", EmacsLisp), ([".ocaml"], "ocaml", OCaml),
  ([".cl"], "lisp", CommonLisp), ([".erl"], "erlang", Erlang),
  ([".lhs"], "literatehaskell", LiterateHaskell),
  ([".scala"], "scala", Scala), ([".go"], "go", Go),
  ([".py"], "python", Python), ([".rb"], "ruby", Ruby),
  ([".elm"], "elm", Elm), ([".idris"], "idris", Idris),
  ([".prl"], "prolog", Prolog), ([".scm"], "scheme", Scheme),
  ([".coq"], "coq", Coq), ([".s", ".asm"], "asm", Assembly),
  ([".sh"], "bash", Bash), ([".c", ".h"], "c", C),
  ([".cs"], "cs", Cs), ([".tex"], "tex", TeX),
  ([".cxx", ".cpp", ".cc", ".hxx", ".hpp", ".hh"], "cpp", Cpp),
  ([".diff", ".patch"], "diff", Diff), ([".java"], "java", Java),
  ([".js"], "javascript", JavaScript), ([".lua"], "lua", Lua),
  ([".m"], "objectivec", ObjectiveC), ([".pl", ".pm"], "perl", Perl),
  ([".smalltalk"], "smalltalk", Smalltalk), ([".sql"], "sql", SQL)
  ]

-- An IRC channel to which an announcement of a paste can be posted.
-- We wrap this in a newtype so we can control how it is rendered in a
-- form.
newtype Channel = Channel { fromChannel :: String }
                deriving (Eq, Show)

-- If a user forgot to supply a leading '#' for a channel name, we add
-- it here.
instance FormValue Channel where
  renderFormValue = renderFormValue . checkHash . fromChannel
    where checkHash cs@('#':_) = cs
          checkHash cs@(_:_)   = '#' : cs
          checkHash cs         = cs

data Paste a = Paste {
    _visibility :: Visibility
  , _title :: Maybe String
  , _author :: Maybe String
  , _channel :: Maybe Channel
  , _language :: Maybe Language
  , _payload :: a
  } deriving (Show)

makeLenses ''Paste

-- Try to match a user-supplied name to a Language type, looking at
-- both full names and filename extensions.
readLanguage :: Monad m => String -> m Language
readLanguage l = do
  let ll = toLower <$> l
      ms = [lang | (suffixes, name, lang) <- languages,
            ll == name || ll `elem` (tail <$> suffixes)]
  case ms of
    [m] -> return m
    _   -> fail $ "unsupported language " ++ show l

-- Figure out the language to specify for a file, either explicitly as
-- specified by the user, or implicitly from the filename extension.
guessLanguage :: FilePath -> Paste a -> Maybe Language
guessLanguage filename p =
    (p ^. language) <|>
    listToMaybe [lang | (suffixes, _, lang) <- languages, sfx `elem` suffixes]
  where sfx = toLower <$> takeExtension filename

upload :: Paste FilePath -> IO ()
upload p0 = do
  let path = p0 ^. payload
  p <- withFile path ReadMode $ \h -> do
         body <- B.hGetContents h
         return $ p0 & payload .~ body
                     & title .~ (p0 ^. title <|> Just (takeFileName path))
                     & language .~ guessLanguage path p0
  resp <- post "http://lpaste.net/new" [
            "private" := p ^. visibility
          , "title" := p ^. title
          , "author" := p ^. author
          , "channel" := p ^. channel
          , "language" := p ^. language
          , "paste" := p ^. payload
          , "email" := (""::String)
          ]
  print resp
  return ()

main :: IO ()
main = upload =<< execParser opts
  where opts = info (helper <*> optionParser) mempty
        optionParser = Paste <$>
          (flag Private Public $ long "public" <>
                                 help "display in index of pastes") <*>
          (optional . strOption $
           long "title" <> short 't' <> metavar "TITLE" <>
           help "title to use for paste") <*>
          (optional . strOption $
           long "author" <> short 'a' <> metavar "AUTHOR" <>
           help "author to display for paste") <*>
          (optional . fmap Channel . strOption $
           long "channel" <> short 'c' <> metavar "CHANNEL" <>
           help "name of IRC channel to announce") <*>
          (optional . nullOption $
           long "language" <> short 'l' <> metavar "LANG" <>
           help "language to use" <> reader readLanguage) <*>
          (Opts.argument str $ metavar "PATH" <>
                               help "file to upload")
