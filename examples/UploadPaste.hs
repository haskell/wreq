-- upload a paste to lpaste.net
--
-- This example is pretty beefy, as it does double duty.
--
-- Perhaps the majority of it shows off some complex uses of the
-- optparse-applicative package.
--
-- The POST portion is in the function named upload below.  It uploads
-- an application/x-www-urlencoded form that creates a paste on the
-- Haskell community pastebin at <http://lpaste.net/>.

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Control.Lens
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Monoid (mempty)
import Network.Wreq (FormParam((:=)), post, responseBody)
import Network.Wreq.Types (FormValue(..))
import Options.Applicative as Opts hiding ((&), header)
import System.FilePath (takeExtension, takeFileName)
import Text.HTML.TagSoup
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- A post to lpaste.net can either be private or public (visible in an
-- index).
data Visibility = Private | Public
                deriving (Show)

-- Wreq supports uploading an application/x-www-urlencoded form (see
-- uses of the := operator in the upload function below), so we tell
-- it how to render a value of a custom datatype.
instance FormValue Visibility where
  renderFormValue = renderFormValue . show

-- The languages that lpaste.net supports. It just so happens that if
-- we convert one of these constructor names to a lower-case string,
-- it exactly matches what lpaste.net needs in its upload form.
data Language =
  Haskell | Agda | Assembly | Bash | C | Coq | Cpp | Cs | Diff | Elm | ELisp |
  Erlang | Go | Idris | Java | JavaScript | LiterateHaskell | Lisp |
  Lua | OCaml | ObjectiveC | Perl | Prolog | Python | Ruby | SQL | Scala |
  Scheme | Smalltalk | TeX
  deriving (Eq, Show)

instance FormValue Language where
  renderFormValue = renderFormValue . fmap toLower . show

-- An association between filename suffixes and our Language type.
languages :: [([String], Language)]
languages = [
  ([".hs"], Haskell), ([".agda"], Agda), ([".el"], ELisp), ([".ocaml"], OCaml),
  ([".cl"], Lisp), ([".erl"], Erlang), ([".lhs"], LiterateHaskell),
  ([".scala"], Scala), ([".go"], Go), ([".py"], Python), ([".rb"], Ruby),
  ([".elm"], Elm), ([".idris"], Idris), ([".prl"], Prolog), ([".scm"], Scheme),
  ([".coq"], Coq), ([".s", ".asm"], Assembly), ([".sh"], Bash),
  ([".c", ".h"], C), ([".cs"], Cs), ([".tex"], TeX), ([".lua"], Lua),
  ([".cxx", ".cpp", ".cc", ".hxx", ".hpp", ".hh"], Cpp), ([".pl", ".pm"], Perl),
  ([".diff", ".patch"], Diff), ([".java"], Java), ([".js"], JavaScript),
  ([".m"], ObjectiveC), ([".smalltalk"], Smalltalk), ([".sql"], SQL)
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

-- This type plays two roles.  It describes the command line options
-- we accept, and also the contents of the form we'll upload to create
-- a new paste.
--
-- We've parameterised the type so that the payload field can either
-- be a filename or the actual contents of the file.
data Paste a = Paste {
    _private :: Visibility
  , _title :: Maybe String
  , _author :: Maybe String
  , _channel :: Maybe Channel
  , _language :: Maybe Language
  , _payload :: a
  , _email :: () -- used by lpaste.net for spam protection
  } deriving (Show)

makeLenses ''Paste

-- Try to match a user-supplied name to a Language type, looking at
-- both full names and filename extensions.
readLanguage :: Monad m => String -> m Language
readLanguage l = do
  let ll = toLower <$> l
      ms = [lang | (suffixes, lang) <- languages,
            ll == (toLower <$> show lang) || ll `elem` (tail <$> suffixes)]
  case ms of
    [m] -> return m
    _   -> fail $ "unsupported language " ++ show l

-- Figure out the language to specify for a file, either explicitly as
-- specified by the user, or implicitly from the filename extension.
guessLanguage :: FilePath -> Paste a -> Maybe Language
guessLanguage filename p =
    (p ^. language) <|>
    listToMaybe [lang | (suffixes, lang) <- languages, sfx `elem` suffixes]
  where sfx = toLower <$> takeExtension filename

upload :: Paste FilePath -> IO ()
upload p0 = do
  let path = p0 ^. payload
  body <- B.readFile path
  -- Transform command line options into form contents.
  let p = p0 & payload .~ body
             & title .~ (p0 ^. title <|> Just (takeFileName path))
             & language .~ guessLanguage path p0
  -- The := operator defines a key/value pair for a form.
  resp <- post "http://lpaste.net/new" [
            "private" := p ^. private
          , "title" := p ^. title
          , "author" := p ^. author
          , "channel" := p ^. channel
          , "language" := p ^. language
          , "paste" := p ^. payload
          , "email" := p ^. email
          ]
  -- Since lpaste.net doesn't provide an API and just spits HTML back
  -- at us, we use tagsoup to look through the tags for the permalink
  -- of the paste we just uploaded.
  let findURI (TagOpen "strong" [] : TagText "Paste:" : TagClose "strong" :
               TagOpen "a" [("href",uri)] : _) = Just uri
      findURI (_:xs) = findURI xs
      findURI _      = Nothing
  case findURI (parseTagsOptions parseOptionsFast (resp ^. responseBody)) of
    Just uri -> L.putStrLn $ "http://lpaste.net" <> uri
    Nothing  -> putStrLn "no uri in response!?"

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
                               help "file to upload") <*>
          (pure ())
