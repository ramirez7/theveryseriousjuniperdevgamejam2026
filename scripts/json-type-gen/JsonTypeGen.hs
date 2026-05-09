{-# LANGUAGE LambdaCase #-}
module JsonTypeGen where

import Data.Aeson qualified as Ae
import Data.Aeson.KeyMap qualified as AeKM
import Data.Aeson.Key qualified as AeK
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Data.Char (toUpper, toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)

jsonTypeGenIO
  :: FilePath -- ^ in file
  -> FilePath -- ^ out file
  -> String -- ^ module
  -> String -- ^ type
  -> String -- ^ prefix
  -> IO ()
jsonTypeGenIO fpin fpout m t p = do
  j <- readJson fpin
  let hs = jsonTypeGen m t p j
  writeFile fpout hs

readJson :: FilePath -> IO Ae.Value
readJson fp = Ae.decodeFileStrict' fp >>= \case
  Nothing -> error "NOT JSON"
  Just v -> pure v

inferType :: Ae.Value -> Maybe String
inferType = \case
  Ae.Object{} -> Nothing -- TODO: Multiple records
  Ae.Array vs -> case nubOrd $ toList (fmap inferType vs) of
    [Just t] -> Just $ "[" <> t <> "]"
    _ -> Nothing
  Ae.String{} -> Just "String"
  Ae.Number{} -> Just "Scientific"
  Ae.Bool{} -> Just "Bool"
  Ae.Null -> Nothing

inferTypes :: Ae.Value -> [(String, String)]
inferTypes = \case
  Ae.Object obj -> flip mapMaybe (AeKM.toList obj) $ \(k, v) -> (AeK.toString k,) <$> inferType v
  _ -> []

jsonTypeGen :: String -> String -> String -> Ae.Value -> String
jsonTypeGen moduleName typeName prefix json = unlines $ mconcat
  [ [ "module " <> moduleName <> " where"
    , "import Data.Aeson as Ae"
    , "import GHC.Generics"
    , "import Data.Scientific"
    , "import Data.Char (toLower)"
    , "data " <> typeName <> " = " <> typeName
    ]
  , zipWith (<>) ("  { " : repeat "  , ") fields
  , [ "  }"
    , "  deriving (Eq, Ord, Show, Read, Generic)"
    , ""
    , "uncapitalize :: String -> String"
    , "uncapitalize [] = []"
    , "uncapitalize (x:xs) = toLower x : xs"
    , ""
    , optionsVar <> " :: Ae.Options"
    , optionsVar <> " = Ae.defaultOptions { fieldLabelModifier = uncapitalize . drop " <> show (length prefix) <> " }"
    , ""
    , "instance Ae.ToJSON " <> typeName <> " where"
    , "  toJSON = genericToJSON " <> optionsVar
    , "  toEncoding = genericToEncoding " <> optionsVar
    , "instance Ae.FromJSON " <> typeName <> " where"
    , "  parseJSON = genericParseJSON " <> optionsVar
    ]
  ]
  where
    optionsVar = "aesonStrip'" <> prefix
    inferred = inferTypes json
    fields = inferred <&> \(fieldName, fieldType) -> mconcat
      [prefix, capitalize fieldName, " :: ", fieldType]

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (x:xs) = toLower x : xs
