{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cookie
  ( Cookie(..)
  , CookieContent(..)
  , SameSite(..)
  , defaultCookie
  , encodeCookie
  , decodeCookie
  ) where

import Chronos.Types (Datetime, Timespan)
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Monoid (Monoid)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time (UTCTime(UTCTime), formatTime, defaultTimeLocale)
import qualified Chronos as C
import qualified Chronos.Types as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Builder.Int as TL
import qualified Data.Time as UTC

data CookieContent a = CookieContent
  { cookieContentName  :: !Text
  , cookieContentValue :: !a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Cookie a = Cookie
  { cookieContent  :: !(CookieContent a)
  , cookieExpires  :: !(Maybe Datetime)
  , cookieMaxAge   :: !(Maybe Timespan)
  , cookieDomain   :: !(Maybe Text)
  , cookiePath     :: !(Maybe [Text])
  , cookieSecure   :: !Bool
  , cookieHttpOnly :: !Bool
  , cookieSameSite :: !(Maybe SameSite)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data SameSite
  = SameSiteLax
  | SameSiteStrict
  deriving (Eq, Ord, Show)

defaultCookie :: CookieContent a -> Cookie a
defaultCookie content = Cookie content Nothing Nothing Nothing Nothing False False Nothing

encodeCookie :: (a -> ByteString) -> Cookie a -> TL.Builder
encodeCookie encodeContent (Cookie (CookieContent name value) expires maxAge domain path secure httpOnly sameSite) =
  (mconcat . L.intersperse "; " . mconcat)
  [ [ TL.fromText name <> "=" <> (TL.fromText . TE.decodeUtf8 . encodeContent) value ]
  , maybeToList expires $ \ex -> "Expires=" <> TL.fromText (formatExpires $ datetimeToUTCTime ex)
  , maybeToList maxAge $ \ma -> "Max-Age=" <> TL.decimal (C.getTimespan ma `div` 1000000000)
  , maybeToList domain $ \d -> "Domain=" <> TL.fromText d
  , maybeToList path $ \p -> "Path=/" <> TL.fromText (T.intercalate "/" p)
  , bool [] ["Secure"] secure
  , bool [] ["HttpOnly"] httpOnly
  , maybeToList sameSite $ \case
      SameSiteLax -> "SameSite=Lax"
      SameSiteStrict -> "SameSite=Strict"
  ]
  where
    datetimeToUTCTime :: Datetime -> UTCTime
    datetimeToUTCTime dt@(C.Datetime _ (C.TimeOfDay h m n)) = UTCTime d undefined
      where
        dif = UTC.secondsToDiffTime $ fromIntegral (3600 * h + 60 * m + fromIntegral (n `div` 1000000000))
        d = UTC.ModifiedJulianDay $ fromIntegral $ C.getDay $ C.timeToDayTruncate $ C.datetimeToTime dt
    formatExpires :: UTCTime -> Text
    formatExpires = T.pack . formatTime defaultTimeLocale expiresFormat
    expiresFormat = "%a, %d-%b-%Y %X GMT"
    maybeToList m f = maybe [] ((:[]) . f) m

decodeCookie :: (Text -> Either Text a) -> Text -> Either Text (Cookie a)
decodeCookie decodeValue txt = case L.filter (\(x,_) -> x == "auth") (kvPairs txt) of
  ((name,value):_) ->
    flip fmap (decodeValue value) $ \v ->
      Cookie
        { cookieContent = CookieContent name v
        , cookieExpires = Nothing
        , cookieMaxAge = Nothing
        , cookieDomain = Nothing
        , cookiePath = Nothing
        , cookieSecure = False
        , cookieHttpOnly = False
        , cookieSameSite = Nothing
        }
  _ -> Left "Empty Cookie"
  where
    kvPairs = fmap (both (T.dropAround (==' ')) . breakOnDiscard "=") . L.filter (not . T.null) . T.splitOn ";"
    both f (x,y) = (f x, f y)
    breakOnDiscard b = second (T.drop 1) . T.breakOn b
