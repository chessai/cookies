# Cookies

Wherein our hero wishes to have convenience methods for creating browser cookies.


Two functions are defined for the convenience of users. They include
```haskell
encodeCookie :: (a -> ByteString) -> Cookie a -> TL.Builder
decodeCookie :: (Text -> Either Text a) -> Text -> Either Text (Cookie a)
```
