% Building data microservices in Haskell
% Marco Zocca (`github.com/ocramz`)
% Zimpler, November 29, 2017


# Hi !

* Data engineer @ RecordUnion

* "Data pipeline"
* Microservices


# This talk

- Data ingestion service

A handful of useful Haskell notions, techniques, current good practices and libraries




    
 

# Requirements

- Correct
- Easy to verify/test
- Easy to extend with new data providers
- Simple API
- ...
- Fast



# Warm-up : HTTP

##

- [`req`](http://hackage.haskell.org/package/req) (http://hackage.haskell.org/package/req)

##

```
req :: (HttpResponse response, HttpBody body, HttpMethod method,
      MonadHttp m,
      HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
     method
     -> Url scheme
     -> body
     -> Proxy response
     -> Option scheme
     -> m response
```

## 

```
requestGet :: MonadHttp m => m LB.ByteString
requestGet = do
   r <- req
      GET
      (https "www.meetup.com" /: "got-lambda")
      NoReqBody
      lbsResponse
      mempty
   return $ responseBody r   

```

## 

```
requestGCPToken :: (MonadThrow m, CR.MonadRandom m, MonadHttp m) =>
     GCPServiceAccount -> GCPTokenOptions -> m OAuth2Token      
requestGCPToken serviceAcct opts = do
  jwt <- T.decodeUtf8 <$> encodeBearerJWT serviceAcct opts
  let
    payload = encodeHttpParametersLB [
       ("grant_type", T.pack $ urlEncode "urn:ietf:params:oauth:grant-type:jwt-bearer"),
       ("assertion", jwt)]
  r <- req
         POST
         (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token")
         (ReqBodyLbs payload)
         lbsResponse
         (header "Content-Type" "application/x-www-form-urlencoded; charset=utf-8")
  maybe
    (throwM $ NotFound "Something went wrong with the token request")
    pure
    (J.decode (responseBody r) :: Maybe OAuth2Token)

```

##
```
{-# language OverloadedStrings #-}
import Network.HTTP.Req
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B8
import qualified Crypto.Hash.Algorithms as CR
import qualified Crypto.PubKey.RSA.PKCS15 as CR (signSafer) 
import qualified Crypto.Random.Types as CR
import Data.Time
```

Dependencies:

- `req`
- `aeson`
- `text`
- `bytestring`
- `cryptonite`
- `time`




# MonadHttp

```
> :i MonadHttp
class MonadIO m => MonadHttp (m :: * -> *) where
  handleHttpException :: HttpException -> m a
  ...
  {-# MINIMAL handleHttpException #-}
```

- NB : Return type `a` is unconstrained





# Multiple data providers

##

- Each provider has its own :
    - Credentials
    - Authentication/token refresh mechanism
    - Handling of invalid input
    - State
    - Request rate limiting
    - Outage modes
    - etc., etc.

##

```
{-# language TypeFamilies #-}

class HasCredentials c where
  type Credentials c :: *
  type Token c :: *

data Handle c = Handle {
    credentials :: Credentials c
  , token :: TVar (Maybe (Token c))
  }
```
- `TVar` is from `stm`


##

```
{-# language GeneralizedNewtypeDeriving #-}

newtype Cloud c a = Cloud {
  runCloud :: ReaderT (Handle c) IO a
  } deriving (Functor, Applicative, Monad)
```

- `ReaderT env IO a` : _no mutable state_ (see "The ReaderT design pattern")
- `Cloud` will also need `MonadIO`, `MonadThrow`, `MonadCatch`, `CR.MonadRandom`, `MonadReader` instances



## 

```
{-# language FlexibleInstances #-}

data GCP

instance HasCredentials GCP where
  type Credentials GCP = GCPServiceAccount
  type Token GCP = OAuth2Token
```
```
instance MonadHttp (Cloud GCP) where
  handleHttpException = throwM
```

- `GCP` is a "phantom type"
- One type per data provider
    - Individual HTTP exception handling



## Ergonomics

```
requestToken :: Cloud GCP OAuth2Token
requestToken = do
   saOk <- asks credentials
   let opts = GCPTokenOptions scopes
   requestGcpOAuth2Token saOk opts
```

```
requestGcpOAuth2Token :: (MonadHttp m, CR.MonadRandom m, MonadThrow m) =>
     GCPServiceAccount -> GCPTokenOptions -> m OAuth2Token
```

```
> :t asks credentials
asks credentials :: MonadReader (Handle a) m => m (Credentials a)
```

- Q: I don't know how to convince GHC that `GCPServiceAccount` is the `Credentials`-associated type of the `GCP`-tagged instance, therefore the type signature was not inferred automatically ("ambiguous return type").



##


- Running a `Cloud` computation

```
getToken :: IO OAuth2Token
getToken = do
   sa <- GCPServiceAccount <$>
     gcpPrivateRSAKey <*>
     gcpClientEmail 
   evalCloudIO sa requestToken 
```

```
evalCloudIO :: (MonadIO m, MonadCatch m, HasCredentials c) => Handle c -> Cloud c a -> m a
```

##

```
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Control.Monad.Trans.Reader as RT (ask, local)
import Control.Monad.Catch
```

Dependencies

- `mtl`
- `transformers`
- `exceptions`
- `stm`


# Exception handling

##

- `f :: .. -> Either ErrorType a` doesn't compose easily
    - What if caller of `f` returns `Either AnotherErrorType a` ?

- `MonadThrow` and `MonadCatch` follow the GHC exception style (type of exceptions not in the signature).
    - Possible to "refine out" functions having only a `MonadThrow` constraint, which is `~` to returning in `Maybe`


##

```
import Control.Applicative (Alternative(..))

instance HasCredentials c => Alternative (Cloud c) where
    empty = throwM $ UnknownError "empty"
    a1 <|> a2 = do
      ra <- try a1
      case ra of
        Right x -> pure x
        Left e -> case (fromException e) :: Maybe CloudException of
          Just _ -> a2
          Nothing -> throwM (UnknownError "d'oh!")
```

```
(<|>) :: Alternative f => f a -> f a -> f a
empty <|> b == b
a <|> empty == a
```



- `Just _` branch may retry `a1` with different parameters
- Pattern match directly on `CloudException` constructors and retry selectively




# References

- `github.com/ocramz/talks/tree/master/GotLambda_26112017`

- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

