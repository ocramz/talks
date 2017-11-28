% Building data microservices in Haskell
% Marco Zocca (`github.com/ocramz`)
% Zimpler, November 29, 2017


# Hi !

* Data engineer @ RecordUnion

* "Data pipeline"
* Microservices = functional units in a distributed program


# This talk

- Data ingestion service

How I work with libraries and APIs


    
 

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





# Multiple data providers

##

```
{-# language TypeFamilies #-}

class HasCredentials a where
  type Credentials a :: *
  type Token a :: *

data Handle a = Handle {
    credentials :: Credentials a
  , token :: TVar (Maybe (Token a))
  }
```

```
newtype Cloud c a = Cloud {
  runCloud :: ReaderT (Handle c) IO a
  } deriving (Functor, Applicative, Monad)
```

- "`mtl` style" : need `MonadIO`, `MonadThrow`, `MonadCatch`, `CR.MonadRandom`, `MonadReader` instances



## 

```
{-# language FlexibleInstances #-}

data GCP

instance HasCredentials GCP where
  type Credentials GCP = GCPServiceAccount
  type Token GCP = OAuth2Token

instance MonadHttp (Cloud GCP) where
  handleHttpException = throwM
```

- "Phantom types"
- One type per data provider



## Ergonomics

```
requestToken :: Cloud GCP OAuth2Token
requestToken = do
   saOk <- asks credentials
   let opts = GCPTokenOptions scopes
   requestGcpOAuth2Token saOk opts
```

```
getToken :: IO OAuth2Token
getToken = do
   sa <- GCPServiceAccount <$>
     gcpPrivateRSAKey <*>
     gcpClientEmail 
   evalCloudIO sa requestToken 
```





# References







# Vertical subslides

## First

- `github.com/ocramz/talks/tree/master/GotLambda_26112017`

## Second

- baz

