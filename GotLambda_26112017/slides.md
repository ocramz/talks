% Building data microservices in Haskell
% Marco Zocca (`github.com/ocramz`)
% Zimpler, November 29, 2017


# Introduction

* "Data pipeline"
* Microservices = functionally independent units in the pipeline



# This talk

Data ingestion service

- `cron`-job for REST-based data retrieval

- Service resilience
    - State is only external to the service (cloud storage)

- Dataset resilience
    - Providers fail/change their mind
    
  


# Requirements


- Easy to verify/test
- Easy to extend with new data providers

- Simple API



# Warm-up : HTTP

##

- [`req`](http://hackage.haskell.org/package/req) (http://hackage.haskell.org/package/req)

##

```
req
  :: (HttpResponse response, HttpBody body, HttpMethod method,
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
    args = [
       ("grant_type", T.pack $ urlEncode "urn:ietf:params:oauth:grant-type:jwt-bearer"),
       ("assertion", jwt)]
    payload = encodeHttpParameters args
  r <- req POST
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








# References







# Vertical subslides

## First

- `github.com/ocramz/talks/tree/master/GotLambda_26112017`

## Second

- baz

