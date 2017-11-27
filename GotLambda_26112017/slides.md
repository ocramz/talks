% Building microservices in Haskell
% Marco Zocca (`github.com/ocramz`)
% Zimpler, November 29, 2017



# Introduction


* "Data pipeline"
* Microservices = functionally independent units in the pipeline

# This talk


Data ingestion service

- `cron`-job for REST-based data retrieval
- Resilience:
    - State is only external to the service (cloud storage)
    - Retry logic



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

## A GET request

```
request1 :: MonadHttp m => m LB.ByteString
request1 = do
   r <- req
      GET
      (https "www.google.com")
      NoReqBody
      lbsResponse
      mempty
   return $ responseBody r   

```









# References







# Vertical subslides

## First

- `github.com/ocramz/talks/tree/master/GotLambda_26112017`

## Second

- baz

