module Tedious where

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
    a <- fetchFn query
    case sequence (map decodeFn a) of
        (Left err) -> return $ Left err
        (Right res) -> do
            a <- makeIoOnlyObj res
            return $ Right a

pipelineFn1 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn1 query = do
    a <- fetchFn query
    traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 = ((traverse makeIoOnlyObj . mapM decodeFn) =<<) . fetchFn

pipelineFn3 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn3 = ((traverse makeIoOnlyObj . traverse decodeFn) =<<) . fetchFn
