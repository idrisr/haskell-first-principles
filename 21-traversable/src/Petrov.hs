module Petrov where

import Control.Monad ((<=<))

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- run query against db
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- decode from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- context initializer
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn q = do
    a <- fetchFn q
    case mapM decodeFn a of
        Left e -> pure $ Left e
        Right xs -> do
            b <- makeIoOnlyObj xs
            pure . Right $ b

pipelineFn1 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn1 q = do
    a <- fetchFn q
    traverse makeIoOnlyObj $ mapM decodeFn a

pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 q = do
    a <- fetchFn q
    traverse makeIoOnlyObj $ traverse decodeFn a

pipelineFn3 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn3 = traverse makeIoOnlyObj . traverse decodeFn <=< fetchFn
