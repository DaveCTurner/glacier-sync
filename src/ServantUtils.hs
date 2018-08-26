{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ServantUtils where

import           Control.Monad.Except
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Network.HTTP.Types.Header
import           Servant                   hiding (Context)

throwErrorShow :: (Show e, MonadError ServantErr m) => e -> m a
throwErrorShow = throwErrorString . show

throwErrorString :: MonadError ServantErr m => String -> m a
throwErrorString message = throwError ServantErr
  { errHTTPCode = 500
  , errReasonPhrase = "Internal error"
  , errBody = BL.fromStrict $ T.encodeUtf8 $ T.pack message
  , errHeaders = [(hContentType, "text/plain; encoding=utf-8")]
  }

badRequestString :: MonadError ServantErr m => String -> m a
badRequestString message = throwError ServantErr
  { errHTTPCode = 400
  , errReasonPhrase = "Bad request"
  , errBody = BL.fromStrict $ T.encodeUtf8 $ T.pack message
  , errHeaders = [(hContentType, "text/plain; encoding=utf-8")]
  }

liftMaybe :: MonadError ServantErr m => String -> Maybe a -> m a
liftMaybe message = maybe (throwErrorString message) return
