{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP

import System.Environment

main :: IO ()
main = do
  let hostname = "localhost"
      bindPort = "3000"
  eTransport <- createTransport hostname bindPort (hostname,) defaultTCPParameters
  localNode <- case eTransport of
    Left err -> error $ show err
    Right transport -> newLocalNode transport initRemoteTable

  args <- getArgs
  case args of
    ["works"] -> thisWorks localNode
    ["doesntwork"] -> thisDoesntWork localNode

--------------------------------------------------------------------------------

thisWorks :: LocalNode -> IO ()
thisWorks localNode = do
  let procName = "thisWorks"
  runProcess localNode $ do

    pid <- spawnLocal $ do
      say "Spawned WhereIsReply handler process"
      register procName =<< getSelfPid
      flip whereIsRemoteProc procName =<< getSelfNode
      forever $ receiveWait
        [ match handleWhereIsReply ]

    liftIO $ threadDelay 2000000
    say =<< show <$> getProcessInfo pid
    say =<< show <$> getLocalNodeStats

    hangForever

thisDoesntWork :: LocalNode -> IO ()
thisDoesntWork localNode = do
  let procName = "thisDoesntWork"
  runProcess localNode $ do

    pid <- spawnLocal $ do
      say "Spawned WhereIsReply handler process"
      register procName =<< getSelfPid
      forever $ receiveWait
        [ match handleWhereIsReply ]

    liftIO $ threadDelay 1000000
    say $ "Sending remote WhereIs message"
    let nid = localNodeId localNode
    whereIsRemoteProc nid procName

    liftIO $ threadDelay 2000000
    say =<< show <$> getProcessInfo pid
    say =<< show <$> getLocalNodeStats

    hangForever

--------------------------------------------------------------------------------

whereIsRemoteProc :: NodeId -> String -> Process ()
whereIsRemoteProc nid procName = do
  liftIO $ threadDelay 1000000
  say "Sending remote WhereIs message..."
  flip whereisRemoteAsync procName nid

handleWhereIsReply :: WhereIsReply -> Process ()
handleWhereIsReply (WhereIsReply _ mPid) =
  say $ "Received WhereIsReply: " ++ show mPid

--------------------------------------------------------------------------------

hangForever = forever $ liftIO $ threadDelay 1000000
