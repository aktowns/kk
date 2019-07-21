module LSP where

import qualified Control.Exception                     as E

import qualified Language.Haskell.LSP.Control          as CTRL
import qualified Language.Haskell.LSP.Core             as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import qualified Language.Haskell.LSP.Utility          as U
import           Language.Haskell.LSP.VFS

run :: IO () -> IO Int
run dispatcherProc = flip E.catches handlers $ do
    rin  <- atomically newTChan :: IO (TChan ReactorInput)

    let dp lf = do
        liftIO $ putStrLn "main.run:dp entered"
        _rpid  <- forkIO $ reactor lf rin
        liftIO $ U.logs "main.run:dp tchan"
        dispatcherProc
        liftIO $ U.logs "main.run:dp after dispatcherProc"
        return Nothing

    flip E.finally finalProc $ do
        Core.setupLogger (Just "/tmp/lsp-hello.log") [] L.DEBUG
        CTRL.run (return (Right ()), dp) (lspHandlers rin) lspOptions (Just "/tmp/lsp-hello-session.log")

    where
        handlers = [ E.Handler ioExcept
                   , E.Handler someExcept
                   ]
        finalProc = L.removeAllHandlers
        ioExcept   (e :: E.IOException)       = print e >> return 1
        someExcept (e :: E.SomeException)     = print e >> return 1
