{-# LANGUAGE OverloadedStrings, JavaScriptFFI, ForeignFunctionInterface, InterruptibleFFI, CPP #-}

module Browser where

#ifdef __GHCJS__

import GHCJS.Types
import GHCJS.Foreign

foreign import javascript interruptible "window.cliAsk($c);" askBrowser' :: IO JSString
foreign import javascript unsafe "window.cliPrint($1);" printBrowser' :: JSString -> IO ()

askBrowser :: IO String
askBrowser = do {
  s <- askBrowser';
  return (fromJSString s)
}

printBrowser :: String -> IO ()
printBrowser s = printBrowser' (toJSString s)

#else

askBrowser :: t
askBrowser = error "Only available in JavaScript"

printBrowser :: t
printBrowser = error "Only available in JavaScript"

#endif
