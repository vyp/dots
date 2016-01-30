-- TODO: Tabbed layout.
-- TODO: Grid Layout.
-- TODO: Theme/colours (e.g. window border colour) in a different file.
-- TODO: Panel!

import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W

myKeys = \c -> mkKeymap c $
    [ ("M-<Return>", spawn $ XMonad.terminal c)
    , ("M-/",        spawn "dmenu_run")
    , ("M-x",        kill)
    , ("M-r",        sendMessage NextLayout)
    , ("M-C-r",      setLayout $ XMonad.layoutHook c)
    , ("M-e",        refresh)
    , ("M-f",        sendMessage $ Toggle NBFULL)
    , ("M-h",        sendMessage $ Go L)
    , ("M-j",        sendMessage $ Go D)
    , ("M-k",        sendMessage $ Go U)
    , ("M-l",        sendMessage $ Go R)
    , ("M-S-h",      sendMessage $ Swap L)
    , ("M-S-j",      sendMessage $ Swap D)
    , ("M-S-k",      sendMessage $ Swap U)
    , ("M-S-l",      sendMessage $ Swap R)
    , ("M-m",        windows W.focusMaster)
    , ("M-S-m",      windows W.swapMaster)
    , ("M-M1-h",     sendMessage Shrink)
    , ("M-M1-j",     sendMessage MirrorShrink)
    , ("M-M1-k",     sendMessage MirrorExpand)
    , ("M-M1-l",     sendMessage Expand)
    , ("M-t",        withFocused $ windows . W.sink)
    , ("M-u",        focusUrgent)
    , ("M-v",        sendMessage $ Toggle REFLECTX)
    , ("M-S-v",      sendMessage $ Toggle REFLECTY)
    , ("M-,",        sendMessage $ IncMasterN 1)
    , ("M-.",        sendMessage $ IncMasterN (-1))
    , ("M-q",        spawn "xmonad --restart")
    , ("M-S-q",      io $ exitWith ExitSuccess)
    , ("M-\\",       toggleWS)
    , ("M-n",        nextWS)
    , ("M-p",        prevWS)
    , ("M-S-n",      shiftToNext)
    , ("M-S-p",      shiftToPrev)
    , ("M-M1-n",     shiftToNext >> nextWS)
    , ("M-M1-p",     shiftToPrev >> prevWS)
    , ("M-o",        moveTo  Next NonEmptyWS)
    , ("M-i",        moveTo  Next EmptyWS)
    , ("M-S-o",      shiftTo Next NonEmptyWS)
    , ("M-S-i",      shiftTo Next EmptyWS)
    , ("M-C-o",      moveTo  Prev NonEmptyWS)
    , ("M-C-i",      moveTo  Prev EmptyWS)
    , ("M-C-S-o",    shiftTo Prev NonEmptyWS)
    , ("M-C-S-i",    shiftTo Prev EmptyWS)
    ] ++
    [ (p ++ i, f i)
        | i <- map show [0..9]
        , (f, p) <- [(toggleOrView, "M-"), (windows . W.shift, "M-S-")]
    ]

myLayout = mkToggle (single NBFULL)
    . mkToggle (single REFLECTY)
    . mkToggle (single REFLECTX)
    $ navigable tiled ||| navigable (Mirror tiled) ||| Full
  where
    navigable :: LayoutClass l w => l w -> ModifiedLayout WindowNavigation l w
    navigable = configurableNavigation noNavigateBorders
    tiled     = ResizableTall nmaster delta ratio slaves
    nmaster   = 1
    delta     = 3/100
    ratio     = 1/2
    slaves    = []

main = xmonad $ withUrgencyHookC NoUrgencyHook myUrgencyConfig $ defaultConfig
    { terminal           = "urxvtc"
    , modMask            = mod4Mask
    , workspaces         = (map show [1..9]) ++ ["0"]
    , borderWidth        = 2
    , normalBorderColor  = "#fbf1c7"
    , focusedBorderColor = "#d5c4a1"
    , keys               = myKeys
    , layoutHook         = myLayout
    }
  where
    myUrgencyConfig = urgencyConfig { suppressWhen = Focused }
