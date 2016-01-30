-- TODO: Focus next urgent window.
-- TODO: Change ratio/height of windows vertically?
-- TODO: 'Easymotion'-esque window navigation?
-- TODO: Preselect new window placement?
-- TODO: Theme/colours (e.g. window border colour) in a different file.
-- TODO: Panel!

import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W

myTerminal = "urxvtc"
myModMask  = mod4Mask

myBorderWidth        = 2
myNormalBorderColor  = "#fbf1c7"
myFocusedBorderColor = "#d5c4a1"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
myKeys       = \c -> mkKeymap c $
    [ ("M-<Return>", spawn $ XMonad.terminal c)
    , ("M-/",        spawn "dmenu_run")
    , ("M-x",        kill)
    , ("M-r",        sendMessage NextLayout)
    , ("M-C-r",      setLayout $ XMonad.layoutHook c)
    , ("M-e",        refresh)
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
    , ("M-M1-l",     sendMessage Expand)
    , ("M-t",        withFocused $ windows . W.sink)
    , ("M-,",        sendMessage $ IncMasterN 1)
    , ("M-.",        sendMessage $ IncMasterN (-1))
    , ("M-q",        spawn "xmonad --restart")
    , ("M-S-q",      io $ exitWith ExitSuccess)
    ] ++
    [ (p++i, f i)
        | i <- map show [0..9]
        , (f, p) <- [(toggleOrView, "M-"), (windows . W.shift, "M-S-")] ] ++
    [ ("M-n",     nextWS)
    , ("M-p",     prevWS)
    , ("M-S-n",   shiftToNext)
    , ("M-S-p",   shiftToPrev)
    , ("M-M1-n",  shiftToNext >> nextWS)
    , ("M-M1-p",  shiftToPrev >> prevWS)
    , ("M-o",     moveTo  Next NonEmptyWS)
    , ("M-i",     moveTo  Next EmptyWS)
    , ("M-S-o",   shiftTo Next NonEmptyWS)
    , ("M-S-i",   shiftTo Next EmptyWS)
    , ("M-C-o",   moveTo  Prev NonEmptyWS)
    , ("M-C-i",   moveTo  Prev EmptyWS)
    , ("M-C-S-o", shiftTo Prev NonEmptyWS)
    , ("M-C-S-i", shiftTo Prev EmptyWS)
    ]

myLayout = navigable tiled ||| navigable (Mirror tiled) ||| Full
  where
    navigable :: LayoutClass l w => l w -> ModifiedLayout WindowNavigation l w
    navigable = configurableNavigation noNavigateBorders
    tiled     = Tall nmaster delta ratio
    nmaster   = 1
    ratio     = 1/2
    delta     = 3/100

main = xmonad $ defaultConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    , keys               = myKeys
    , layoutHook         = myLayout
    }
