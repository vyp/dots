-- TODO: Focus next urgent window.
-- TODO: Cartesian window navigation/movement.
-- TODO: Change ratio/height of windows vertically?
-- TODO: 'Easymotion'-esque window navigation?
-- TODO: Preselect new window placement?
-- TODO: Theme/colours (e.g. window border colour) in a different file.
-- TODO: Panel!

import System.Exit
import XMonad
import XMonad.Actions.CycleWS
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
    , ("M-j",        windows W.focusDown)
    , ("M-k",        windows W.focusUp)
    , ("M-S-j",      windows W.swapDown)
    , ("M-S-k",      windows W.swapUp)
    , ("M-m",        windows W.focusMaster)
    , ("M-S-m",      windows W.swapMaster)
    , ("M-h",        sendMessage Shrink)
    , ("M-l",        sendMessage Expand)
    , ("M-t",        withFocused $ windows . W.sink)
    , ("M-,",        sendMessage $ IncMasterN 1)
    , ("M-.",        sendMessage $ IncMasterN (-1))
    , ("M-q",        spawn "xmonad --restart")
    , ("M-S-q",      io $ exitWith ExitSuccess)
    ] ++
    [ (k++i, f i)
        | i <- map show [0..9]
        , (f, k) <- [(toggleOrView, "M-"), (windows . W.shift, "M-S-")] ] ++
    [ ("M-n",   nextWS)
    , ("M-p",   prevWS)
    , ("M-S-n", shiftToNext)
    , ("M-S-p", shiftToPrev)
    , ("M-o",   moveTo  Next NonEmptyWS)
    , ("M-i",   moveTo  Next EmptyWS)
    , ("M-S-o", shiftTo Next NonEmptyWS)
    , ("M-S-i", shiftTo Next EmptyWS)
    , ("M-C-o", moveTo  Prev NonEmptyWS)
    , ("M-C-i", moveTo  Prev EmptyWS)
    ]

main = xmonad $ defaultConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    , workspaces         = myWorkspaces
    }
