-- TODO: Next/previous occupied/free/urgent workspace.
-- TODO: 10 workspaces.
-- TODO: Go back to previous workspace if win + the number of the current
-- workspace is pressed. Allows for quick switching/toggling of workspace views.
-- TODO: Cartesian window navigation/movement.
-- TODO: Change ratio/height of windows vertically?
-- TODO: 'Easymotion'-esque window navigation?
-- TODO: Preselect new window placement?
-- TODO: Theme/colours (e.g. window border colour) in a different file.
-- TODO: Panel!

import System.Exit
import XMonad
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W

myTerminal = "urxvtc"
myModMask  = mod4Mask

myBorderWidth        = 2
myNormalBorderColor  = "#fbf1c7"
myFocusedBorderColor = "#d5c4a1"

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
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
    [ ("M-" ++ x,   windows $ W.greedyView x) | x <- map show [1..9] ] ++
    [ ("M-S-" ++ x, windows $ W.shift x)      | x <- map show [1..9] ]

main = xmonad $ defaultConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    }
