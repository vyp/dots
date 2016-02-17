import Data.List (isPrefixOf)
import System.Directory
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ToggleHook
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified XMonad.StackSet as W

myKeys c = mkKeymap c $
    [ ("M-<Return>",              launchTerminal)
    , ("M-M1-<Return>",           hookNext "centerFloat" True >> launchTerminal)
    , ("M-/",                     spawn "dmenu_run")
    , ("M-x",                     kill)
    , ("M-r",                     sendMessage NextLayout)
    , ("M-C-r",                   setLayout $ XMonad.layoutHook c)
    , ("M-e",                     spawn "emacsclient -c")
    , ("M-f",                     sendMessage $ Toggle NBFULL)
    , ("M-m",                     windows W.focusMaster)
    , ("M-S-m",                   windows W.swapMaster)
    , ("M-d",                     windows W.focusDown)
    , ("M-a",                     windows W.focusUp)
    , ("M-S-d",                   windows W.swapDown)
    , ("M-S-a",                   windows W.swapUp)
    , ("M-M1-h",                  sendMessage Shrink)
    , ("M-M1-j",                  sendMessage MirrorShrink)
    , ("M-M1-k",                  sendMessage MirrorExpand)
    , ("M-M1-l",                  sendMessage Expand)
    , ("M-t",                     withFocused $ windows . W.sink)
    , ("M-u",                     focusUrgent)
    , ("M-b",                     sendMessage ToggleStruts)
    , ("M-v",                     sendMessage $ Toggle REFLECTX)
    , ("M-S-v",                   sendMessage $ Toggle REFLECTY)
    , ("M-,",                     sendMessage $ IncMasterN 1)
    , ("M-.",                     sendMessage $ IncMasterN (-1))
    , ("M-q",                     spawn "xmonad --restart")
    , ("M-S-q",                   io $ exitWith ExitSuccess)
    , ("M-\\",                    toggleWS)
    , ("M-n",                     nextWS)
    , ("M-p",                     prevWS)
    , ("M-S-n",                   shiftToNext)
    , ("M-S-p",                   shiftToPrev)
    , ("M-M1-n",                  shiftToNext >> nextWS)
    , ("M-M1-p",                  shiftToPrev >> prevWS)
    , ("M-o",                     moveTo  Next NonEmptyWS)
    , ("M-i",                     moveTo  Next EmptyWS)
    , ("M-S-o",                   shiftTo Next NonEmptyWS)
    , ("M-S-i",                   shiftTo Next EmptyWS)
    , ("M-C-o",                   moveTo  Prev NonEmptyWS)
    , ("M-C-i",                   moveTo  Prev EmptyWS)
    , ("M-C-S-o",                 shiftTo Prev NonEmptyWS)
    , ("M-C-S-i",                 shiftTo Prev EmptyWS)
    , ("<XF86AudioRaiseVolume>",  spawn "amixer set Master 2%+")
    , ("<XF86AudioLowerVolume>",  spawn "amixer set Master 2%-")
    , ("<XF86AudioMute>",         spawn "amixer sset Master toggle")
    , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 5 -steps 1")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5 -steps 1")
    ]
    ++
    [ (p ++ k, sendMessage $ f d)
        | (k, d) <- zip ["h", "j", "k", "l"] [L, D, U, R]
        , (f, p) <- [(Go, "M-"), (Swap, "M-S-")]
    ]
    ++
    [ (p ++ i, f i)
        | i      <- map show [0..9]
        , (f, p) <- [(toggleOrView, "M-"), (windows . W.shift, "M-S-")]
    ]
    ++
    [ (k, spawn $ "mpc " ++ a) | (k, a) <- zip
        ["M-S-.","<XF86AudioNext>","M-S-,","<XF86AudioPrev>","<Pause>","<XF86AudioPlay>"]
        (concatMap (replicate 2) ["next", "prev", "toggle"])
    ]
  where
    launchTerminal = spawn $ XMonad.terminal c

myLayout = avoidStruts
    $ mkToggle (single NBFULL)
    . mkToggle (single REFLECTY)
    . mkToggle (single REFLECTX)
    $ navigable tiled
    ||| navigable Grid
    ||| navigable (Mirror tiled)
    ||| simpleTabbed
  where
    navigable :: LayoutClass l w => l w -> ModifiedLayout WindowNavigation l w
    navigable = configurableNavigation noNavigateBorders
    tiled     = ResizableTall nmaster delta ratio slaves
    nmaster   = 1
    delta     = 3/100
    ratio     = 1/2
    slaves    = []

-- Place following dzen related functions under separate module?
dzenIcon :: String -> String
dzenIcon icon = "^i(" ++ icon ++ ".xbm)"

idIcon :: WorkspaceId -> String
idIcon wsid = ["mail","web","code","term","docs","art","pics","media","note","cpu"]
    !! (read wsid)

dzenBackground :: String
dzenBackground = "#fbf1c7"

dzenForeground :: String -> String -> String
dzenForeground fg = dzenColor fg dzenBackground

dzenFocus :: String -> String
dzenFocus = dzenForeground "#1d2021"

dzenOccupy :: String -> String
dzenOccupy = dzenForeground "#a89984"

dzenFree :: String -> String
dzenFree = dzenForeground "#ebdbb2"

dzenUrgent :: String -> String
dzenUrgent = dzenForeground "#cc241d"

layoutIcon :: String -> String
layoutIcon layout
    | layout == "ResizableTall"        = "tall"
    | layout == "Grid"                 = "grid"
    | layout == "Mirror ResizableTall" = "layout_mirror_tall"
    | "Tabbed" `isPrefixOf` layout     = "mouse_01"
    | otherwise                        = "layout_full"

main = do
    h      <- getHomeDirectory
    handle <- spawnPipe (h ++ "/ui/dzen2/panel")
    xmonad $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ defaultConfig
        { terminal           = "urxvtc"
        , modMask            = mod4Mask
        , workspaces         = (map show [1..9]) ++ ["0"]
        , borderWidth        = 2
        , normalBorderColor  = "#fbf1c7"
        , focusedBorderColor = "#d5c4a1"
        , keys               = myKeys
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , handleEventHook    = docksEventHook
        , logHook            = dynamicLogWithPP defaultPP
            -- Is it possible to somehow factor out these same endings here?
            { ppCurrent         = dzenFocus  . dzenIcon . ((h ++ i) ++) . idIcon
            , ppHidden          = dzenOccupy . dzenIcon . ((h ++ i) ++) . idIcon
            , ppHiddenNoWindows = dzenFree   . dzenIcon . ((h ++ i) ++) . idIcon
            , ppUrgent          = dzenUrgent . dzenIcon . ((h ++ i) ++) . idIcon
            , ppSep             = ","
            , ppWsSep           = "  "
            , ppTitle           = const ""
            , ppLayout          = dzenIcon . ((h ++ i) ++) . layoutIcon
            , ppOutput          = hPutStrLn handle
            }
        }
      where
        i               = "/ui/dzen2/icons/"
        myUrgencyConfig = urgencyConfig { suppressWhen = Focused }
        myUrgencyHook   = BorderUrgencyHook { urgencyBorderColor = "#cc241d" }
        myManageHook    = manageDocks <+> toggleHook "centerFloat" doCenterFloat
