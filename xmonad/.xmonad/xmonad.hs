import XMonad

-- TODO: Find the defaultConfig code.
-- TODO: Next/previous occupied/free/urgent workspace.
-- TODO: 10 workspaces.
-- TODO: Go back to previous workspace if win + the number of the current
-- workspace is pressed. Allows for quick switching/toggling of workspace views.
-- TODO: Cartesian window navigation/movement.
-- TODO: Change ratio/height of windows vertically?
-- TODO: 'Easymotion'-esque window navigation?
-- TODO: Check/fix keybindings. (Is it possible to have 'hydra-like'
-- keybindings?)
-- TODO: Preselect new window placement?
-- TODO: Theme/colours (e.g. window border colour) in a different file.
-- TODO: Panel!

myTerminal = "urxvtc"
myModMask  = mod4Mask

myBorderWidth = 2
myNormalBorderColor  = "#fbf1c7"
myFocusedBorderColor = "#d5c4a1"

main = xmonad defaultConfig
    { terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    }
