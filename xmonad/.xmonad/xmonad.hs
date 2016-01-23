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
-- TODO: Theme/colours => window border colour.
-- TODO: Panel!

main = xmonad defaultConfig
    { terminal    = "urxvtc"
    , modMask     = mod4Mask
    , borderWidth = 2
    }
