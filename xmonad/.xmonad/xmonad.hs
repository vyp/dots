import XMonad

main = xmonad defaultConfig
    { terminal    = "urxvtc"
    , modMask     = mod4Mask
    , borderWidth = 2
    }
