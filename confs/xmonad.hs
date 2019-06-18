-------------------------------------------------------------------------
-- | Example.hs
-- 
-- Example configuration file for xmonad using the latest recommended
-- features (e.g., 'desktopConfig').

module Main (main) where


import System.Exit
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Prompt
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing (smartSpacingWithEdge, spacing)
-- import XMonad.Prompt.ConfirmPrompt
-- import XMonad.Prompt.Shell
import XMonad.Util.EZConfig

import qualified XMonad.StackSet                as W
import qualified Data.Map                       as M
import qualified XMonad.Layout.Magnifier        as MG
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
-- import XMonad.Layout.Spacing (spacing)


main :: IO ()
main = do
  -- spawn "polybar top" -- Start a task bar.

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  xmonad $ ewmh $ desktopConfig
    {workspaces = ["1:dev","2:web","3:music","4:comm","5:ham","6:tmp"]
    , terminal = "urxvt"
    --, borderWidth = 2
    -- , focusedBorderColor = "#ffae1a"
    , modMask    = mod4Mask -- Use the "Win" key for the mod key
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = noBorders $ desktopLayoutModifiers myLayouts
    , focusFollowsMouse = False
    , logHook    = dynamicLogString def >>= xmonadPropLog
    , keys = myKeys
    , startupHook = setWMName "LG3D"
    }

--------------------------------------------------------------------------------
-- | My theme
--
-- This is just used for de top bar indicator.
myTheme = def{ decoHeight            = 5
             , inactiveBorderColor   = base03
             , inactiveColor         = base03
             , inactiveTextColor     = base03
             , activeBorderColor     = active
             , activeColor           = active
             , activeTextColor       = active
             }
base03 = "#282828"
active = "#50fa7b"

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- This layout configuration uses two primary layouts, 'ResizableTall'
-- and 'BinarySpacePartition'.  You can also use the 'M-<Esc>' key
-- binding defined above to toggle between the current layout and a
-- full screen layout.
myLayouts = highlightedLayouts ||| Full
  where
    highlightedLayouts = noFrillsDeco shrinkText myTheme $ smartSpacingWithEdge 4 $ (myTall ||| Mirror myTall)
    myTall = ResizableTall 1 (1.5/100) (3/5) []
    
--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook :: ManageHook
myManageHook = composeOne
  [ className =? "Pidgin" -?> doFloat
  , className =? "XCalc"  -?> doFloat
  , className =? "mpv"    -?> doFloat
  , className =? "Emacs"  -?> doShift "1:dev"
  , className =? "Firefox"  -?> doShift "2:web"
  , className =? "Thunderbird"  -?> doShift "2:web"
  , isDialog              -?> doCenterFloat

    -- Move transient windows to their parent:
  , transience
  ]
  where
    myDev = ["Emacs", "Zeal"]

-- My keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [
     ((modMask .|. shiftMask, xK_i     ), spawn "thunderbird") -- %! Run thunderbird
    ,((modMask .|. shiftMask, xK_m     ), spawn "iocane -i") -- %! Run iocane for moving the mouse
    ,((modMask,               xK_x     ), spawn "xchat") -- %! Run xchat
    ,((modMask,               xK_i     ), spawn "firefox") -- %! Run firefox
    ,((modMask ,              xK_Escape), spawn "slock") -- %! Run lock
    ,((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    ,((modMask .|. shiftMask, xK_p     ), spawn "rofi -show run") -- %! Launch gmrun
    ,((modMask,                xK_s    ), spawn "rofi -show window")
    ,((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window
    ,((modMask,               xK_e     ), spawn "emacsclient -c ") -- emacs client frame
    ,((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    ,((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    ,((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size
      -- Volume control
    --, ((modMask              , xK_F9     ),  void(lowerVolume 3))
    --, ((modMask              , xK_F10     ), void(raiseVolume 3))
    -- , ((modMask              , xK_F11     ), void toggleMute)
    -- move focus up or down the window stack
    ,((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    ,((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    ,((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    ,((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    ,((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    ,((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    ,((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    ,((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    ,((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    ,((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area
    ,((modMask .|. shiftMask,xK_h     ), sendMessage MirrorShrink) -- %! Shrink the master area
    ,((modMask .|. shiftMask,xK_l     ), sendMessage MirrorExpand) -- %! Expand the master area
    -- floating layer support
    ,((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    ,((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    ,((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    ,((modMask .|. shiftMask              , xK_plus ), sendMessage MG.MagnifyMore)
    ,((modMask .|. shiftMask              , xK_minus), sendMessage MG.MagnifyLess)
    ,((modMask                            , xK_x), spawn "xrandr --output eDP1 --primary --output HDMI1 --right-of eDPC1")
    ,((modMask .|. shiftMask               , xK_x), spawn "xrandr --output HDMI1 --off")

    -- quit, or restart
    ,((modMask .|. shiftMask, xK_q     ), io exitSuccess) -- %! Quit xmonad
    ,((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

