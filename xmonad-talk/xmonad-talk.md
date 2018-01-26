% XMonad
% DATE

# What is XMonad

XMonad is a tiling window manager written in Haskell.
A tiling window manager manages the position, sizes and organization of the open windows. Though one does have some degree of freedom.

# Outside of Haskell

There are other tiling window managers:

* Awesome (written in `C` but configured in `Lua`)
* bspwm (written in `C`)
* dwm (written and configured in `C`)
* i3 (written in `C` configured in plain text)

# Outside of Linux

For MAC OS X:

* Amethyst
* chunkwm

And there should be something for Windows...

# How to install it

There are three (sane) options:

* `cabal`
* `stack`
* Your package manager.

# How does one configure it?

You need to have a file at `~/.xmonad/xmonad.hs`.

It needs a `main` that runs an `xmonad` and a customized [XConfig](https://hackage.haskell.org/package/xmonad-0.13/docs/XMonad-Core.html#t:XConfig)

# Useful tools

Status bars:

* [Polybar](https://github.com/jaagr/polybar)
* [Xmobar](https://github.com/jaor/xmobar)
* [Lemonbar](https://github.com/LemonBoy/bar)

Launch bars:

* [Rofi](https://github.com/DaveDavenport/rofi)
* [dmenu](https://tools.suckless.org/dmenu/)
