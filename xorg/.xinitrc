#!/bin/sh

xrdb -I$HOME ~/.Xresources

# Be sure to run `mkfontscale` and then `mkfontdir` in these directories when
# installing fonts for the first time.
for dir in /usr/share/fonts/artwiz-fonts ~/.fonts/xlsfonts; do
  xset +fp "$dir";
done && xset fp rehash

# Disables terminal beeps (if otherwise they would be produced).
xset -b

xsetroot -cursor_name left_ptr &
synclient HorizTwoFingerScroll=1 &

setxkbmap -option ''
setxkbmap -option 'ctrl:nocaps'
xmodmap -e 'keycode 36 = Super_R'
xmodmap -e 'keycode any = Return'
xcape -e 'Super_R=Return'
xcape -e 'Control_L=Escape'
xcape -e 'Control_R=Escape'

[ -s ~/.set-wallpaper ] && . ~/.set-wallpaper &

disableInputDevice () {
  if xinput list | grep -q "$1"; then
    device=`xinput list | grep "$2"`
    xinput float `echo "$device" | sed 's/.*\(id=[0-9]\+\).*/\1/' | cut -c 4-`
  fi
}

tchscrinput="Atmel Atmel maXTouch Digitizer"

disableInputDevice "Poker II" "AT Translated Set 2 keyboard"
disableInputDevice "$tchscrinput" "$tchscrinput"

urxvtd &
sxhkd &

exec bspwm
