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

setxkbmap -option 'ctrl:nocaps' &
xcape -e 'Control_L=Escape' &
xcape -e 'Control_R=Escape' &
setxkbmap -option 'lv3:rwin_switch' &
spare_modifier='Hyper_L'
xmodmap -e "keycode 36 = $spare_modifier" &
xmodmap -e "remove Mod4 = $spare_modifier" &
xmodmap -e "add Mod3 = $spare_modifier" &
xmodmap -e 'keycode any = Return' &
xcape -e "$spare_modifier=Return" &

. ~/.fehsetrootrc &

disableInputDevice () {
  if xinput list | grep -q "$1"; then
    device=`xinput list | grep "$2"`
    xinput float `echo "$device" | sed 's/.*\(id=[0-9]\+\).*/\1/' | cut -c 4-`
  fi
}

tchscrinput="Atmel Atmel maXTouch Digitizer"

disableInputDevice "Poker II" "AT Translated Set 2 keyboard"
disableInputDevice "$tchscrinput" "$tchscrinput"

# Because mainly only use gui emacs, and seems to make login slower if put in
# ~/.zprofile, even with the ampersand at the end.
emacs --daemon &

urxvtd &
sxhkd &

exec bspwm
