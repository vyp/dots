#!/bin/sh

xset -b
xsetroot -cursor_name left_ptr
xrdb ~/dots/x/resources
sxhkd &
exec herbstluftwm
