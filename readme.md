<!-- vim: syntax=off
-->
# #goals

## 🚀 Free Software

- I should be able to use, study, modify, and share my modifications of all the
  software used in the setup. 🙈🙉🙊

**[GNU]** is the chosen OS due to it's unequivocal commitment to [free
software][free-sw]. Grand acclamation to **[Dr. Richard Stallman][rms]** for
creating it and for creating the entire free software ideology and culture. I
can imagine the world would be a *much* worse place if he did not! 👏👏👏

[free-sw]: https://www.gnu.org/philosophy/free-sw.en.html
[GNU]:     https://www.gnu.org/gnu/gnu.en.html
[rms]:     https://stallman.org/

## 🥗 Reproducibility

- I should be able to easily but flexibly use exactly the same setup across my
  various machines. 🖥️💻👩‍💻

**[NixOS]** is the chosen distribution because of it's clean, functional and
declarative approach to package and configuration management. Thanks to
**[@edolstra]** for creating Nix, which is the package manager that NixOS uses
to perform it's magic. ✨

I think that the world would be a much better place if more people used Nix!

---

Shoutout to **[@civodul]** who created **[Guix]** (and GuixSD), which I think
is better than Nix because it uses Scheme (**[Guile]**) instead of the Nix
expression language.

<details>
  <summary>Excuses excuses... 😒</summary>

  I would use GuixSD instead of NixOS if it had all the packages I use, and if
  I didn't have to build so many things from GuixSD. I regularly use small
  laptops with limited RAM that simply run out of memory before being able to
  upgrade all packages on GuixSD, because Guix tends to require building so
  many things, making it impossible to use on these machines. Nix on the other
  hand very rarely requires it. 🤷‍
</details>

[@civodul]:  https://github.com/civodul
[@edolstra]: https://github.com/edolstra
[Guile]:     https://www.gnu.org/software/guile/
[Guix]:      https://www.gnu.org/software/guix/
[NixOS]:     https://nixos.org

## 💅 Mouseless Workflow

- I should be (physically) comfortable and safe when using my setup. 🛋️💆‍♀️

Obviously this highly depends on *you* and what you normally use a (general
purpose) computer for. For me, this means being able to keep my fingers on the
home row as much as possible as typing is the principle and most frequent
action in my usual workflow.

That means having keyboard shortcuts to do everything (instead of requiring
mouse or touchpad usage). Additionally, the keyboard shortcuts should be
composable and/or modal instead of requiring extensive chording (in the spirit
of the **[Vi editor][vi]** from **[Bill Joy]**), so that they remain
comfortable to press.

### 🏨 Window Manager

The first point of interaction after logging in is usually the window manager.
I've found that **[herbstluftwm]**'s model of window management is the perfect
balance of manual and automatic tiling window management *for me*. A very big
thank you to **[@t-wissmann]** for creating it, and to the rest of the team
(**[@The-Compiler]** and **[@ypnos]**) for helping with it's continued
development. 👍

*TODO: Add comparison to other popular window managers and explain why
herbstluftwm was chosen over them.*

### ✒️ Editor

Because typing is so prominent in my workflow, having a decent text editor to
follow the above goals is paramount. **[@brammool]** created **[Vim]**, an
essentially 'modernized' version of Vi, which I have as my `$EDTIOR`.

But text editing is only so much in my typical workflow, so Vim is not enough.
**[Emacs]** is a text editor created by Dr. Richard Stallman that can do a lot
more than just text editing, and as such is perfect for providing an integrated
keyboard-controlled environment for many things not limited to just text
editing. However, it makes extended use of chorded keybindings by default. 😕

But not to worry! Dr. Richard Stallman made emacs so powerful that, unbeknownst
to him, it could emulate just about all of vim.. before vim was even created!
And that's exactly what **[@epsil]** did when he created **[Evil mode]**. So I
actually use emacs as my *real* `$EDITOR`, because it does a better job at
being vim than vim itself! 😈

[Bill Joy]:      https://en.wikipedia.org/wiki/Bill_Joy
[@brammool]:     https://github.com/brammool
[herbstluftwm]:  http://www.herbstluftwm.org
[Emacs]:         https://www.gnu.org/software/emacs
[@epsil]:        https://github.com/epsil
[Evil Mode]:     https://github.com/emacs-evil/evil
[@t-wissmann]:   https://github.com/t-wissmann
[@The-Compiler]: https://github.com/The-Compiler
[vi]:            https://en.wikipedia.org/wiki/Vi
[Vim]:           http://www.vim.org/
[@ypnos]:        https://github.com/ypnos

## 💄 Rice

- I should be looking at a pretty screen, especially as I look at it a lot. 😍

<details>
  <summary>Obviously this is highly subjective, but click me for some samples.
  😎</summary>

  *TODO: Add screenshots here.*
</details>

## ♻️ Minimalism

- I should not be wasting time with unnecessary stuff. ⏳
