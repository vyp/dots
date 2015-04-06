# Arch

The usual ["Arch Philosophy"][1] applies. It provides a solid, but simply and
easy base to build upon, meaning of course two things:

1. Fairly minimal, but has the basic GNU utilities on top of the Linux kernel,
   to get you going.

2. But not too much after that. This means you do not have to worry about
   things you do not want like in many other typical distributions like Ubuntu.
   Or at the least, you do not have to waste time *removing* software you do
   not want, meaning setting up a new system from scratch as outlined in
   [installation.md][2] takes a **lot less** time.

The binary packages as distributed through the official repositories also save
time, as opposed to source based distributions like Gentoo. And the AUR allows
Arch software availability to rival that of Debian.

But I think the greatest reason is the brilliant [Magnus Therning][3] and co
provide the *unofficial* [ArchHaskell][4] repository. Now of course, as you all
know, [cabal-install is not a package manager][5], so it turns out this
unofficial repository is actually really useful. In fact, it aims to provide
the entirety of [Hackage][6]!

This is a major advantage because it means I do not have to compile pandoc
everytime a new version comes out, which is signficant because pandoc is one of
the most important packages I use.

However, I can never guarantee that the decision will remain with Arch.
Although there may be some Arch specific settings, most of the files should be
compatible with practically any GNU distribution. (But I do not care about BSD,
Mac, Windows, POSIX etc. So I will not go out of my way to maintain
compatibility with any of that.) Some distributions I recommend are Debian
unstable, Gentoo and CRUX.

I also really like [GuixSD][7], or at least the idea behind it, and perhaps one
day I might even switch to it.

[1]: https://wiki.archlinux.org/index.php/The_Arch_Way
[2]: installation.md
[3]: http://therning.org/magnus/
[4]: https://wiki.archlinux.org/index.php/ArchHaskell
[5]: https://ivanmiljenovic.wordpress.com/2010/03/15/repeat-after-me-cabal-is-not-a-package-manager/
[6]: https://hackage.haskell.org/
[7]: http://www.gnu.org/software/guix/manual/guix.html#GNU-Distribution
