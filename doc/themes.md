# What

The `themes` directory is special in that it does not directly contain configs,
except it contains further directories which contain *theme-specific* configs.

This started as a not so serious 'would be nice to have' idea, but has now
actually become something usable.

The idea was that it would be nice to be able to switch 'themes' *without
affecting the state of the repository*. By which I mean being able to change the
themes of applications without making the git status 'dirty', i.e. there would
be no need to make a new commit and update the 'currently chosen theme'
everytime I wanted to switch themes. And that of course would (psychologically)
allow switching themes multiples times a day, compared to maybe a few times a
week or month (because you would not want to clutter the history with 'useless'
commits).

But what does it mean to change the 'theme' of an application? It predominantly
means to just change the colours. As you can imagine, this depends on the
application and whether or not it provides a declarative way to set colours.
Therefore only choose programs that do!

# Why

I find myself wanting to switch themes/colours quite often. It would not make
sense to commit a new choice everytime I change my mind because that does not
necessarily mean that the previous choice was 'incorrect' or 'bad'.

# How

The `bin/set-theme` script is a small stow wrapper that makes it easier to
dynamically switch themes. (See </doc/stow.md> for how stow works.)

The idea is to stow config files which only have theme-specific settings, and
then the main configuration files will 'source'/'import' (or similar) the
*symlinks* of these files, instead of the files themselves directly. This means
that the main configuration files will source the currently stow theme files,
and thus load the correct theme.

## Gotchas

As you may have guessed, the above would not be possible for an application that
does not provide a way to 'source' or 'import' another configuration file.
Fortunately, this is not the case for the majority of applications that I use.
So it is feasible to employ a simple build system instead to take care of the
ones that do not.

See the `Makefile` for an example of how such a build system is implemented.
