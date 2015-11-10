# What

It may not be immediately obvious why GNU Stow is used to manage dotfiles even
after visiting its homepage.

While Stow refers to itself as a "package installation manager", for the
purposes of managing dotfiles, it may be easier to think of it as a 'symlink
manager'. I use it because it is significantly easier to use the `stow` command
instead of manually using `ln -s` on every file (or writing a script that
symlinks everything).

# How

For the purposes of managing dotfiles, although maybe not technically correct,
stow by default takes a directory as an argument. Taking the `vim` directory for
example (from the root of this repository):

``` shell
$ stow vim
```

run from the root of the repository will symlink everything in the `vim`
directory to the directory *above* the *current* directory. Assume for example
you have your dotfiles repository in the `~/dotfiles` directory, in this case
stow will symlink `~/dotfiles/vim/.vimrc` to `~/.vimrc`. i.e. The directory
*above* `~/dotfiles`, which is `~`.

Do not confuse this for thinking that stow will symlink the `~/dotfiles/vim`
directory itself (to `~/vim`).

# Advantages

The advantage of this setup compared to simply keeping the dotfiles in the root
of this repository is that an application's settings can be contained within a
directory and installed or removed separately from other applications.

If the `.vimrc` file was simply in the root of the repository, next to other
config files, one would have to manually `ln` or `rm` in order to maintain the
same level of flexibility. Otherwise, a wrapper around `ln -s` could be written,
but the wrapper would have to have knowledge of which dotfiles correspond to
which application. By using stow, this is already taken care of because the
dotfiles are already organised in separate directories.

Of course, you could just put all the dotfiles in one single directory if you
wanted to.

# Extras

## Uninstallation

``` shell
$ stow -D <directory>
```

## Ignoring

It is noteworthy to mention that stow can also be told to ignore particular
files/directories to ignore. i.e. They will not be symlinked. This is useful if
there are dotfiles particular to an application that *do not need* to be
symlinked above.

This can be done using a `.stow-local-ignore` file in the root of the
application specific directory. So for vim, its dotfiles are located in the
`vim` directory (which is `~/dotfiles/vim` on the filesystem following from the
above examples), so the stow ignore file will be at `vim/.stow-local-ignore`
(`~/dotfiles/vim/.stow-local-ignore`).

The file should have PCREs (one on each line) signifying paths to files or
directories that should be ignored. So for example to ignore *everything* in a
particular directory, the file can just have:

```
.*
```

Everything after a `#` on a line is considered a comment. The
`.stow-local-ignore` file itself is also ignored.

Search this repository for a few more examples. And of course consult the
official documentation for more information.
