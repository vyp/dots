" Change cursor color for insert mode. Not sure if options exist for other
" modes. (Doesn't seem to.)
" Apparently only for xterm/rxvt/terminator/gnome-terminal.
" From http://vim.wikia.com/wiki/Configuring_the_cursor.
if &term =~ "xterm\\|rxvt"
  " Use for changing cursor color at vim startup. (For example if your prompt's
  " cursor color is not the same as the desired normal mode color.)
  " silent !echo -ne "\033]12;\<color>\007"
  "
  " That might change the terminal contents after exiting vim though. Another
  " potential method may be:
  "
  "     au VimEnter * nested call FixCursorColorUponStartup()
  "
  "     fun! FixCursorColorUponStartup()
  "       startinsert
  "       call feedkeys('')
  "     endfun

  " See `:help t_SI` and `:help t_EI`.
  " Insert mode color.
  let &t_SI = "\<Esc>]12;#928374\x7"
  " Normal mode color.
  let &t_EI = "\<Esc>]12;#3c3836\x7"

  " Might be needed for resetting color when vim exits.
  " autocmd VimLeave * silent !echo -ne "\033]<color>\007"
  " Use "\003]12;<color>\007" for gnome-terminal.
endif

colorscheme gruvbox

hi buftablineactive ctermfg=246 ctermbg=223
hi error ctermfg=1 ctermbg=NONE
hi link extrawhitespace cursorcolumn
hi link shtodo vimtodo
hi link zshtodo vimtodo
hi vimtodo ctermfg=136 ctermbg=223
hi visual term=NONE cterm=NONE ctermbg=223
hi visualnos term=NONE cterm=NONE ctermbg=223
