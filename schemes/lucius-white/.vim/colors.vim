" Change cursor color for insert mode. Not sure if options exist for other
" modes. (Doesn't seem to.)
" Apparently only for xterm/rxvt/terminator/gnome-terminal.
" From http://vim.wikia.com/wiki/Configuring_the_cursor.
if &term =~ "xterm\\|rxvt"
  " Use for changing cursor color at vim startup. (For example if your prompt's
  " cursor color is not the same as the desired normal mode color.)
  " silent !echo -ne "\033]12;\#268bd2\007"

  " See `:help t_SI` and `:help t_EI`.
  " Insert mode color.
  let &t_SI = "\<Esc>]12;#5f87af\x7"
  " Normal mode color.
  let &t_EI = "\<Esc>]12;#444444\x7"

  " Might be needed for resetting color when vim exits.
  " autocmd VimLeave * silent !echo -ne "\033]<color>\007"
  " Use "\003]12;<color>\007" for gnome-terminal.
endif

colorscheme lucius
LuciusWhite

hi conceal term=reverse cterm=reverse
hi pandocStrikeout ctermfg=242
hi tabline ctermfg=0 ctermbg=7
hi tablinefill ctermfg=0 ctermbg=7
hi pmenusel ctermfg=0 ctermbg=252
hi tablinesel ctermfg=7 ctermbg=0

" hi pandocBlockQuote ctermfg=12
" hi pandocEmphasis ctermfg=5
" hi pandocEmphasisInStrong ctermfg=5
" hi pandocStrikeout ctermfg=8
" hi pandocStrong ctermfg=14
" hi pandocStrongEmphasis ctermfg=9
" hi pandocStrongInEmphasis ctermfg=14
