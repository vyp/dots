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

hi buftablineactive ctermfg=7 ctermbg=244

hi cursorline ctermbg=254
hi cursorlinenr ctermfg=241 ctermbg=254

hi folded ctermbg=254

hi htmlBoldUnderline term=NONE cterm=NONE
hi htmlBoldUnderlineItalic term=NONE cterm=NONE
hi htmlUnderlineItalic term=NONE cterm=NONE
hi htmlUnderline term=NONE cterm=NONE

hi linenr ctermfg=247 ctermbg=254

hi pandocAutomaticLink ctermfg=3
hi pandocCodeblock ctermfg=6
hi pandocEmphasis ctermfg=2
hi pandocEmphasisInStrong ctermfg=2
hi pandocNoFormatted ctermfg=6
hi pandocReferenceDefinition ctermfg=1
hi pandocReferenceDefinitionLabel ctermfg=1
hi pandocReferenceDefinitionAddress term=NONE cterm=NONE ctermfg=3
hi pandocReferenceLabel ctermfg=5
hi pandocReferenceURL ctermfg=1
hi pandocStrikeout ctermfg=245
hi pandocStrong ctermfg=3
hi pandocStrongEmphasis ctermfg=1
hi pandocStrongInEmphasis ctermfg=3

hi statusline ctermfg=7 ctermbg=6
hi statuslinenc ctermfg=7 ctermbg=244

hi tabline ctermfg=0 ctermbg=7
hi tablinefill ctermfg=0 ctermbg=7
hi tablinesel ctermfg=7 ctermbg=6

hi Underlined term=NONE cterm=NONE
hi visualNOS term=NONE cterm=NONE
