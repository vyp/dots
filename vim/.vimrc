set nocompatible
" vim: foldmethod=marker

" Options {{{1
filetype plugin indent on
set encoding=utf-8

" Turn on syntax highlighting.
syntax enable

" Turn on relative line numbers except for current line and uses absolute line
" numbers for insert mode.
set number
set relativenumber
au InsertEnter * :set number norelativenumber
au InsertLeave * :set number relativenumber

" Disable faux bold.
set t_md=

" Enable mouse in all modes.
set mouse=a

" Show cursor position in the form of ROW,COL at bottom right.
set ruler

" Highlights current line.
set cursorline

" Show information of current command at the bottom line.
set showcmd

" Interactive menu for command line completion. See `:help wildmode` for more.
set wildmenu

" To be able to remap as <Down> in command mode.
set wildcharm=<Tab>

" Hide current unsaved buffer when opening new files.
set hidden

" Open splits below current window, and vertical splits to the right of the
" current window.
set splitbelow
set splitright

" Searching is case-sensitive if a capital is used, otherwise it is not.
set ignorecase
set smartcase

" Move cursor to first match as search pattern is typed (without the need to
" press enter).
set incsearch

" Carry over indent level at new lines.
set autoindent

" Backspace works as 'normal'.
set backspace=2

" Insert spaces when tab key is pressed.
set expandtab

" Number of spaces inserted when tab key is pressed.
set tabstop=2

" Number of spaces inserted for indentation.
set shiftwidth=2

" Number of spaces deleted when backspace key is pressed.
set softtabstop=2

" Always show status line.
set laststatus=2

" Retains cursor (column) position across buffer switches.
set nostartofline

" As much as possible of the last line in a window will be displayed.
set display+=lastline

set textwidth=79
set colorcolumn=80

" set formatoptions+=tc " See `:help fo-table`.
set formatoptions+=tcroqnj " See `:help fo-table`.

" Less delay when pressing escape.
set timeoutlen=1000
set ttimeoutlen=10

" Vim info options:
" '0 --> Marks will not be saved.
" f0 -- Marks will not be saved.
" @0 --> Input line history will not be saved.
" No % --> Buffer list will not be saved.
" No :0 --> Command line history will be saved.
" No <0 --> Registers will be saved.
" No / --> Search history will be saved.
set viminfo='0,@0,f0

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
  let &t_SI = "\<Esc>]12;#d3d0c8\x7"
  " Normal mode color.
  let &t_EI = "\<Esc>]12;#f99157\x7"

  " Might be needed for resetting color when vim exits.
  " autocmd VimLeave * silent !echo -ne "\033]<color>\007"
  " Use "\003]12;<color>\007" for gnome-terminal.
endif

" Mappings {{{1
nnoremap <Bslash> ,
xnoremap <Bslash> ,
let mapleader=","

nnoremap ; :
nnoremap <cr> ;
nnoremap : ;
xnoremap ; :
xnoremap <cr> ;
xnoremap : ;

autocmd CmdwinEnter * nnoremap <cr> <cr>
autocmd CmdwinEnter * xnoremap <cr> <cr>
autocmd CmdwinLeave * nnoremap <cr> ;
autocmd CmdwinLeave * xnoremap <cr> ;

" TODO: Find the opposite of BufReadPost so that <CR> can be reset.
" autocmd BufReadPost quickfix nnoremap <CR> <CR>

nnoremap - +
nnoremap + -
xnoremap - +
xnoremap + -
nnoremap q; q:k
xnoremap q; q:k
nnoremap q/ q/k
nnoremap gs :w<CR>
nnoremap <leader>s <C-z>
nnoremap <silent> <leader>n :nohls<CR>
nnoremap <silent> <leader>m :set<space>hlsearch!<CR>

nnoremap <leader>k H
nnoremap <leader>j L
nnoremap H ^
xnoremap H ^
nnoremap L $
xnoremap L $
onoremap M %
nnoremap M %
xnoremap M %
nnoremap Y y$

nnoremap <leader><leader> <C-^>

" Can't use <Tab> and <S-Tab> since <Tab> is the same as ctrl-i in vim. See
" http://stackoverflow.com/a/14642074.
nnoremap <C-n> :bn<CR>
nnoremap <C-p> :bp<CR>
nnoremap <leader>l :ls<CR>:b<space>
nnoremap <Right> :bn<CR>
nnoremap <Left> :bp<CR>
nnoremap <Up> :ls<CR>:b<space>
nnoremap <Down> <C-^>
" nnoremap <leader>bx :w<CR>:bd<CR>

" When switching buffers, preserve window view.
" From http://vim.wikia.com/wiki/Avoid_scrolling_when_switch_buffers.
if v:version >= 700
  au BufLeave * if !&diff | let b:winview = winsaveview() | endif
  au BufEnter * if exists('b:winview') && !&diff | call winrestview(b:winview) | unlet! b:winview | endif
endif
nmap <leader>f ysiW
nmap <leader>fs ysiW*

nnoremap <leader>wo :only<CR>
nnoremap <leader>wu <C-w>_
nnoremap <leader>wi <C-w>|
nnoremap <leader>wh <C-w><
nnoremap <leader>wj <C-w>-
nnoremap <leader>wk <C-w>+
nnoremap <leader>wl <C-w>>
nnoremap <leader>wr <C-w>x
nnoremap <leader>we <C-w>=
nnoremap <leader>wn <C-w>n
nnoremap <leader>wv :vne<CR>

nnoremap <leader>re <C-l>
nnoremap <leader>ri <C-a>
nnoremap <leader>rd <C-x>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

cnoremap <C-t> <C-e>
cnoremap <C-e> <Down>

" Allows opening files from the directory of the currently opened buffer
" quickly.
" From http://vimcasts.org/episodes/the-edit-command/.
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
nnoremap <leader>ew :e <C-R>=fnameescape(expand('%:h')).'/'<cr>
nnoremap <leader>en :e ~/notes/
nnoremap <leader>es :sp <C-R>=fnameescape(expand('%:h')).'/'<cr>
nnoremap <leader>ev :vsp <C-R>=fnameescape(expand('%:h')).'/'<cr>

nnoremap <leader>cp "*y
xnoremap <leader>cp "*y
nnoremap <leader>co "+y
xnoremap <leader>co "+y

" Removes default key binding, see `:help tag-stack`.
nnoremap <C-t> "*p
nnoremap <leader>t "+p
nnoremap <leader>vc :edit ~/.vimrc<CR>
nnoremap <leader>vz :edit ~/.zshrc<CR>

" TODO: Remove this.
" nnoremap <leader>cx :!xelatex %<CR>
" nnoremap <leader>cv :!pandoc --template=$HOME/templates/latex.tex --toc --number-sections -V geometry:margin=1.25in -V fontfamily:libertine -o %f %<CR>
" nnoremap <leader>cw :!texcount %<CR>

" Center on search {{{2
" The percentage height of the window at which you wish to define 'the center'.
let g:centerlevel = 25

" nnoremap <silent> n nzv:call<space>ScrollToPercent(g:centerlevel)<CR>
" nnoremap <silent> N Nzv:call<space>ScrollToPercent(g:centerlevel)<CR>
" nnoremap <silent> <C-o> <C-o>zv:call<space>ScrollToPercent(g:centerlevel)<CR>
" nnoremap <silent> <C-i> <C-i>zv:call<space>ScrollToPercent(g:centerlevel)<CR>

let g:centeronsearchini = "zv:call ScrollToPercent(g:centerlevel)\<CR>"
let g:centeronsearch = "zv:call ScrollToPercent(g:centerlevel)\<CR>"
autocmd InsertEnter * let g:centeronsearch = "\<C-O>zv\<C-O>:call ScrollToPercent(g:centerlevel)\<CR>"
autocmd InsertLeave * let g:centeronsearch = "zv:call ScrollToPercent(g:centerlevel)\<CR>"

function! s:returnAndCenterMaybe()
  if getcmdtype() =~ "[/?]" && mode() != "v" && mode() != "\<C-V>"
    return g:centeronsearch
  else
    return ""
  endif
endfunction

function s:centerOnOperatorPendingSearch(key)
  let g:centeronsearch = ""
  return a:key
endfunction

" cnoremap <silent> <expr> <CR> "\<CR>" . ( <SID>returnAndCenterMaybe() )
" onoremap <expr> / <SID>centerOnOperatorPendingSearch("/")
" onoremap <expr> ? <SID>centerOnOperatorPendingSearch("?")
" nnoremap / :let<space>g:centeronsearch<space>=<space>g:centeronsearchini<CR>/
" nnoremap ? :let<space>g:centeronsearch<space>=<space>g:centeronsearchini<CR>?

" Like zz, but allows you to choose a level other than the middle of the window
" (i.e. at 50).
"
" From
" http://stackoverflow.com/questions/22877483/is-it-possible-to-always-overwrite-a-specific-built-in-command-with-a-particular.
function ScrollToPercent(percent, ...)
  let movelines=winheight(0)*a:percent/100
  
  if has("float") && type(movelines)==type(0.0)
    let movelines=float2nr(movelines)
  endif
  
  let oldso=&so
  execute ":set so=" . movelines

  if exists("a:1")
    execute "normal! zb"
  else
    execute "normal! zt"
  endif

  execute ":set so=" . oldso
endfunction

nnoremap <silent> <leader>z :call ScrollToPercent(g:centerlevel)<CR>
nnoremap z<CR> zt
nnoremap zt z<CR>
nnoremap zB zb
nnoremap <silent> zb :call ScrollToPercent(g:centerlevel, 1)<CR>
" }}}2

" Status Line {{{1
set statusline=
" Buffer number.
set statusline+=%3.3n\ »\ 

" Filename.
set statusline+=%f\ «\ 

" Status flags.
set statusline+=%h%m%r%w

" " File type.
" set statusline+=\[%{strlen(&ft)?&ft:'none'}]

" Right align the rest.
set statusline+=%=

" " Character value.
" set statusline+=0x%-8B

" Line, character.
set statusline+=%-14(%l,%c%V%)

" File position.
set statusline+=%<%P

" Folding {{{1
set foldmethod=syntax
set foldlevel=99
set foldlevelstart=99

nnoremap <space> za
nnoremap <leader><space> zM
nnoremap z<space> zR

" 'co' for 'Close all folds and Open Cursor fold'.
nnoremap co zMzv

nnoremap <C-s> zkzMzv
nnoremap <C-f> zjzMzv
nnoremap zp zC
nnoremap zu zO
nnoremap zh [z
nnoremap zl ]z

" From http://dhruvasagar.com/2013/03/28/vim-better-foldtext
" TODO: Use something better, possibly more simpler.
function! NeatFoldText()
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '| ' . printf("%10s", lines_count . ' lines') . ' |'
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextstart = strpart('+' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction

set foldtext=NeatFoldText()

" Filetype {{{1
" TODO: Fix this, i.e. make it more simpler.

au BufNewFile,BufRead *.css set fdm=marker fmr={,}

au FileType gitcommit setl tw=72 cc=73

" au FileType html call CheckIfMD()
"
" Enables folding in javascript files, from the 'jelera/vim-javascript-syntax'
" plugin.
au FileType javascript call JavaScriptFold()

au BufRead,BufNewFile *.md set filetype=pandoc.markdown
au BufRead,BufNewFile *.pd set filetype=pandoc.markdown
au Filetype pandoc.markdown setl ts=4 sw=4 sts=4

" More characters: ▼ ▾ ▲ ▴
au Filetype pandoc.markdown syntax match htmlEntityalpha /&alpha;/ conceal cchar=α
au Filetype pandoc.markdown syntax match htmlEntityApprox /&approx;/ conceal cchar=≈
au Filetype pandoc.markdown syntax match htmlEntitybeta /&beta;/ conceal cchar=β
au Filetype pandoc.markdown syntax match htmlEntityDegree /&deg;/ conceal cchar=°
au Filetype pandoc.markdown syntax match htmlEntityDownArrow /&darr;/ conceal cchar=▾
au Filetype pandoc.markdown syntax match htmlEntityGreaterThan /&gt;/ conceal cchar=>
au Filetype pandoc.markdown syntax match htmlEntityLeftArrow /&larr;/ conceal cchar=←
au Filetype pandoc.markdown syntax match htmlEntityLessThan /&lt;/ conceal cchar=<
au Filetype pandoc.markdown syntax match htmlEntitymicro /&micro;/ conceal cchar=µ
au Filetype pandoc.markdown syntax match htmlEntityNonBreakingSpace /&nbsp;/ conceal cchar=╌
au Filetype pandoc.markdown syntax match htmlEntityPi /&pi;/ conceal cchar=π
au Filetype pandoc.markdown syntax match htmlEntityRightArrow /&rarr;/ conceal cchar=→
au Filetype pandoc.markdown syntax match htmlEntityUpArrow /&uarr;/ conceal cchar=▴
au Filetype pandoc.markdown syntax match htmlEntityTherefore /&there4;/ conceal cchar=∴
au Filetype pandoc.markdown syntax match htmlEntityTimes /&times;/ conceal cchar=×

au FileType python setl ts=4 sw=4 sts=4

" function! CheckIfMD()
"   if match(getline(1,"$"),'<article class="md">') >= 0
"     set filetype=markdown
"   endif
" endfunction

" Extra {{{1
" Allows to `:q` when opening multiple files via the command line without
" needing to open all buffers.
au VimEnter * nested call VisitLastBuffer()

fun! VisitLastBuffer()
  if (argc() > 1)
    last
    rew
  endif
endfun

" Plugins {{{1
" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

call plug#begin('~/.vim/plugged')

" Essential:
Plug 'godlygeek/tabular'
Plug 'mattn/emmet-vim'
Plug 'Shougo/neocomplete'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

" Extra:
Plug 'ap/vim-buftabline'
Plug 'Raimondi/delimitMate'
Plug 'tomtom/tcomment_vim'
Plug 'wellle/targets.vim'

" Syntax:
Plug 'jelera/vim-javascript-syntax'
Plug 'nelstrom/vim-markdown-folding'
Plug 'rust-lang/rust.vim'
Plug 'vim-pandoc/vim-pandoc-syntax'

" Theme:
Plug 'chriskempson/base16-vim'

call plug#end()

" TODO: +snippets, fugitive.

" Plugin options {{{1
" 'ap/vim-buftabline' {{{2
let g:buftabline_indicators=1
let g:buftabline_numbers=1
let g:buftabline_show=1

" 'godlygeek/tabular' {{{2
" TODO: Move to 'Filetype' section.
au FileType markdown nnoremap <silent> <leader><Tab> :Tabularize /<Bar><CR>

" 'junegunn/vim-plug' {{{2
" 'Valloric/YouCompleteMe' takes a long time clone (recursively), so this stops
" it from potentially timing out and thus failing to install.
"
" If it _still_ fails, clone manually:
"
"     cd ~/.vim/plugged
"     git clone https://github.com/Valloric/YouCompleteMe
"     cd YouCompleteMe
"     git submodule update --init --recursive
"
" Don't forget to compile the ycm_support_libs! (See README.md for how.)
let g:plug_timeout=1000 "

" 'nelstrom/vim-markdown-folding' {{{2
" let g:markdown_fold_style = 'nested'
let g:markdown_fold_override_foldtext = 0

" 'Raimondi/delimitMate' {{{2
let delimitMate_expand_cr=1
inoremap <C-l> <Esc>l%%a

" 'Shougo/neocomplete' {{{2
let g:acp_enableAtStartup = 0
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyword_length = 4

" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#close_popup() . "\<CR>"
endfunction

" <TAB>: completion.
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

" <C-h>, <BS>: close popup and delete backword char.
" inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
" inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" Close popup by <Space>.
inoremap <expr><Space> pumvisible() ? neocomplete#close_popup() . "\<Space>" : "\<Space>"

" TODO: Enable omni completion.
" autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
" autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
" autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
" autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
" autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
" if !exists('g:neocomplete#sources#omni#input_patterns')
"   let g:neocomplete#sources#omni#input_patterns = {}
" endif
"let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
"let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
"let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" 'Valloric/YouCompleteMe' {{{2
" TODO: Remove this plugins' settings.
" Removes all default blacklists.
let g:ycm_filetype_blacklist = {}
let g:ycm_min_num_of_chars_for_completion = 3

" 'vim-pandoc/vim-pandoc-syntax' {{{2
" Disable underlining of superscript, subscript and strikeout delimited text.
let g:pandoc#syntax#style#underline_special = 0

" Colors {{{1
" set background=dark
" TODO: Change to custom theme.
colorscheme base16-eighties

" Highlight groups need to be at the end of vimrc so that they don't get
" overridden.

" base16-eighties:
hi pandocBlockQuote ctermfg=12
hi pandocEmphasis ctermfg=5
hi pandocEmphasisInStrong ctermfg=5
hi pandocStrikeout ctermfg=8
hi pandocStrong ctermfg=14
hi pandocStrongEmphasis ctermfg=9
hi pandocStrongInEmphasis ctermfg=14
 
" " solarized-light:
" hi buftablineactive term=NONE cterm=NONE ctermfg=10 ctermbg=0
" hi buftablinecurrent term=NONE cterm=NONE ctermbg=0
" hi buftablinefill term=NONE cterm=NONE ctermbg=8
" hi buftablinehidden term=NONE cterm=NONE ctermbg=8
" hi folded term=NONE cterm=NONE
" " hi htmlEntityTherefore ctermfg=1
" hi matchparen term=NONE cterm=NONE ctermfg=230 ctermbg=5
" hi pandocBlockQuote ctermfg=13
" hi pandocEmphasis ctermfg=4
" hi pandocStrikeout term=NONE cterm=NONE
" hi pandocStrong ctermfg=3
" hi pandocStrongEmphasis ctermfg=5
" hi search term=NONE cterm=NONE ctermfg=230 ctermbg=3
