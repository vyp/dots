set nocompatible
" vim: foldmethod=marker

" Plugins {{{1
" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

" Technically this directory does not exist, but I think some sort of
" `plug#begin` line is still needed.
call plug#begin('~/.vim/plugged')
let plugins_dir = "~/ui/vendor/vim/"

Plug plugins_dir.'vim-repeat'
Plug plugins_dir.'vim-surround'

" Extra:
Plug plugins_dir.'vim-buftabline'
Plug plugins_dir.'delimitMate'
Plug plugins_dir.'vim-commentary'
Plug plugins_dir.'targets.vim'

" Syntax:
Plug plugins_dir.'vim-better-whitespace'

" Themes:
Plug plugins_dir.'gruvbox'

call plug#end()

" Plugin Options {{{1
" 'ap/vim-buftabline' {{{2
let g:buftabline_indicators=1
let g:buftabline_numbers=1
let g:buftabline_show=1

" 'Raimondi/delimitMate' {{{2
let delimitMate_expand_cr=2
let delimitMate_expand_space=1
" Disable autocompleting quote pairs.
let delimitMate_quotes=""

" 'junegunn/vim-plug' {{{2
let g:plug_timeout=1000

" Basic {{{1
filetype plugin indent on
set encoding=utf-8
set ff=unix

syntax enable

" Turn on relative line numbers except for current line and uses absolute line
" numbers for insert mode.
" set number
" set relativenumber
" au InsertEnter * :set number norelativenumber
" au InsertLeave * :set number relativenumber

" Disable faux bold.
set t_md=

" Enable mouse in all modes.
set mouse=a

set ruler
set cursorline
set showcmd

" See `:help wildmode` for more.
set wildmenu

" To be able to remap as <Down> in command mode.
set wildcharm=<Tab>

set hidden
set splitbelow
set splitright
set ignorecase
set smartcase
set incsearch

set autoindent
set backspace=2

set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

set textwidth=80
set colorcolumn=81

" Always show status line.
set laststatus=2

" Retains cursor (column) position across buffer switches.
set nostartofline

" As much as possible of the last line in a window will be displayed.
set display+=lastline

" See `:help fo-table`.
" set formatoptions+=tc
set formatoptions+=tcroqnj

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

" Main Mappings {{{1
nnoremap <Bslash> <C-^>
let mapleader=" "

nnoremap ' :
xnoremap ' :

nnoremap - +
nnoremap + -
xnoremap - +
xnoremap + -
nnoremap q; q:k
xnoremap q; q:k
nnoremap q/ q/k
nnoremap gs :w<CR>
nnoremap <silent> <leader>s :set<space>hlsearch!<CR>

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

nnoremap ZZ <nop>
nnoremap ZQ <nop>

" Can't use <Tab> and <S-Tab> since <Tab> is the same as ctrl-i in vim.
" See <http://stackoverflow.com/a/14642074>.
nnoremap <C-n> :bn<CR>
nnoremap <C-p> :bp<CR>
nnoremap <leader>l :ls<CR>:b<space>
nnoremap <leader>bs :cex []<BAR>bufdo vimgrepadd @@g %<BAR>cw<s-left><s-left><right>
nnoremap <leader>f mmvipgq`m

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
inoremap <C-l> <Esc>l%%a

cnoremap <C-t> <C-e>
cnoremap <C-e> <Down>

" Allows opening files from the directory of the currently opened buffer
" quickly.
" From <http://vimcasts.org/episodes/the-edit-command/>.
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
nnoremap <leader>ew :e <C-R>=fnameescape(expand('%:h')).'/'<cr>
nnoremap <leader>es :sp <C-R>=fnameescape(expand('%:h')).'/'<cr>
nnoremap <leader>ev :vsp <C-R>=fnameescape(expand('%:h')).'/'<cr>

xnoremap <leader>cp "*y
xnoremap <leader>co "+y
nnoremap <leader>p "*p
nnoremap <leader>o "+p

nnoremap <leader>vc :edit ~/.vimrc<CR>
nnoremap <leader>vz :edit ~/.zshrc<CR>

" These only exist as a potential bonus for keyboards where the arrow keys are
" actually on or near the home row (such as the Poker II).
nnoremap <Right> :bn<CR>
nnoremap <Left> :bp<CR>
nnoremap <Up> :ls<CR>:b<space>
nnoremap <Down> <C-^>

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

" Folds {{{1
set fillchars="fold: "
set foldmethod=syntax
set foldlevel=99
set foldlevelstart=99

nnoremap <c-f> za
nnoremap <leader>z zM
nnoremap z<space> zR

" 'co' for 'Close all folds and Open Cursor fold'.
nnoremap co zMzv

nnoremap <c-s> zkzMzv
nnoremap <c-g> zjzMzv
nnoremap zp zC
nnoremap zu zO
nnoremap zh [z
nnoremap zl ]z

function! FoldText()
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g')
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = printf("%10s", lines_count)
  let foldtextstart = '+' . repeat('-', v:foldlevel*2) . line
  let rightalignby = winwidth(0) - 80

  if rightalignby < 0
    let rightalignby = 1
  endif

  let foldtextend = lines_count_text . repeat(' ', rightalignby)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn

  return foldtextstart . repeat(' ', winwidth(0)-foldtextlength) . foldtextend
endfunction

set foldtext=FoldText()

" Filetype {{{1
" CSS {{{2
function CSSOptions()
  setl fdm=marker fmr={,}
endfunction

au filetype css call CSSOptions()

" Gitcommit {{{2
function GitcommitOptions()
  setl tw=72 cc=73
endfunction

au filetype gitcommit call GitcommitOptions()

" Rust {{{2
function RustOptions()
  setl ts=2 sw=2 sts=2
endfunction

au filetype rust call RustOptions()

" Python {{{2
function PythonOptions()
  setl ts=4 sw=4 sts=4
  setl completeopt-=preview
endfunction

au filetype python call PythonOptions()

" Extra {{{1
" Allows to `:q` when opening multiple files via the command line without
" needing to open all buffers.
au VimEnter * nested call VisitLastBuffer()

fun! VisitLastBuffer()
  startinsert
  call feedkeys('')

  if (argc() > 1)
    last
    rew
  endif
endfun

" When switching buffers, preserve window view.
" From <http://vim.wikia.com/wiki/Avoid_scrolling_when_switch_buffers>.
if v:version >= 700
  au BufLeave * if !&diff | let b:winview = winsaveview() | endif
  au BufEnter * if exists('b:winview') && !&diff | call winrestview(b:winview) | unlet! b:winview | endif
endif

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

nnoremap <silent> <leader><leader> :call ScrollToPercent(g:centerlevel)<CR>
nnoremap z<CR> zt
nnoremap zt z<CR>
nnoremap zB zb
nnoremap <silent> zb :call ScrollToPercent(g:centerlevel, 1)<CR>

" Highlight group under cursor {{{2
command! SynName echo synIDattr(synID(line("."), col("."), 1), "name")
command! SynStack echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
command! SynTrans echo synIDattr(synIDtrans(synID(line('.'), col('.'), 1)), "name")

" Colors {{{1
" Highlight groups need to be at the end of vimrc so that they don't get
" overridden.

source ~/.vim/colors.vim
