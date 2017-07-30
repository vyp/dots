" vim: foldmethod=marker
" Basic Settings {{{1
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

" Enable mouse in all modes.
set mouse=a

set ruler
set cursorline
set showcmd
set scrolloff=5

" See `:help wildmode` for more.
set wildmenu

" To be able to remap as <down> in command mode.
set wildcharm=<tab>

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

set textwidth=79
set colorcolumn=80

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
nnoremap <bslash> <c-^>
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
nnoremap gs :w<cr>
nnoremap <silent> <leader>s :set<space>hlsearch!<cr>

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

nnoremap <c-n> :bn<cr>
nnoremap <c-p> :bp<cr>
nnoremap <leader>l :ls<cr>:b<space>
nnoremap <leader>f mmvipgq`m

nnoremap <leader>wo :only<cr>
nnoremap <leader>we <c-w>=

nnoremap <leader>re <c-l>
nnoremap <leader>ri <c-a>
nnoremap <leader>rd <c-x>
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
inoremap <c-l> <esc>l%%a

" Allows opening files from the directory of the currently opened buffer
" quickly.
" From <http://vimcasts.org/episodes/the-edit-command/>.
nnoremap <leader>ew :e <c-r>=fnameescape(expand('%:h')).'/'<cr>

xnoremap <leader>cp "*y
xnoremap <leader>co "+y
nnoremap <leader>p "*p
nnoremap <leader>o "+p

nnoremap <leader>eh :edit $HISTFILE<cr>
nnoremap <leader>vc :edit ~/.vimrc<cr>
nnoremap <leader>vz :edit ~/.zshrc<cr>

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

function! FoldText()
  let line = ' ' . substitute(getline(v:foldstart),
                              \ '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g')
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = printf("%10s", lines_count)
  let foldtextstart = '+' . repeat('-', v:foldlevel*2) . line
  let rightalignby = winwidth(0) - 79

  if rightalignby < 0
    let rightalignby = 1
  endif

  let foldtextend = lines_count_text . repeat(' ', rightalignby)
  let foldtextlength = strlen(substitute(foldtextstart .
                                         \ foldtextend, '.', 'x', 'g'))
                       \ + &foldcolumn

  return foldtextstart . repeat(' ', winwidth(0)-foldtextlength) . foldtextend
endfunction

set foldtext=FoldText()

" Filetype {{{1
" Gitcommit {{{2
function GitcommitOptions()
  setl tw=72 cc=73
endfunction

au filetype gitcommit call GitcommitOptions()

" Python {{{2
function PythonOptions()
  setl ts=4 sw=4 sts=4
endfunction

au filetype python call PythonOptions()

" Extra {{{1
" Change cursor shape based on vim mode:
"
"   - Block for normal mode.
"   - Bar for insert mode.
"   - Underline for replace mode if possible.
"
" Restricted mode doesn't allow shell commands.
let s:isRestricted = 0

try
  call system('')
catch /E145/
  let s:isRestricted = 1
endtry

if &term =~ "xterm\\|rxvt\\|st"
  if s:isRestricted
    " Insert mode shape.
    let &t_SI = "\x1b[\x36 q"
    " Normal mode shape.
    let &t_EI = "\x1b[\x32 q"
  else
    " The above is simpler but this also changes shape to underline for replace
    " mode.
    autocmd VimEnter,InsertLeave *
      \ silent execute '!echo -ne "\x1b[\x32 q"' | redraw!
    autocmd InsertEnter,InsertChange *
      \ if v:insertmode == 'i' |
        \ silent execute '!echo -ne "\x1b[\x36 q"' | redraw! |
      \ elseif v:insertmode == 'r' |
        \ silent execute '!echo -ne "\x1b[\x34 q"' | redraw! |
      \ endif
    endif
endif

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
au BufLeave * if !&diff | let b:winview = winsaveview() | endif
au BufEnter * if exists('b:winview') && !&diff | call winrestview(b:winview) |
  \ unlet! b:winview | endif

" Highlight group under cursor {{{2
command! SynName echo synIDattr(synID(line("."), col("."), 1), "name")
