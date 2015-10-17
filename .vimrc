set nocompatible
set number
set notitle
set ruler
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set incsearch
set ignorecase
set smartcase
set shiftround
set hlsearch
set virtualedit=block
set expandtab
set backspace=indent,eol,start
set wrap
set textwidth=0
set colorcolumn=80
set t_vb=
set novisualbell
set nowritebackup
set nobackup
set noswapfile
set spelllang+=cjk
set spell
set pumheight=10
set scrolloff=3
set iskeyword-=_
set clipboard=unnamed,autoselect
set t_Co=16
"set list
"set listchars=tab:»-,trail:-,extends:»,precedes:«,nbsp:%,eol:↲
set mouse=a
let mapleader = "\<space>"
" Neobundle
" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

if has('vim_starting')
  if &compatible
    set nocompatible
  endif

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
  call neobundle#begin(expand('~/.vim/bundle/'))
endif

" Required:

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
" Refer to |:NeoBundle-examples|.
" Note: You don't set neobundle setting in .gvimrc!
NeoBundle 'Shougo/unite.vim'
" NeoBundle 'Shougo/vimproc'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'ujihisa/unite-colorscheme'
NeoBundle 'tomasr/molokai'
NeoBundle 'junegunn/vim-easy-align'
NeoBundle 'terryma/vim-expand-region'

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

syntax on
set background=dark
let g:solarized_termtrans=1
colorscheme solarized

" Key bind
"""" emacs-like key bind in insert mode
imap <c-a> <home>
imap <c-e> <end>
imap <c-d> <del>
imap <c-k> <esc>lc$
imap <c-y> <esc>pa
imap <c-b> <left>
imap <c-f> <right>
"""" 
map gh ^
map gl $
" Go back to normal mode when typing jj quickly
inoremap jj <Esc>
" Turn off highlight with <Esc><Esc>
nmap <silent> <Esc><Esc> :nohlsearch<CR>
" Search forward under a current cursor with *
vnoremap <silent> * "vy/\V<C-r>=substitute(escape(@v, '\/'), "\n", '\\n', 'g')<CR><CR>
nnoremap j gj
nnoremap k gk
nmap n nzz
nmap N Nzz
nmap * *zz
nmap # #zz
nmap g* g*zz
nmap g# g#zz
noremap ; :
noremap : ;
vmap <Enter> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)
nnoremap + <C-a>
nnoremap - <C-x>
" Reinden all
nnoremap <leader>i gg=<S-g><C-o><C-o>zz
" Select all text in current line
nnoremap <leader>v 0v$h
" Delete all text in current line
nnoremap <leader>d 0v$hx
" Yank all text in current line
nnoremap <leader>y 0v$hy
