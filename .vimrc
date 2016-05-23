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
set runtimepath+=/usr/local/opt/fzf
set spelllang+=cjk
set spell
set pumheight=10
set scrolloff=3
" set iskeyword-=_
set clipboard=unnamed,autoselect
set t_Co=16
"set list
"set listchars=tab:»-,trail:-,extends:»,precedes:«,nbsp:%,eol:↲
set mouse=a
set wildmenu
set wildmode=full
let mapleader = "\<space>"
" Neobundle
" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/bundle')
    Plug 'Shougo/unite.vim'
    " Plug 'Shougo/vimproc'
    Plug 'scrooloose/nerdcommenter'
    Plug 'altercation/vim-colors-solarized'
    Plug 'ujihisa/unite-colorscheme'
    Plug 'tomasr/molokai'
    Plug 'junegunn/vim-easy-align'
    Plug 'terryma/vim-expand-region'
    Plug 'LeafCage/yankround.vim'
call plug#end()

" syntax on
set background=dark
let g:solarized_termtrans=1
colorscheme solarized

" nerdcommenter
let NERDSpaceDelims = 1

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
vnoremap j gj
vnoremap k gk
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
" Reindent all
nnoremap <leader>i gg=<S-g><C-o><C-o>zz
" Select all text in current line
nnoremap <leader>v 0v$h
" Delete all text in current line
nnoremap <leader>d 0v$hx
" Yank all text in current line
nnoremap <leader>y 0v$hy
" nerdcommenter
nmap <leader><Space> <Plug>NERDCommenterToggle
vmap <leader><Space> <Plug>NERDCommenterToggle
nmap p <Plug>(yankround-p)
nmap P <Plug>(yankround-P)
nmap gp <Plug>(yankround-gp)
nmap gP <Plug>(yankround-gP)
nmap <C-p> <Plug>(yankround-prev)
nmap <C-n> <Plug>(yankround-next)
