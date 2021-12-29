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
set colorcolumn=100
set t_vb=
set novisualbell
set nowritebackup
set nobackup
set noswapfile
set spell
set spelllang=en,cjk
set pumheight=10
set scrolloff=3
" set iskeyword-=_
set clipboard=unnamed,autoselect
" set t_Co=16
"set list
"set listchars=tab:»-,trail:-,extends:»,precedes:«,nbsp:%,eol:↲
if has('mouse')
  set mouse=a
endif
set wildmenu
set wildmode=longest:full,full
let mapleader = "\<space>"

" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

" Tab settings for each file type
augroup fileTypeIndent
  autocmd!
  autocmd BufNewFile,BufRead *.py setlocal tabstop=4 softtabstop=4 shiftwidth=4
  autocmd BufNewFile,BufRead *.md,*.markdown setlocal tabstop=4 softtabstop=4 shiftwidth=4
  autocmd BufNewFile,BufRead *.js setlocal tabstop=2 softtabstop=2 shiftwidth=2
  autocmd BufNewFile,BufRead *.html setlocal tabstop=2 softtabstop=2 shiftwidth=2
  autocmd BufNewFile,BufRead *.yaml,*.yml setlocal tabstop=2 softtabstop=2 shiftwidth=2
  autocmd BufNewFile,BufRead *.rb setlocal tabstop=2 softtabstop=2 shiftwidth=2
augroup END

if empty(glob('~/.vim/autoload/plug.vim'))
  call system('mkdir -p ~/.vim/autoload')
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/bundle')
    Plug 'scrooloose/nerdcommenter'
    Plug 'altercation/vim-colors-solarized'
    Plug 'arcticicestudio/nord-vim', { 'branch': 'develop' }
    Plug 'junegunn/vim-easy-align'
    Plug 'terryma/vim-expand-region'
    Plug 'LeafCage/yankround.vim'
    Plug 'plasticboy/vim-markdown'
    Plug 'tpope/vim-surround'
    Plug 'kana/vim-submode'
    Plug 'editorconfig/editorconfig-vim'
    Plug 'dhruvasagar/vim-table-mode'
    Plug 'justinmk/vim-sneak'
    Plug 'easymotion/vim-easymotion'
    Plug 'tpope/vim-repeat'
    " setup fzf
    if isdirectory("/usr/local/opt/fzf")
      Plug '/usr/local/opt/fzf'
    elseif isdirectory(expand("~/.fzf"))
      Plug '~/.fzf'
    endif
call plug#end()

" modify bgcolor on visual mode
augroup nord-theme-overrides
    autocmd!
    autocmd ColorScheme nord highlight Visual ctermbg=8
    autocmd ColorScheme nord highlight Comment ctermfg=243
    autocmd ColorScheme nord highlight LineNr ctermfg=243
augroup END
" diff color
let g:nord_uniform_diff_background = 1
" activate nord theme
colorscheme nord
" highlight for spell check
hi clear SpellBad
hi SpellBad cterm=underline
hi clear SpellCap
hi SpellCap cterm=underline,bold
hi markdownH1 ctermfg=3

" nerdcommenter
let NERDSpaceDelims = 1

" markdown
let g:vim_markdown_new_list_item_indent = 4
let g:vim_markdown_folding_disabled = 1
let g:table_mode_corner="|"

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
map <leader>h ^
map <leader>l $
" Go back to normal mode when typing jj quickly or <C-j>
inoremap jj <Esc>
inoremap <C-j> <Esc>
" Turn off highlight with <Esc><Esc>
nmap <silent> <Esc><Esc> :nohlsearch<CR>
" Search forward under a current cursor with *
vnoremap <silent> * "vy/\V<C-r>=substitute(escape(@v, '\/'), "\n", '\\n', 'g')<CR><CR>
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
noremap ; :
noremap : ;
map : <Plug>SneakNext
vmap ga <Plug>(EasyAlign)
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
nmap p <Plug>(yankround-p)
nmap P <Plug>(yankround-P)
nmap gp <Plug>(yankround-gp)
nmap gP <Plug>(yankround-gP)
nmap <C-p> <Plug>(yankround-prev)
nmap <C-n> <Plug>(yankround-next)
:command WClean %s/\s\+$//g
ca wc WClean
" key binds for vimdiff
if &diff
    nnoremap <leader>1 :diffget LOCAL<CR>
    nnoremap <leader>2 :diffget BASE<CR>
    nnoremap <leader>3 :diffget REMOTE<CR>
endif
" key binds for window configuration
nnoremap <leader>sj <C-w>j
nnoremap <leader>sk <C-w>k
nnoremap <leader>sl <C-w>l
nnoremap <leader>sh <C-w>h
nnoremap <leader>sJ <C-w>J
nnoremap <leader>sK <C-w>K
nnoremap <leader>sL <C-w>L
nnoremap <leader>sH <C-w>H
nnoremap <leader>sr <C-w>r
nnoremap <leader>sn gt
nnoremap <leader>sp gT
nnoremap <leader>s= <C-w>=
nnoremap <leader>sw <C-w>w
nnoremap <leader>so <C-w>_<C-w>|
nnoremap <leader>sO <C-w>=
nnoremap <leader>sN :<C-u>bn<CR>
nnoremap <leader>sP :<C-u>bp<CR>
nnoremap <leader>ss :<C-u>sp<CR>
nnoremap <leader>sv :<C-u>vs<CR>
nnoremap <leader>sq :<C-u>q<CR>
nnoremap <leader>sQ :<C-u>bd<CR>
call submode#enter_with('bufmove', 'n', '', '<leader>s>', '<C-w>>')
call submode#enter_with('bufmove', 'n', '', '<leader>s<', '<C-w><')
call submode#enter_with('bufmove', 'n', '', '<leader>s+', '<C-w>+')
call submode#enter_with('bufmove', 'n', '', '<leader>s-', '<C-w>-')
call submode#map('bufmove', 'n', '', '>', '<C-w>>')
call submode#map('bufmove', 'n', '', '<', '<C-w><')
call submode#map('bufmove', 'n', '', '+', '<C-w>+')
call submode#map('bufmove', 'n', '', '-', '<C-w>-')

