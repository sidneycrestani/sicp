"Plug
call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'preservim/nerdtree'
Plug 'preservim/tagbar'
Plug 'dense-analysis/ale' "linting
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'junegunn/seoul256.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'jpalardy/vim-slime' "C-c C-c to run
call plug#end()

"information
set number
set wildmenu
set showcmd

"utility
set mouse=r
nmap <F3> :TagbarToggle<CR>
let g:UltiSnipsExpandTrigger="§"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
"NERDTree stuff
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") && v:this_session == "" | NERDTree | endif
map <F2> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

"aesthetics
set background=dark
colorscheme seoul256
let g:airline_theme='seoul256'
let g:airline_powerline_fonts = 1

"lisp/scheme
let g:slime_target = "vimterminal"
autocmd filetype lisp,scheme,art setlocal equalprg=scmindent.rkt
augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END
