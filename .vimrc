
" vim: foldmarker={{{,}}}
" ============================================================================
" BASIC SETTINGS {{{
" ============================================================================
syntax on

if has('patch-7.4.1778')
  set guicolors
endif
if has('nvim')
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

" python scripts execution
imap <F32> <Esc>:!python 
nmap <F32> :!python 
vmap <F32> :!python 

" enable all Python syntax highlighting features
let python_highlight_all = 1

" disable synaptic scoll in vim
augroup scroll
    au!
    au  VimEnter * :silent !synclient VertEdgeScroll=0
    au  VimLeave * :silent !synclient VertEdgeScroll=1
augroup END

" identation for python files
au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

au BufNewFile,BufRead *.js
    \ set filetype=javascript |
    \ set tabstop=2 |
    \ set softtabstop=2 |
    \ set shiftwidth=2

" identation for html and css files
au BufRead,BufNewFile *.html; *.css
    \ set tabstop=2 |
    \ set softtabstop=2 |
    \ set shiftwidth=2

set virtualedit=block
set nojoinspaces
set expandtab smarttab

set formatoptions+=1
if has('patch-7.3.541')
  set formatoptions+=j
endif
if has('patch-7.4.338')
  let &showbreak = '↳ '
  set breakindent
  set breakindentopt=sbr
endif

set foldmethod=indent
set nofoldenable
set synmaxcol=1000
set mouse=c
set report=0
set encoding=utf-8
set completeopt=longest,menu,preview
scriptencoding utf-8

set novb
"disable peep
"set noeb vb t_vb=

set autoread                                                  " automatically load file when its changed
set viewoptions=folds,options,cursor,unix,slash
set history=1000

set spelllang=en,de_at
set spellfile=~/.vim/spell/custom.utf-8.add
set nospell

set nobackup
set nowritebackup
set backupdir=~/.vim/tmp

set noswapfile
set noautowrite
set lazyredraw

set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace
set fillchars=vert:│ "Set vertical border of splits

" lets you open multiple buffers without saving
set hidden

set conceallevel=0

set modelines=2
set sessionoptions=buffers,folds,tabpages,winsize
" dont load binary files:
set wildignore+=*.xlsx,*.ico,*.png,*.jpg,*.gif,*.jpeg,*.xcf,*.xls,*.orig,*.swp,*.bak,*.pyc,*.class,*.obj,*.o,*.aux,*.odg,*.pdf
let mapleader = ','
let maplocalleader = ','

set nostartofline

if has('mac')
  set macmeta
endif

set showcmd                                                   " Show current command
set wildmenu                                                  " show menu in commandline (tabs)
set tabpagemax=15                                             " only show 15 tabs
set ttyfast                                                   " Improves redrawing
set number                                                    " Views Line Numbers
set numberwidth=1
set relativenumber                                            " Views Line Numbers
set linespace=0                                               " No extra spaces between rows
set winminheight=0                                            " Windows can be 0 line high
set scrolljump=5                                              " Lines to scroll when cursor leaves screen
set scrolloff=0                                               " Minimum lines to keep above and below cursor
set t_Co=256                                                " forc 256 color scheme before trying anything
set cursorline                                              " highlight current line
set cursorcolumn                                            " highlight current column

set laststatus=2                                              " Always show the statusline

set backspace=2                                             " more powerful backspacing
set wrap                                                    " wrap long lines
set autoindent                                              " indent at the same level of the previous line
set shiftwidth=2                                            " use indents of 4 spaces
set expandtab                                               " tabs are spaces, not tabs
set tabstop=2                                               " an indentation every four columns
set softtabstop=2                                           " let backspace delete indent
set textwidth=80
set colorcolumn=+0
set shiftround                                              " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch                                               " set show matching parenthesis
set ignorecase                                              " ignore case (must be set for smartcase)
set smartcase                                               " ignore case if search pattern is all lowercase,
set hlsearch                                                " highlight search terms
set incsearch                                               " show search matches as you type
set ruler                                                   " show the cursor position
set cmdheight=1

"set undofile (undo after re- opening vim)
if exists("+undofile")
  " undofile - This allows you to use undos after exiting and restarting
  " :help undo-persistence
  " This is only present in 7.3+
  set undodir=~/.vim/tmp/
  set undofile
endif

"}}}

" ============================================================================
" NEOVIM {{{
" ============================================================================
if has('nvim')
  " set poython host (default would be python3)
  "let g:python_host_prog='/usr/bin/python2.7'
  let g:python_host_prog = '/usr/bin/python'
  highlight TermCursor ctermbg=0 guibg=#ff6767

  " Terminal settings
  tnoremap <C-\> <C-\><C-n>

  " Window navigation function
  " Make ctrl-h/j/k/l move between windows and auto-insert in terminals

  :au BufEnter * if &buftype == 'terminal' | :startinsert | endif
  tnoremap <esc><esc> <c-\><c-n>
  " " Workaround since <C-h> isn't working in neovim right now
  tnoremap <c-j> <C-\><C-n><C-w>j
  tnoremap <c-k> <C-\><C-n><C-w>k
  tnoremap <c-l> <C-\><C-n><C-w>l

  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

  " Hack to get C-h working in neovim
  imap <BS> <C-W>h

  set ttimeout
  set ttimeoutlen=0
endif
" }}}

" ============================================================================
" GUI {{{
" ============================================================================
if has('gui_running')
  set guioptions-=m
  set guioptions-=T
  set guioptions-=r
  set guioptions-=e

  if has("gui_gtk2")
    set guifont=Hack\ 11
  elseif has("gui_mac")
    set guifont=Hack:11
  elseif has("gui_win32") || has('win64')
    set guifont=Hack:11
  else
    set guifont=Droid\ Sans\ Mono\ for\ Powerline\ Plus\ Nerd\ File\ Types\ 11
  endif
endif

" }}}

" ============================================================================
" MAPPINGS {{{
" ============================================================================
" store relative line number jumps in jumplist
" NOTE: m' stores current position in jumplist
" NOTE: thanks to osse and bairui in #vim IRC!
nnoremap <silent> k :<C-U>execute 'normal!' (v:count>1 ? "m'".v:count.'k' : 'k')<Enter>
nnoremap <silent> j :<C-U>execute 'normal!' (v:count>1 ? "m'".v:count.'j' : 'j')<Enter>

" switch 0 and ^
noremap 0 ^
noremap ^ 0

" Buffer swtiching with [Bufferindex]!
nnoremap ! :<C-u>b<C-r>=v:count<CR><CR> " nnoremap #! :b #<CR>

" Map <space> to : for faster command mode
"noremap <space> :

" Better Mark jumps
noremap <leader>m :marks<CR>

"go to marks 'exact
noremap ` '
noremap ' `

"Map Q to repeat last recorded Macro
noremap Q @@

" Select last inserted text
nmap gp `[v`]

" select Visual again after shifting
vnoremap > >gv
vnoremap < <gv

" To save instead of :w
noremap <C-Z> :update<CR>
vnoremap <C-Z> <C-C>:update<CR>
inoremap <C-Z> <C-O>:update<CR>
" To quit instead of :q
noremap <C-Q> :q<CR>
vnoremap <C-Q> <C-C>:q<CR>
inoremap <C-Q> <C-O>:q<CR>

" making new line in insert mode
imap <A-Cr> <esc>o

" This is for opening new tabs or
" switching between tabs
imap <A-&> <esc>:tabnext 1<Cr>i
imap <A-é> <esc>:tabnext 2<Cr>i
imap <A-"> <esc>:tabnext 3<Cr>i
imap <A-'> <esc>:tabnext 4<Cr>i
imap <A-(> <esc>:tabnext 5<Cr>i
imap <A-§> <esc>:tabnext 6<Cr>i
imap <A-è> <esc>:tabnext 7<Cr>i
imap <A-!> <esc>:tabnext 8<Cr>i
imap <A-ç> <esc>:tabnext 9<Cr>i
imap <A-à> <esc>:tabnext 10<Cr>i
nnoremap <A-&> 1gt
nnoremap <A-é> 2gt
nnoremap <A-"> 3gt
nnoremap <A-'> 4gt
nnoremap <A-(> 5gt
nnoremap <A-§> 6gt
nnoremap <A-è> 7gt
nnoremap <A-!> 8gt
nnoremap <A-ç> 9gt
nnoremap <A-à> 10gt
"nnoremap <C-tab> :tabprevious<CR>
"nnoremap <F3> :tabnext<CR>
nnoremap <C-t> :tabnew<CR>
"inoremap <F2> <Esc>:tabprevious<CR>i
"inoremap <F3> <Esc>:tabnext<CR>i
inoremap <C-t> <Esc>:tabnew<CR>

" ctrl-I to switch between vertical or
" horizontal splitted windows
map <C-I> <C-W><C-W>

" vim explorer
map <F4> :!ls<CR>:e

" Maps Alt-[h,j,k,l] to resizing a window split
noremap <silent> <A-h> <C-w><<esc>
noremap <silent> <A-k> <C-W>-<esc>
noremap <silent> <A-j> <C-W>+<esc>
noremap <silent> <A-l> <C-w>><esc>

" emacs insert bindings
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A

" TO move is selection mode
inoremap <C-d> <Left>
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>

" Move up and down lines or blocks
nnoremap <A-S-j> :m .+1<CR>==
nnoremap <A-S-k> :m .-2<CR>==
inoremap <A-S-j> <Esc>:m .+1<CR>==gi
inoremap <A-S-k> <Esc>:m .-2<CR>==gi
vnoremap <A-S-j> :m '>+1<CR>gv=gv
vnoremap <A-S-k> :m '<-2<CR>gv=gv

" Map <Leader>f to display all lines with keyword under cursor
" and ask which one to jump to
nmap <Leader>f [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>

" set Y to duplicate lines, works in visual mode as well.
nnoremap <C-d> yyp
vnoremap <C-d> y`>pgv

"<leader>y or <leader>p for system clipboard
vnoremap <Leader>y "+y
vnoremap <Leader>d "+d
nnoremap <Leader>d "+d
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P

"Make Y behave Like D or C
noremap Y y$

" Gif config
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)

"J and K to Jump to beginning of Line
noremap J +
noremap K -
"to Join lines
"noremap <leader>j J
"to show help under cursor
"noremap <leader>k K

" make easier to make esc
inoremap éé <esc>

"write file with gs
noremap gs :w<CR>

" go to end of search highlight
noremap <silent> <leader>e /<c-r>//e<cr>:let @/='<c-r>/'<cr>

" Easy vimrc editing with :EditVim
command! VimEdit :edit ~/.vimrc
noremap g. :VimEdit<CR>

"nnoremap <silent> <leader>l :set spell!<CR>

"Opens the help page on the current window if cursor is in a help page or opens another tab if it's not
command! -nargs=1 -complete=help Help if &ft=~"help" | help <args> | else | tab help <args> | endif

"Display the numbered registers, press a key and paste it to the buffer
function! Reg()
  reg
  echo "Register: "
  let char = getchar()
  execute "normal! \"".nr2char(char)."p"
  redraw
  normal! k
endfunction
command! -nargs=0 Reg call Reg() | normal <cr>

" }}}

" ============================================================================
" PLUGINS {{{
" ============================================================================
if filereadable(expand("~/.vim/bundles.vim"))
    source ~/.vim/bundles.vim
endif

" Automatic detect file types
filetype plugin indent on
" }}}
