" Easy bundles edeting with :EditBundles
command! BundlesEdit :edit ~/.vim/bundles.vim
noremap gb :BundlesEdit<CR>

" Load vim-plug
if empty(glob("~/.vim/autoload/plug.vim"))
  execute '!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin('~/.vim/plugged')

" deoplete (async completion)
function! DoRemote(arg)
  UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }

" Colorscheme
Plug 'MaxSt/FlatColor'
let g:flatcolor_termcolors=16
" set background=dark
Plug 'tyrannicaltoucan/vim-deep-space'
" set background=dark
Plug 'junegunn/seoul256.vim'
" seoul256 (light):
" Range: 252 (darkest) ~ 256 (lightest)
" Default: 253
let g:seoul256_background = 255
Plug 'NLKNguyen/papercolor-theme'
set t_Co=256   " This is may or may not needed.
set background=dark

" Active and Inactive window
" Plug 'blueyed/vim-diminactive'
" let g:diminactive_enable_focus = 0
" let g:diminactive_use_colorcolumn = 0
" let g:diminactive_use_syntax = 1

" " ctrlp (open file with fuzzy search)
" Plug 'ctrlpvim/ctrlp.vim'
" let g:ctrlp_map = '<leader>t'
" let g:ctrlp_open_new_file = 'r' " ctrlp opens new file in current window
" let g:ctrlp_cache_dir='~/.vim/tmp/'
" " let g:ctrlp_reuse_window='startify'
" let g:ctrlp_open_multiple_files = 'rr'
" " let g:ctrlp_clear_cache_on_exit=1
" let g:ctrlp_max_files = 10000
" let g:ctrlp_follow_symlinks=1
" nmap <leader>b :CtrlPBuffer<CR>
" if executable('ag')
"   let g:ctrlp_user_command = {
"     \ 'types': {
"     \ 1: ['.git', 'cd %s && git ls-files . -co --exclude-standard'],
"     \ 2: ['.hg', 'hg --cwd %s locate -I .']
"     \ },
"     \ 'fallback': 'ag %s -l --nocolor -g ""'
"   \ }
" else
"   let g:ctrlp_user_command = {
"     \ 'types': {
"       \ 1: ['.git', 'cd %s && git ls-files . -co --exclude-standard'],
"       \ 2: ['.hg', 'hg --cwd %s locate -I .']
"     \ },
"     \ 'fallback': 'find %s/.. -type f'
"   \ }
" endif


Plug 'jimmyhchan/dustjs.vim'

" fuzzy finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
set rtp+=~/.fzf
let g:fzf_history_dir = '~/.local/share/fzf-history'
if has('nvim')
  aug fzf_setup
    au!
    au TermOpen term://*FZF tnoremap <silent> <buffer><nowait> <esc> <c-c>
  aug END
end
nmap <silent> <leader>t :FZF<CR>

" mutiple cursors
Plug 'terryma/vim-multiple-cursors'
" Called once right before you start selecting multiple cursors
function! Multiple_cursors_before()
  " call youcompleteme#DisableCursorMovedAutocommands()
  " set foldmethod=manual
  " let s:old_ycm_whitelist = g:ycm_filetype_whitelist
  " let g:ycm_filetype_whitelist = {} 
  if exists(':NeoCompleteLock')==2
    exe 'NeoCompleteLock'
  endif
endfunction

" Called once only when the multiple selection is canceled (default <Esc>)
function! Multiple_cursors_after()
  " call youcompleteme#EnableCursorMovedAutocommands()
  " set foldmethod=syntax
  " let g:ycm_filetype_whitelist = s:old_ycm_whitelist
  if exists(':NeoCompleteUnlock')==2
    exe 'NeoCompleteUnlock'
  endif
endfunction


"YouCompleteMe (Code Completion)
"Plug 'Valloric/YouCompleteMe'
Plug 'Valloric/YouCompleteMe', { 'do': '~/.vim/plugged/YouCompleteMe/install.py --all'}
let g:ycm_collect_identifiers_from_tags_files = 1 " Let YCM read tags from Ctags file
let g:ycm_use_ultisnips_completer = 1 " Default 1, just ensure
let g:ycm_seed_identifiers_with_syntax = 1 " Completion for programming language's keyword
let g:ycm_complete_in_comments = 1 " Completion in comments
let g:ycm_complete_in_strings = 1 " Completion in string

let g:ycm_add_preview_to_completeopt=0
let g:ycm_confirm_extra_conf=0
set completeopt-=preview
"let g:ycm_key_list_select_completion = ['<C-v>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-b>', '<Up>']
let g:ycm_filetype_blacklist = {
      \ 'xtm' : 1
      \}

" Goto definition with F3
map <F3> :YcmCompleter GoTo<CR>


" syntastic
Plug 'scrooloose/syntastic'
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_python_use_codec=1
let g:syntastic_jade_checkers = ['jade_lint', 'tsuquyomi']
let g:syntastic_typescript_checkers = ['tsuquyomi']
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

" typescript auto complation
Plug 'leafgarland/typescript-vim'
let g:typescript_indent_disable = 1
let g:typescript_compiler_binary = 'tsc'
let g:typescript_compiler_options = ''
autocmd FileType typescript :set makeprg=tsc
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

Plug 'Quramy/tsuquyomi'
let g:tsuquyomi_disable_quickfix = 1
" autocmd FileType typescript nmap <buffer> <Leader>e <Plug>(TsuquyomiRenameSymbol)
" autocmd FileType typescript nmap <buffer> <Leader>E <Plug>(TsuquyomiRenameSymbolC)
autocmd FileType typescript setlocal completeopt+=menu,preview

" Angular 2
Plug 'bdauria/angular-cli.vim'
Plug 'burnettk/vim-angular'
Plug 'othree/javascript-libraries-syntax.vim'
let g:used_javascript_libs = 'jquery,underscore,backbone,angular,angularui,angularuirouter,jasmine,Vue.js,d3.js'
let g:syntastic_html_tidy_ignore_errors = [' attribute name ',' attribute "*ngswitchdefault" ']

if !exists("g:ycm_semantic_triggers")
  let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers['typescript'] = ['.']

" Ultisnips (Code Snippets)
Plug 'SirVer/ultisnips'
noremap gu :UltiSnipsEdit!<CR>
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsListSnippets='<c-s>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'

Plug 'OmniSharp/omnisharp-vim'

" lightline  (StatusBar)
Plug 'itchyny/lightline.vim'
let g:lightline = {
      \'colorscheme': 'PaperColor',
      \'active': {
      \'left': [ [ 'mode' ],
      \          [ 'readonly', 'filename', 'modified' ] ]
      \},
      \'component': {
      \'readonly': '%{&filetype=="help"?"":&readonly?"L":""}',
      \'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \},
      \'component_function': {
      \   'filetype': 'MyFiletype',
      \   'fileformat': 'MyFileformat'
      \},
      \'separator': { 'left': '▓', 'right': '▓' },
      \'subseparator': { 'left': '❱', 'right': '❱' }
      \ }

" function! LightLineParinferMode()
"   if &filetype == "clojure"
"     if g:parinfer_mode == "off"
"       return "o"
"     elseif g:parinfer_mode == "indent"
"       return "⇆"
"     else
"       return "❪❫"
"     endif
"   else
"     return ""
"   endif
" endfunction

function! MyFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! MyFileformat()
  return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

Plug 'cohama/lexima.vim' "auto-close chars)

Plug 'mbbill/undotree', {'on': 'UndotreeToggle'} " (Undo Tree)
noremap <leader>u :UndotreeToggle<Cr>

"Neomake (async make/lint)
Plug 'benekastah/neomake'
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_python_enabled_makers = ['pylint']
let g:neomake_python_pylint_exe = 'pylint2'

let g:neomake_warning_sign = {
      \ 'text': '?',
      \ 'texthl': 'WarningMsg'
      \ }
let g:neomake_error_sign = {
      \ 'text': '!',
      \ 'texthl': 'ErrorMsg'
      \ }
"let g:neomake_open_list = 1
if has('nvim')
  autocmd! BufWritePost *.js Neomake
  autocmd! BufWritePost *.jsx Neomake
endif

"vimproc
Plug 'Shougo/vimproc.vim', { 'do': 'make' }

" Vim-table-mode
Plug 'dhruvasagar/vim-table-mode'

" Vim-jade
Plug 'digitaltoad/vim-jade'

" Vim-stylus
Plug 'wavded/vim-stylus'


" Tabular (text filtering and alignment (:Tab /[ =,... ]))
Plug 'godlygeek/tabular', {'on': 'Tab'}

" Surround (surrounding text ( cs[motion], insert: ysi[motion], entire line: yss))
Plug 'tpope/vim-surround'

" Unimpaired (pairs of handy bracket mappings)
Plug 'tpope/vim-unimpaired'

" vim-commentary (Comment and Uncomment with gcc)
Plug 'tpope/vim-commentary'

" vim-repeat
Plug 'tpope/vim-repeat'

" search for, substitute, and abbreviate multiple variants of a word 
" and camel/snake case conversion
Plug 'tpope/vim-abolish'

" Ack (Ack for vim)
Plug 'dyng/ctrlsf.vim'
" Plug 'mileszs/ack.vim', {'on': 'Ack'}
" let g:ackprg = 'ag --nogroup --nocolor --column'


" Trailertrash (identify and Irradicate unwanted whitespace at the end of the line (:Trim))
Plug 'csexton/trailertrash.vim', {'on': 'Trim'}

" Matchit (% matches for html, latex, ruby,...)
Plug 'vim-scripts/matchit.zip'

" SplitJoin (splits and joins multiple code lines)
Plug 'AndrewRadev/splitjoin.vim'

" Fugitive (Git Support for vim)
Plug 'tpope/vim-fugitive'


" Covim (Collaborative Editing)
"Plug 'FredKSchott/CoVim.git'


" eunuch (unix commands inside vim)
Plug 'tpope/vim-eunuch'


" automatically sets conceallevel --> deactive
" " IndentLine (Show vertical line at each indent Level)
" let g:indentLine_char = '┆'
" Plug 'Yggdroot/indentLine'

" vim-css-color
Plug 'ap/vim-css-color'

" " chrisbra/Colorizer
" Plug 'chrisbra/Colorizer'
" let g:colorizer_auto_color = 1

"Emmet (Zen Coding)
Plug 'mattn/emmet-vim'

"function! s:zen_html_tab()
"  let line = getline('.')
"  if match(line, '<.*>') >= 0
"    return "\<c-y>n"
"  endif
"  return "\<c-y>,"
"endfunction

" textobject user (define your own text objects)
Plug 'kana/vim-textobj-user'

" textobject indent (textobject [i] for same indent level)
Plug 'kana/vim-textobj-indent'

" textobject indent (textobject [/] for last search pattern)
Plug 'kana/vim-textobj-lastpat'

" vim-over (substitute preview)
" Plug 'osyo-manga/vim-over'

" vim-tmux-navigator (c-hjkl navigation in vim and tmux, c-\ = previous)
Plug 'christoomey/vim-tmux-navigator'

" targets (more text objects z.b in])
Plug 'wellle/targets.vim'

" pandoc-syntax (Pandoc Syntax Highlighting)
Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': 'pandoc' }

" vim-exchange (exchange with cx)
Plug 'tommcdo/vim-exchange'


" incsearch (Improved incremental searching for vim)
Plug 'haya14busa/incsearch.vim'
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" :h g:incsearch#auto_nohlsearch
" Sets hlsearch off automatically
set hlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

"Verymagic option as default
let g:incsearch#magic = '\v'

let g:incsearch#consistent_n_direction = 1
let g:incsearch#separate_highlight = 1

Plug 'haya14busa/vim-asterisk'
"stay as default
map *  <Plug>(asterisk-z*)
map #  <Plug>(asterisk-z#)
map g* <Plug>(asterisk-gz*)
map g# <Plug>(asterisk-gz#)
"keep cursor position when iterating over matches
let g:asterisk#keeppos = 1

Plug 'easymotion/vim-easymotion'
" vim-sneak (s is 2 char f)
"Plug 'justinmk/vim-sneak'
"let g:sneak#use_ic_scs = 1

" vim-shot-f (highlight next f,t,F,T chars)
"Plug 'deris/vim-shot-f'
Plug 'unblevable/quick-scope'
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']

" vimux (send commands to tmux)
"Plug 'benmills/vimux'

" better js syntax{
Plug 'othree/yajs.vim', { 'for' : ['javascript']}

" Plug 'pangloss/vim-javascript'
" let g:javascript_plugin_jsdoc = 1
" let g:javascript_plugin_ngdoc = 1
" let g:javascript_plugin_flow = 1

Plug 'sheerun/vim-polyglot'

" Plug 'Shougo/neco-syntax'
" Plug 'Shougo/neoinclude.vim'
"  let g:deoplete#enable_at_startup = 1
" set completeopt+=menuone

" supertab (tab complete/tab insert)
"Plug 'ervandew/supertab'
"let g:SuperTabDefaultCompletionType = "<c-n>"


" Close html tags
Plug 'alvan/vim-closetag'
let g:closetag_filenames = "*.xml,*.html,*.xhtml,*.html.erb,*.jsx"

" show git adds/changes/deletes
Plug 'airblade/vim-gitgutter'
"let g:gitgutter_sign_column_always = 1

"turn off signs
let g:gitgutter_map_keys = 0
let g:gitgutter_highlight_lines = 0
let g:gitgutter_signs = 0
"jump to next/previous hunk(change)
nmap ]c <Plug>GitGutterNextHunk
nmap [c <Plug>GitGutterPrevHunk
"stage and revert hunk
nmap <Leader>ga <Plug>GitGutterStageHunk
nmap <Leader>gu <Plug>GitGutterRevertHunk

" indent navigation (=],...)
Plug 'jeetsukumaran/vim-indentwise'
"
"File Explorer
Plug 'scrooloose/nerdtree'
let NERDTreeShowLineNumbers=1
let NERDTreeQuitOnOpen=1
noremap \ :NERDTreeToggle<CR>

Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim'
map <leader>c :RangerWorkingDirectory<CR>

" dev icons for neerdtree and powerline, etc
Plug 'ryanoasis/vim-devicons'

" "Vim for writing
" Plug 'reedes/vim-pencil', {'for': ['markdown','mkd','text']}
" augroup pencil
"   autocmd!
"   autocmd FileType markdown,mkd call pencil#init()
"   autocmd FileType text         call pencil#init()
" augroup END

Plug 'ReplaceWithRegister'

Plug 'junegunn/vim-peekaboo'

" Plug 'guns/vim-sexp', {'for': 'clojure'}
" " no insert mode mappings
" let g:sexp_enable_insert_mode_mappings = 0
" Plug 'tpope/vim-sexp-mappings-for-regular-people', {'for': 'clojure'}

" Plug 'kovisoft/slimv'

" For use with alternate Clojure REPL plugins
Plug 'guns/vim-clojure-static'
let g:clojure_align_multiline_strings = 0
let g:clojure_align_subforms = 1
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
" Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-classpath'


Plug 'neovim/node-host'
" Plug 'clojure-vim/nvim-parinfer.js'
" let g:parinfer_airline_integration = 1
" au BufNewFile,BufRead *.xtm let g:parinfer_mode = "off"

function! ToggleParinferMode()
  if g:parinfer_mode == "indent"
    let g:parinfer_mode = "paren"
  else
    let g:parinfer_mode = "indent"
  endif
endfunction
noremap <silent> gm :call ToggleParinferMode()<CR>


Plug 'lervag/vimtex', {'for': 'tex'}
let g:vimtex_fold_enabled=0
let g:vimtex_mappings_enabled=0
autocmd FileType tex,bib noremap <buffer> <space>v :VimtexView<cr>
autocmd FileType tex,bib noremap <buffer> <space>t :VimtexTocOpen<cr>

call plug#end()
let g:deoplete#enable_at_startup = 1
filetype plugin indent on

" colorscheme flatcolor
" colorscheme seoul256
colorscheme PaperColor
" try 229 for light background
" hi CursorLine   cterm=NONE ctermbg=232 ctermfg=NONE
" hi CursorColumn   cterm=NONE ctermbg=232 ctermfg=NONE
hi Normal guibg=NONE ctermbg=NONE
autocmd BufReadPost * call FocusLost_SaveFiles()

hi link GitGutterAdd DiffAdd
hi link GitGutterDelete DiffDelete
hi link GitGutterChange DiffChange
