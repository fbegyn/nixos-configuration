" Plugins
" => vim-plug
"''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
call plug#begin('~/.vim/plugged')
" Automatically install missing plugins on startup
if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
  autocmd VimEnter * PlugInstall | q
endif

" IDE
Plug 'jwilm/i3-vim-focus'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'sirver/ultisnips'
Plug 'ervandew/supertab'
Plug 'terryma/vim-multiple-cursors'
Plug 'Yggdroot/indentLine'
Plug 'benmills/vimux'
Plug 'kien/ctrlp.vim'
Plug 'majutsushi/tagbar'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'autozimu/LanguageClient-neovim', {'branch': 'next', 'do': 'bash install.sh'}
" Plug 'liuchengxu/vim-clap', { 'do': function('clap#helper#build_all') }

" Autocompletion engine
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-jedi', {'for':['python','py']}
Plug 'ncm2/ncm2-tmux'
Plug 'ncm2/ncm2-ultisnips'
Plug 'ncm2/ncm2-pyclang'

" Misc
Plug 'roxma/nvim-yarp'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'

" Helpers
Plug 'christoomey/vim-tmux-navigator'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'ntpeters/vim-better-whitespace'
Plug 'w0rp/ale'

" Theme
Plug 'vim-airline/vim-airline'
Plug 'srcery-colors/srcery-vim'

" Languages
Plug 'fatih/vim-go', {'for': ['go'], 'do': ':GoInstallBinaries'}
" Plug 'nsf/gocode', { 'rtp': 'vim', 'do': '~/.vim/plugged/gocode/vim/symlink.sh' }
" Plug 'benmills/vimux-golang'
Plug 'buoto/gotests-vim',{'for':['go']}
Plug 'rust-lang/rust.vim',{'for':['rs','rust']}
Plug 'davidhalter/jedi-vim', { 'for':['python']}
Plug 'pangloss/vim-javascript',{ 'for' : ['js']}
Plug 'elzr/vim-json',{ 'for' : ['json']}
Plug 'othree/html5.vim', { 'for':['html','htm'] }
Plug 'suoto/vim-hdl', {'for':['vhdl','hdl']}
Plug 'ledger/vim-ledger',{'for':['journal']}
Plug 'lervag/vimtex',{'for':['latex','tex']}
Plug 'KeitaNakamura/tex-conceal.vim', {'for': ['tex','latex']}
Plug 'hashivim/vim-terraform',{'for':['tf', 'terraform']}
Plug 'pearofducks/ansible-vim', {'for':['ansible']}
Plug 'google/vim-jsonnet',{'for':['jsonnet']}
Plug 'cespare/vim-toml',{'for':['toml']}
call plug#end()

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let g:mapleader = ','

set history=500
set showmode
set lazyredraw
set hidden
set noswapfile
set autoread
set ignorecase
set smartcase
set magic
set showmatch
set nobackup            " Do not keep backup files
set nowritebackup       " Do not keep backup files
set noerrorbells        " No annoying error bells
set updatetime=750      " Based on https://github.com/airblade/vim-gitgutter#when-are-the-signs-updated
set smartindent         " Smart indent
set colorcolumn=100     " Show column at n characters
set textwidth=101       " Linebreak at n characters
set linebreak
set wrap

" show line numbers
set number
set numberwidth=2
highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

" Default split behavior
set splitright
set splitbelow

set mouse=a
set fileformat=unix
set whichwrap+=<,>,h,l

" Default ignore files
set wildignore+=.*,.git,*.swp,*pyc,*pyo,*.png,*.jpg,*.gif,*.ai,*.jpeg,*.psd,*.jar,*.zip,*.gem,log/**,tmp/**,coverage/**,rdoc/**,output_*,*.xpi,doc/**

" Autcompletion
" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()
" IMPORTANT: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect
set shortmess+=c

" Enable filetype plugins
filetype plugin on
filetype indent on

" Number sytem for c-a and c-x incrementation/decrementation
set nrformats-=octal

" Rounds to indent to multiples of shiftwidth
set shiftround
set tabstop=2
set shiftwidth=2
set expandtab

" timeout for combination of keys before considered sime key strokes
set ttimeout
set ttimeoutlen=60

" Fast saving
nmap <leader>w :w!<cr>
" Fast exiting
nnoremap <leader>qq :qa!<cr>
nnoremap <leader>q :q!<cr>

" exit insert mode
inoremap <C-c> <Esc>

" Set x lines to the cursor - when moving vertically using j/k
set scrolloff=4
" Height of the command bar
set cmdheight=2
" Makes search act like search in modern browsers
set incsearch

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" No annoying sound on errors
set novisualbell
set t_vb=
set timeoutlen=500

" Enable syntax highlighting
syntax on
" Enable better colors
set t_Co=256

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf-8

" => Vimdiff
nnoremap <leader>dr :diffget RE<cr>
nnoremap <leader>dl :diffget LO<cr>
nnoremap <leader>db :diffget BA<cr>

" => Visual mode related
" Visual mode pressing # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" => Moving around, tabs, windows and buffers
" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <c-space> ?

" Calling rust tools
nnoremap <silent> <M-o> :Clap<cr>
nnoremap <silent> <leader>clf :Clap files<cr>

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>
" Disabling the arrow keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
" Smart way to move between windows
map <C-j>     <C-W>j
map <C-k>     <C-W>k
map <C-h>     <C-W>h
map <C-l>     <C-W>l

" Close the current buffer
map <leader>bd :Bclose<cr>:tabclose<cr>gT
" Close all the buffers
map <leader>ba :bufdo bd<cr>
map <leader>j :bnext<cr>
map <leader>k :bprevious<cr>

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove<cr>
map <leader>tj :tabnext<cr>
map <leader>tk :tabprevious<cr>

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
augroup tabLeave
  au TabLeave * let g:lasttab = tabpagenr()
augroup end

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>
" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set showtabline=2
catch
endtry

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" => Spell checking
set spelllang=nl_be,en_gb
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u
" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>
" Shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=

" Terraform language settings
let g:terraform_align=1
let g:terraform_fmt_on_save=1

" Language server
let g:LanguageClient_autoStart = 1
let g:LanguageClient_hasSnippetSupport = 0
let g:LanguageClient_hoverPreview = 'Never'
let g:LanguageClient_serverCommands = {
    \ 'rust': ['/home/francis/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'python': ['/usr/bin/pyls'],
    \ 'sh': ['/usr/bin/bash-language-server', 'start'],
    \ 'php': ['/usr/bin/php-language-server'],
    \ 'html' : ['/usr/lib/node_modules/vscode-html-languageserver-bin/htmlServerMain.js','--stdio'],
    \ 'go' : ['/home/francis/Go/bin/gopls','-mode','-stdio'],
    \ 'css' : ['/usr/lib/node_modules/vscode-css-languageserver-bin/cssServerMain.js','--stdio'],
    \ 'cpp': ['/usr/bin/ccls', '--log-file=/tmp/ccls.log'],
    \ 'c': ['/usr/bin/ccls', '--log-file=/tmp/ccls.log'],
    \ 'json' : ['/usr/lib/node_modules/vscode-json-languageserver-bin/jsonServerMain.js','--stdio'],
    \ 'javascript': ['/usr/bin/javascript-typescript-stdio'],
\}
nnoremap <F7> :call LanguageClient_contextMenu()<CR>
noremap <leader>rn :call LanguageClient#textDocument_rename()<CR>
call ncm2#override_source('LanguageClient_python', {'enable': 1})

" Default mapping
let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_start_word_key      = '<C-n>'
let g:multi_cursor_select_all_word_key = '<A-n>'
let g:multi_cursor_start_key           = 'g<C-n>'
let g:multi_cursor_select_all_key      = 'g<A-n>'
let g:multi_cursor_next_key            = '<C-n>'
let g:multi_cursor_prev_key            = '<C-p>'
let g:multi_cursor_skip_key            = '<C-x>'
let g:multi_cursor_quit_key            = '<Esc>'

" => goyo
nnoremap <leader>f :Goyo<cr>
let g:goyo_width = 100
let g:goyo_height = 100
let g:goyo_linenr = 1

augroup Goyo
  autocmd! User GoyoEnter Limelight
  autocmd! User GoyoLeave Limelight!
augroup end

let g:AutoPairsMapCR=0
let g:SuperTabDefaultCompletionType = '<c-n>'

" ale
let g:ale_linters_explicit = 1
let g:ale_completion_enabled = 0

" => IndentLine
let g:indentLine_char = '|'
let g:indentLine_color_term = 239

" => Golang
let g:go_fmt_command = 'goimports'
let g:go_auto_type = 1
let g:go_auto_type_info = 1
let g:go_template_autocreate = 0
let g:go_gocode_unimported_packages = 1
let g:go_gopls_complete_unimported = 1
let g:go_bin_path= '/home/francis/Go/bin'
let g:go_def_mode='/home/francis/Go/bin/gopls'
let g:go_info_mode='gopls'

augroup golang
  autocmd FileType go nnoremap <leader>gt :GoTest<cr>
  autocmd FileType go nnoremap <leader>gs :GoFillStruct<cr>
  autocmd FileType go nnoremap <leader>gf :GoFmt
augroup end

" => Rust
augroup rustfmt
  autocmd FileType rust,rs cnoreabbrev ff :%! rustfmt<cr>
  autocmd FileType rust,rs cnoreabbrev fs :!rustfmt<cr>
augroup end

" => Markdown
augroup markdown
  autocmd FileType markdown,md set virtualedit=all
augroup end

" javascript
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_ngdoc = 1

" => Git
set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''}
set signcolumn=yes

" => airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

" hledger
" => Helper functions
" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction

" Basic shortcuts definitions
" comment / decomment & normal comment behavior
vmap <C-m> gc
" Disable tComment to escape some entities
let g:tcomment#replacements_xml={}

nnoremap <Leader>p :set paste<CR>
nnoremap <Leader>o :set nopaste<CR>

" Color scheme
" Background color
set background=dark
" Colorscheme
colorscheme srcery
let g:srcery_transparent_background = 1
highlight SpellBad cterm=underline ctermbg=130
highlight DiffAdd    cterm=bold ctermfg=2 ctermbg=none gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=1 ctermbg=none gui=none guifg=bg guibg=Red
highlight DiffChange cterm=none ctermfg=4 ctermbg=none gui=none guifg=bg guibg=Red
highlight DiffText   cterm=none ctermfg=11 ctermbg=none gui=none guifg=bg guibg=Red

" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif

" NERDtree
" autocmd vimenter * NERDTree
map <C-t> :NERDTreeToggle<CR>

" CtrlP
nnoremap <leader>tp :CtrlPTag<cr>
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
	\ 'dir':  '\v[\/]\.(git|hg|svn)$|log\|tmp$',
	\ 'file': '\v\.(exe|so|dll|aux|alg|fls|glg)$',
	\ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
	\ }

" Tagbar
nnoremap <silent> <Leader>tb :TagbarToggle<CR>
let g:tagbar_ctags_bin = "/home/francis/.local/bin/unctags"

" jedi-vim + ncm2-jedi
" Disable Jedi-vim autocompletion and enable call-signatures options
let g:jedi#auto_initialization = 1
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
let g:jedi#smart_auto_mappings = 0
let g:jedi#popup_on_dot = 0
let g:jedi#completions_command = ""
let g:jedi#show_call_signatures = "1"

set rtp+=./

let g:UltiSnipsExpandTrigger		= "<tab>"
let g:UltiSnipsJumpForwardTrigger	= "<tab>"
let g:UltiSnipsJumpBackwardTrigger	= "<s-tab>"
let g:UltiSnipsRemoveSelectModeMappings = 0

" Vimtex
let g:tex_flavor='latex'
let g:vimtex_compiler_progname = 'nvr'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=2
let g:vimtex_quickfix_open_on_warning=0
set conceallevel=2
let g:tex_conceal='abdmg'
"let g:tex_fast='r'
let g:vimtex_quickfix_autoclose_after_keystrokes=5
let g:vimtex_quickfix_open_on_warning=0
let g:vimtex_toc_enabled=1
let g:vimtex_toc_todo_keywords=['TODO', 'FIXME']
let g:vimtex_fold_enabled=1
autocmd Filetype tex setl updatetime=1
" Snippets
" Press enter key to trigger snippet expansion
" The parameters are the same as `:help feedkeys()`
inoremap <silent> <expr> <CR> ncm2_ultisnips#expand_or("\<CR>", 'n')

au Filetype tex call ncm2#register_source({
        \ 'name' : 'vimtex-cmds',
        \ 'priority': 8,
        \ 'complete_length': -1,
        \ 'scope': ['tex'],
        \ 'matcher': {'name': 'prefix', 'key': 'word'},
        \ 'word_pattern': '\w+',
        \ 'complete_pattern': g:vimtex#re#ncm2#cmds,
        \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
        \ })
    au Filetype tex call ncm2#register_source({
        \ 'name' : 'vimtex-labels',
        \ 'priority': 8,
        \ 'complete_length': -1,
        \ 'scope': ['tex'],
        \ 'matcher': {'name': 'combine',
        \             'matchers': [
        \               {'name': 'substr', 'key': 'word'},
        \               {'name': 'substr', 'key': 'menu'},
        \             ]},
        \ 'word_pattern': '\w+',
        \ 'complete_pattern': g:vimtex#re#ncm2#labels,
        \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
        \ })
    au Filetype tex call ncm2#register_source({
        \ 'name' : 'vimtex-files',
        \ 'priority': 8,
        \ 'complete_length': -1,
        \ 'scope': ['tex'],
        \ 'matcher': {'name': 'combine',
        \             'matchers': [
        \               {'name': 'abbrfuzzy', 'key': 'word'},
        \               {'name': 'abbrfuzzy', 'key': 'abbr'},
        \             ]},
        \ 'word_pattern': '\w+',
        \ 'complete_pattern': g:vimtex#re#ncm2#files,
        \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
        \ })
    au Filetype tex call ncm2#register_source({
        \ 'name' : 'bibtex',
        \ 'priority': 8,
        \ 'complete_length': -1,
        \ 'scope': ['tex'],
        \ 'matcher': {'name': 'combine',
        \             'matchers': [
        \               {'name': 'prefix', 'key': 'word'},
        \               {'name': 'abbrfuzzy', 'key': 'abbr'},
        \               {'name': 'abbrfuzzy', 'key': 'menu'},
        \             ]},
        \ 'word_pattern': '\w+',
        \ 'complete_pattern': g:vimtex#re#ncm2#bibtex,
        \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
        \ })
