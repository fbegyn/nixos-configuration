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

" Enable filetype plugins
filetype plugin on
filetype indent on

" Number sytem for c-a and c-x incrementation/decrementation
set nrformats-=octal

" Rounds to indent to multiples of shiftwidth
set shiftround
set tabstop=2
set shiftwidth=2

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

" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif
