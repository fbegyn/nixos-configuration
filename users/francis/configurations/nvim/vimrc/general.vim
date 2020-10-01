" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let g:mapleader = ','

set history=500
set hidden
set noswapfile
set magic
set showmatch
set nobackup            " Do not keep backup files
set nowritebackup       " Do not keep backup files
set updatetime=750      " Based on https://github.com/airblade/vim-gitgutter#when-are-the-signs-updated
set colorcolumn=80     " Show column at n characters
set textwidth=81      " Linebreak at n characters
set linebreak
set wrap

" show line numbers
set number
set numberwidth=2
highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

set mouse=a

" Default ignore files
set wildignore+=.*,.git,*.swp,*pyc,*pyo,*.png,*.jpg,*.gif,*.ai,*.jpeg,*.psd,*.jar,*.zip,*.gem,log/**,tmp/**,coverage/**,rdoc/**,output_*,*.xpi,doc/**

autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType mako setlocal ts=2 sts=2 sw=2 expandtab

" Enable filetype plugins
filetype plugin indent on

" Rounds to indent to multiples of shiftwidth
set noexpandtab
set smartindent         " Smart indent
set tabstop=4
set shiftwidth=4

" timeout for combination of keys before considered sime key strokes
set ttimeout
set ttimeoutlen=60

nmap <leader>w :w!<cr>
nnoremap <leader>q :q!<cr>
" exit insert mode
inoremap <C-c> <Esc>

" Set x lines to the cursor - when moving vertically using j/k
set scrolloff=4
set cmdheight=2
" Makes search act like search in modern browsers
set incsearch

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" No annoying sound on errors
set noerrorbells        " No annoying error bells
set novisualbell
set t_vb=
set timeoutlen=500

" Enable syntax highlighting
syntax on

" Enable better colors
set t_Co=256

" Set utf8 as standard encoding
set encoding=utf-8

" => Vimdiff
nnoremap <leader>dr :diffget RE<cr>
nnoremap <leader>dl :diffget LO<cr>
nnoremap <leader>db :diffget BA<cr>

" => Visual mode related
" Visual mode pressing # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif
