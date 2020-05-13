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

" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set showtabline=2
catch
endtry
