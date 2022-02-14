augroup golang
  autocmd FileType go nnoremap <leader>gs :GoFillStruct<cr>
  autocmd FileType go nnoremap <leader>gf :%! gofmt<cr>
augroup end
