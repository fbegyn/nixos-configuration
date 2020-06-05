augroup golang
  autocmd FileType go nnoremap <leader>gt :GoTest<cr>
  autocmd FileType go nnoremap <leader>gs :GoFillStruct<cr>
  autocmd FileType go nnoremap <leader>gf :GoFmt
augroup end
