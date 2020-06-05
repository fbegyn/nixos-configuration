nnoremap <leader>tp :CtrlPTag<cr>
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn)$|log\|tmp$',
    \ 'file': '\v\.(exe|so|dll|aux|alg|fls|glg)$',
    \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
    \ }
