let g:LanguageClient_autoStart = 1
let g:LanguageClient_hasSnippetSupport = 0
let g:LanguageClient_hoverPreview = 'Never'
let g:LanguageClient_serverCommands = {
    \ 'go' : ['/home/francis/go/bin/gopls','-mode','-stdio'],
\}
nnoremap <F7> :call LanguageClient_contextMenu()<CR>
noremap <leader>rn :call LanguageClient#textDocument_rename()<CR>
