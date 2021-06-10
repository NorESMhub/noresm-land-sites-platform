set nocompatible              " required
filetype off                  " required

" ruler: (line,column) bottom right
set ruler

" visual bell
set visualbell
set t_vb=

" always highlight search results
set hlsearch
hi Search cterm=NONE ctermfg=Yellow ctermbg=DarkGrey

" enable file-specific settings
filetype plugin on

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" alternatively, pass a path where Vundle should install plugins
" "call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Add all your plugins here (note older versions of Vundle used Bundle instead of Plugin)
Plugin 'vim-scripts/indentpython.vim'
Bundle 'Valloric/YouCompleteMe'
Plugin 'ervandew/supertab'
Plugin 'jpalardy/vim-slime'
Plugin 'JuliaEditorSupport/julia-vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" youcompleteme settings
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>

"" omnicompletion (instead of Valloric/YouCompleteMe on vim 7)
"filetype plugin on
"set omnifunc=syntaxcomplete#Complete

" Other Python settings
syntax on
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent
set fileformat=unix

" Show current line number and relative numbers around it
set number
set relativenumber
highlight CursorLineNR ctermbg=DarkGrey cterm=bold ctermfg=Yellow

" colors
set term=screen-256color
"if exists('$TMUX')
"  set term=screen-256color
"endif
highlight Visual cterm=bold ctermbg=Black ctermfg=NONE

" Slime plugin (send text to REPL via tmux)
if exists('$TMUX')
    let g:slime_target = "tmux"
    let g:slime_paste_file = tempname()
    let g:slime_default_config = {"socket_name": split($TMUX, ",")[0], "target_pane": "1"}
    let g:slime_python_ipython = 1
    xmap ,q <Plug>SlimeRegionSend
    xmap ,a <Plug>SlimeLineSend
endif
nmap ,q <Plug>SlimeParagraphSend
nmap ,a <Plug>SlimeLineSend

" for tmux to automatically set paste and nopaste mode at the time pasting (as happens in VIM UI)
function! WrapForTmux(s)
  if !exists('$TMUX')
    return a:s
  endif       
  let tmux_start = "\<Esc>Ptmux;"
  let tmux_end = "\<Esc>\\"        
  return tmux_start . substitute(a:s, "\<Esc>","\<Esc>\<Esc>", 'g') . tmux_end
endfunction
               
let &t_SI .= WrapForTmux("\<Esc>[?2004h")
let &t_EI .= WrapForTmux("\<Esc>[?2004l")
                  
function! XTermPasteBegin()
  set pastetoggle=<Esc>[201~
  set paste
  return ""
endfunction
 
inoremap <special> <expr> <Esc>[200~ XTermPasteBegin()

" wrap text in diff mode
au VimEnter * if &diff | execute 'windo set wrap' | endif

" backspace
set backspace=indent,eol,start
