" 画面表示の設定
set number         " 行番号を表示する
set list           " 不可視文字を表示
set listchars=eol:~,tab:>\ ,trail:~,extends:<      " listで表示される文字のフォーマットを指定する
scriptencoding utf-8 "これ入れないと下記が反映されない
augroup highlightZenkakuSpace "全角スペースを赤色にする
  autocmd!
  autocmd VimEnter,ColorScheme * highlight ZenkakuSpace term=underline ctermbg=Red guibg=Red
  autocmd VimEnter,WinEnter * match ZenkakuSpace /　/
augroup END

set encoding=utf-8
set termencoding=cp932
set fileencoding=utf-8
set fileencodings=utf-8,cp932

" カーソル移動関連の設定
set backspace=indent,eol,start " Backspaceキーの影響範囲に制限を設けない
set whichwrap=b,s,h,l,<,>,[,]  " 行頭行末の左右移動で行をまたぐ
set scrolloff=8                " 上下8行の視界を確保
set sidescrolloff=16           " 左右スクロール時の視界を確保
set sidescroll=1               " 左右スクロールは一文字づつ行う


" 入力モードでのカーソル移動
" inoremap <C-j> <Down>
" inoremap <C-k> <Up>
" inoremap <C-h> <Left>
" inoremap <C-l> <Right>


" カッコの自動補完
inoremap {<Enter> {}<Left><CR><ESC><S-o><Tab>
inoremap [<Enter> []<Left><CR><ESC><S-o><Tab>
inoremap (<Enter> ()<Left><CR><ESC><S-o><Tab>
inoremap { {}<ESC>i
inoremap ( ()<ESC>i
inoremap [ []<ESC>i


" カーソルを自動的に()の中へ
imap {} {}<Left>
imap [] []<Left>
imap () ()<Left>
imap "" ""<Left>
imap '' ''<Left>
imap <> <><Left>
" imap // //<left>
imap /// ///<left>


" ファイル処理関連の設定
set nobackup   " ファイル保存時にバックアップファイルを作らない
set noswapfile " ファイル編集中にスワップファイルを作らない
set noundofile " undoファイルを作成しない


" 検索/置換の設定
set hlsearch   " 検索文字列をハイライトする
set incsearch  " インクリメンタルサーチを行う
set ignorecase " 大文字と小文字を区別しない
set smartcase  " 大文字と小文字が混在した言葉で検索を行った場合に限り、大文字と小文字を区別する
set wrapscan   " 最後尾まで検索を終えたら次の検索で先頭に移る
set gdefault   " 置換の時 g オプションをデフォルトで有効にする


" タブ/インデントの設定
set autoindent    " 改行時に前の行のインデントを継続する
" set smartindent   " 改行時に入力された行の末尾に合わせて次の行のインデントを増減する


" 動作環境との統合関連の設定
set mouse=a     " マウスの入力を受け付ける
set shellslash  " Windows でもパスの区切り文字を / にする
set clipboard+=unnamed  "クリップボードを使用可能にする


" コマンドラインの設定
" コマンドラインモードでTABキーによるファイル名補完を有効にする
set wildmenu wildmode=list:longest,full
" コマンドラインの履歴を10000件保存する
set history=10000


" ビープの設定

"ビープ音すべてを無効にする
set visualbell t_vb=
set noerrorbells "エラーメッセージの表示時にビープを鳴らさない


:source $VIMRUNTIME/mswin.vim
 " Note: Skip initialization for vim-tiny or vim-small.
 if 0 | endif

 if has('vim_starting')
   if &compatible
     set nocompatible
   endif

   " Required:
   set runtimepath+=~/.vim/bundle/neobundle.vim/
 endif

 " Required:
 call neobundle#begin(expand('~/.vim/bundle/'))

 " NeoBundle 設定
 " Required:
 NeoBundleFetch 'Shougo/neobundle.vim'

 " My Bundles here:
 " Refer to |:NeoBundle-examples|.
 " Note: neobundle の設定を.gvimrcに書かないこと！

 " ここに追加したいプラグインを追加します。
" NeoBundle 'https://github.com/Shougo/neocomplcache.git'
" NeoBundle 'https://github.com/Shougo/unite.vim.git'
" NeoBundle 'https://github.com/Shougo/vimshell.git'
  NeoBundle 'jelera/vim-javascript-syntax'
  NeoBundle 'tpope/vim-fugitive'

 call neobundle#end()

 " Required:
 filetype plugin indent on

 " If there are uninstalled bundles found on startup,
 " this will conveniently prompt you to install them.
 NeoBundleCheck
