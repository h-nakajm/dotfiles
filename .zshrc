# エディタの設定
export EDITOR=vim

## Emacs風キーバインド
bindkey -e
bindkey -r '^t'

# 補完の強化
autoload -U compinit
compinit

# 補完候補を詰めて表示
setopt list_packed

# cdの履歴を保存
setopt auto_pushd

# ディレクトリ名だけでcd
setopt auto_cd

# 同じディレクトリをpushdしない
setopt pushd_ignore_dups

# 言語の設定
export LANG=ja_JP.UTF-8

# 色の設定
autoload -Uz colors
colors
export TERM=xterm-256color

# lsに色を付ける
export LS_COLORS='di=36:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# ls --colorをglsにalias
alias gls="ls --color"

# for cygwin to open
alias open="cygstart"

## プロンプトの設定
#PROMPT="%F{cyan}[%n@%m]%{$reset_color%

#$ "

PROMPT="%F{012}[%n@%m]%f %F{057}%K{000}%d%f%k
$ "

# 履歴の保存
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_dups		# 同じコマンドを記憶しない
setopt share_history		# 履歴データを共有する
setopt hist_verify			# ヒストリの呼び出しから実行前に編集可能

# コマンド履歴の検索
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

## ビープを鳴らさない
setopt nobeep

# コマンドの自動修正
setopt correct

# 最後のバックスラッシュを自動で削除しない
setopt noautoremoveslash

## cdr
autoload -Uz add-zsh-hock
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook

# zsh-syntax-highlighting
# if [ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
# 	source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# fi

show_buffer_stack() {
  POSTDISPLAY="
stack: $LBUFFER"
  zle push-line
}
zle -N show_buffer_stack
bindkey "^[q" show_buffer_stack

# c-Jでコマンド入力中に改行
bindkey '^J' self-insert

# 拡張ファイルグロブを有効化
setopt extended_glob

# Ctrl+Dでログアウトしてしまうことを防ぐ
setopt IGNOREEOF

# git 関連のalias
alias gs="git status"
alias ga="git add"
alias gc="git commit"
alias gp="git push"

# zplug
source ~/.zplug/init.zsh

# zplug "zplug/zplug", hook-build:"zplug --self-manage"
zplug "mollifier/anyframe"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-history-substring-search"
# zplug "kagamilove0707/moonline.zsh"
# zplug "percol/percol"

if ! zplug check --verbose; then
    printf "インストールしますか？[y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

# anyframe keybinds
# percol-keybinds: http://blog.shibayu36.org/entry/2013/10/06/184146

bindkey '^x^d' anyframe-widget-cdr
# bindkey '^x^r' anyframe-widget-execute-history
bindkey '^x^r' anyframe-widget-put-history