# エディタの設定
export EDITOR=vim

## Emacs風キーバインド
bindkey -e

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

# zsh-syntax-highlighting
if [ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
	source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
