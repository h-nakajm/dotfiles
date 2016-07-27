## Emacs風キーバインド
bindkey -e

# 補完の強化
autoload -U compinit
compinit

# 補完候補を詰めて表示
setopt list_packed

# cdの履歴を保存
setopt auto_pushd

# 言語の設定
export LANG=ja_JP.UTF-8

# 色の設定
autoload -Uz colors
colors

## プロンプトの設定
PROMPT="%F{cyan}[%n@%m]%{$reset_color%

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

