# 言語の設定
export LANG=ja_JP.UTF-8

# PROMPT="%F{cyan}[%n@%m %d]%f "
# 色の設定
autoload -Uz colors
colors

#プロンプトの設定
PROMPT="%F{cyan}[%n@%m]%{$reset_color%

$ "
HISTSIZE=100000
SAVEHIST=100000

## ビープを鳴らさない
setopt nobeep
