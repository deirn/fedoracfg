FEDORACFG="$HOME/Documents/github.com/deirn/fedoracfg"

# [ powerlevel10k direnv compat ]
# https://github.com/romkatv/powerlevel10k?tab=readme-ov-file#how-do-i-initialize-direnv-when-using-instant-prompt
(( ${+commands[direnv]} )) && emulate zsh -c "$(direnv export zsh)"
# [ powerlevel10k direnv compat ]


# [ powerlevel10k instant prompt ]
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
# [ powerlevel10k instant prompt ]


# [ powerlevel10k direnv compat ]
(( ${+commands[direnv]} )) && emulate zsh -c "$(direnv hook zsh)"
# [ powerlevel10k direnv compat ]


# [ global definitions ]
if [ -f /etc/zshrc ]; then
    source /etc/zshrc
fi
# [ global definitions ]


# [ zsh-newuser-install ]
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# [ zsh-newuser-install ]


# [ completions ]
export fpath=("$HOME/.local/bin/_completions" $fpath)

zstyle :compinstall filename '/home/deirn/.zshrc'

autoload -Uz compinit
compinit
# [ completions ]


# [ powerlevel10k ]
source "$FEDORACFG/share/powerlevel10k/powerlevel9k.zsh-theme"
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
source "$FEDORACFG/.pk10.zsh"
# [ powerlevel10k ]


# [ zsh-vi-mode ]
source "$FEDORACFG/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh"

function zvm_get_cutbuffer() {
    wl-paste -n
}

function zvm_set_cutbuffer() {
    echo -n $1 | sed 's/\x1B\[[0-9;]*m//g' | wl-copy -n
}
# [ zsh-vi-mode ]


# [ fzf ]
source <(fzf --zsh)
# [ fzf ]


# [ vterm ]
vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
# [ vterm ]


# [ asdf ]
source "$HOME/.asdf/plugins/java/set-java-home.zsh"
# [ asdf ]


# [ path variables ]
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.local/share/JetBrains/Toolbox/bin:$PATH"
export PATH="$HOME/.local/share/JetBrains/Toolbox/scripts:$PATH"
export PATH="$HOME/.asdf/shims:$PATH"
# [ path variables ]


# [ custom aliases ]
alias ls="lsd"
alias ll="ls -lA"
# [ custom aliases ]
