set -gx FEDORACFG $HOME/Documents/github.com/deirn/fedoracfg


# [ direnv ]
eval (direnv hook fish)
# [ direnv ]

function vterm_printf
    if test -n "$TMUX"
        printf "\ePtmux;\e\e]%s\007\e\\" $argv
    else if string match -q "screen*" $TERM
        printf "\eP\e]%s\007\e\\" $argv
    else
        printf "\e]%s\e\\" $argv
    end
end


# [ path variables ]
fish_add_path $HOME/.local/bin
fish_add_path $HOME/.config/emacs/bin
fish_add_path $HOME/.local/share/JetBrains/Toolbox/bin
fish_add_path $HOME/.local/share/JetBrains/Toolbox/scripts
fish_add_path $FEDORACFG/bin
# [ path variables ]


# [ vi emulation ]
fish_vi_key_bindings

function fish_vi_cursor
end

if status is-interactive
    set fish_cursor_default block blink
    set fish_cursor_insert line blink
    set fish_cursor_replace_one underscore blink
    set fish_cursor_visual block
end
# [ vi emulation ]


# [ fzf ]
fzf --fish | source
# [ fzf ]


# [ mise ]
mise activate fish | source
# [ mise ]


# [ npm ]
set -gx NPM_PACKAGES "$HOME/.local/share/npm_packages"
fish_add_path $NPM_PACKAGES/bin
# set -gx MANPATH (manpath) $NPM_PACKAGES/share/man
# [ npm ]


# [ pnpm ]
set -gx PNPM_HOME $HOME/.local/share/pnpm
fish_add_path $PNPM_HOME
# [ pnpm ]


# [ abbreviations ]
abbr -a ls lsd
abbr -a ll lsd -lA
abbr -a fetch fastfetch
abbr -a enw emacs -nw

alias protontricks="flatpak run com.github.Matoking.protontricks"
alias protontricks-launch="latpak run --command=protontricks-launch com.github.Matoking.protontricks"
# [ abbreviations ]
