# @fish-lsp-disable 2002

if test "$DISPLAY" = "" -a "$XDG_VTNR" = 1
    tbsm
end

set -gx FEDORACFG $HOME/Documents/github.com/deirn/fedoracfg

# [ direnv ]
eval (direnv hook fish)
# [ direnv ]

# [ vterm ]
function vterm_printf
    if test -n "$TMUX"
        printf "\ePtmux;\e\e]%s\007\e\\" $argv
    else if string match -q "screen*" $TERM
        printf "\eP\e]%s\007\e\\" $argv
    else
        printf "\e]%s\e\\" $argv
    end
end
# [ vterm ]

# [ tide ]
# tide configure --auto \
#                --style=Lean \
#                --prompt_colors='16 colors' \
#                --show_time=No \
#                --lean_prompt_height='Two lines' \
#                --prompt_connection=Solid \
#                --prompt_spacing=Compact \
#                --icons='Few icons' \
#                --transient=Yes \
set -g tide_character_icon '>'
set -g tide_character_vi_icon_default N
set -g tide_character_vi_icon_visual V
set -g tide_character_vi_icon_replace R
# [ tide ]

# [ path variables ]
fish_add_path $HOME/.local/bin
fish_add_path $HOME/.config/emacs/bin
fish_add_path $HOME/.local/share/JetBrains/Toolbox/bin
fish_add_path $HOME/.local/share/JetBrains/Toolbox/scripts
fish_add_path $HOME/Android/Sdk/platform-tools
fish_add_path $HOME/.pub-cache/bin
fish_add_path $HOME/.local/share/qlot/bin
fish_add_path $FEDORACFG/bin
# [ path variables ]

# [ vi emulation ]
fish_vi_key_bindings

if status is-interactive
    set fish_cursor_default block blink
    set fish_cursor_insert line blink
    set fish_cursor_replace_one underscore blink
    set fish_cursor_visual block
end
# [ vi emulation ]

# [ fzf ]
set -gx FZF_DEFAULT_COMMAND "fd --type f"
set -gx FZF_DEFAULT_OPTS "--color 16 --cycle --layout=reverse --border --height=90% --preview-window=wrap --marker='*'"
# [ fzf ]

# [ mise ]
mise activate fish | source
# [ mise ]

# [ uv ]
uv generate-shell-completion fish | source
uvx --generate-shell-completion fish | source
# [ uv ]

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
abbr -a rm trash
abbr -a rm! /usr/bin/rm
abbr -a c clear
abbr -a dnf sudo dnf

alias zen-browser="flatpak run app.zen_browser.zen"
# [ abbreviations ]

# [ zoxide ]
zoxide init fish | source
# [ zoxide ]
