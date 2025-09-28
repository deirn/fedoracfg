#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# Set "Application not responding" popup time to 10 seconds
gsettings set org.gnome.mutter check-alive-timeout 10000

# Set default terminal to wezterm
gsettings set org.gnome.desktop.default-applications.terminal exec 'wezterm'
gsettings set org.gnome.desktop.default-applications.terminal exec-arg '-e'
