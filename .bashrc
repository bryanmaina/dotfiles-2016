#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export VISUAL="nvim"
export BROWSER=/usr/bin/chromium
export AWT_TOOLKIT=XToolkit
export NOTES_DIR="/home/dumy/Documents/Cours_UCL/"
export XDG_CONFIG_HOME="/home/dumy/.config/"
export ANDROID_HOME="/opt/android-sdk"
#export PATH="$PATH:$HOME/.node/bin"

if which ruby >/dev/null && which gem >/dev/null; then
    PATH="$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi

# Git bash prompt
source /usr/share/git/completion/git-prompt.sh
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWDIRTYSTATE=true


alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano PKGBUILD'
alias fixit='sudo rm -f /var/lib/pacman/db.lck && sudo pacman-mirrors -g && sudo pacman -Syyuu  &&
sudo pacman -Suu'
alias wine32='env WINEARCH=win32 WINEPREFIX="/home/dumy/.wine32" wine'
alias League_of_Legends='cd /home/dumy/.wine32/drive_c/Riot\ Games/League\ of\ Legends/RADS/system/ && wine32 rads_user_kernel.exe run lol_launcher $(ls ../projects/lol_launcher/releases/) LoLLauncher.exe'

alias ls='ls --color=auto'

alias v='nvim'

# Getting the weather of any city
wego ()
{
  curl http://wttr.in/"$1"
}

# ex - archive extractor
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

#convert audios
webmTOmp4 () {
  ffmpeg -i "$1".webm -qscale 0 "$1".mp4
}
mp4TOmp3 () {
  ffmpeg -i "$1".mp4 "$1".mp3
}


#return value visualisation
 PS1="\$(if [[ \$? == 0 ]]; then echo \"\[\033[34m\]\"; else echo \"\[\033[31m\]\"; fi)\342\226\210\342\226\210 [ \W ] \[\033[0;31m\] [ \t ]\$(__git_ps1 ' (%s)')\n\[\033[m\]\342\226\210\342\226\210 "

#PS1="$(if [[ ${EUID} == 0 ]]; then echo '\[\033[01;31m\]\u@\h'; else echo '\[\033[01;32m\]\u@\h'; fi)\[\033[01;34m\] \w \[\033[01;33m\]\n \$(__git_ps1 ' (%s)') \[\033[01;34m\]\$([[ \$? != 0 ]] && echo \"\[\033[01;31m\]:(\[\033[01;34m\] \")\\$\[\033[00m\] "

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
