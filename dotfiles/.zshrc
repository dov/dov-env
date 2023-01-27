# Oh my 
# Disable some plugins while running in Emacs
if [[ -n "$INSIDE_EMACS" ]]; then
  plugins=(git)
else
  plugins=(git
           zsh-autosuggestions)

fi

ZSH_THEME=bira
DISABLE_AUTO_UPDATE="true" 
export ZSH=/home/dov/git/dov-env/zsh/oh-my-zsh
DISABLE_AUTO_TITLE="true"
source $ZSH/oh-my-zsh.sh
CASE_SENSITIVE="true"

# settings
setopt extendedglob autolist listtypes
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=$HOME/.zsh/history
setopt complete_in_word
setopt multios
setopt hist_ignore_dups
unset hup
stty -ixon

# Shell functions
setenv() { export $1=$2 }  # csh compatibility

# Speed up interactive work - See http://www.webupd8.org/2010/11/alternative-to-200-lines-kernel-patch.html
#if [ "$PS1" ] ; then  
#    mkdir -m 0700 /sys/fs/cgroup/cpu/user/$$
#    echo $$ > /sys/fs/cgroup/cpu/user/$$/tasks
#fi

if [[ $TERM == "xterm" || $TERM == "xterm-256color" || $TERM == "rxvt" || $TERM == "screen" ]]; then
    # Change title when switching directories
    chpwd () { print -Pn "\e]0;(Z) $USER@$HOST: [%~]\a" }
#    PROMPT="> "
    alias ls="ls -F --color=auto"
elif [[ $TERM == "screen-256color" || $TERM == "tmux-256color" ]]; then
#    chpwd () { print -Pn "\e]0;<Z> $USER@$HOST: [%~]\a" }
     chpwd () { print -Pn "\ePtmux;\e\e]0;(Z) $USER@$HOST: [%~]\a\e\\" }
#    PROMPT="> "
    alias ls="ls -F --color=auto"
elif [[ -n $INSIDE_EMACS ]]; then
    unsetopt zle
#    PROMPT="> "
    export PAGER=cat
else 
    PROMPT="> "

    if [[ $TERM == "linux" ]]; then
	loadkeys .dvorak.map
    fi
fi

if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
#  unfunction precmd
#  unfunction preexec
  PS1='$ '
fi

# stty
stty intr 
stty kill 
stty erase 
stty susp 
stty -istrip


# Make the title change after each command has been completed
#[[ $EMACS = t ]] || precmd () { cd . } 

# Filename suffixes to ignore during completion
fignore=(.o .c~ .old .pro)

# Completion control
compctl -g '*(-/)' cd
compctl -g '*.tex *(/)' tex
compctl -g '*.tex *(/)' latex
compctl -g '*.dpf *(/)' dpfview
compctl -g '*.dpf *(/)' dpf2ps
compctl -g '*.dvi *(/)' dvitops
compctl -g '*.dvi *(/)' xdvi
compctl -g '*.lyx *(/)' lyx
compctl -g '*.mp *(/)' mp
compctl -g '*.(<1-9>|man|n)' manm
compctl -C -c -x 'C[0,*/*]' -g '*[^~](*)' + -c

function term_set_title() {
	emulate -L zsh
	local term_is_known=0 term_is_multi=0
	if [[ \
		$TERM == rxvt-unicode*
		|| $TERM == xterm*
		|| ! -z $TMUX
	]] then
		term_is_known=1
	fi
	if [[ ! -z $TMUX ]] then
		term_is_multi=1
	fi
	if [[ $term_is_known -ne 1 ]] then
		return
	fi
	printf '\033]0;%s\007' ${1//[^[:print:]]/}
	if [[ \
		$TERM_TITLE_SET_MULTIPLEXER -eq 1
		&& $term_is_multi -eq 1
	]] then
#		printf '\033k%s\033\\' ${1//[^[:print:]]/}
	fi
}

function emacs-nw() {
    TERM=xterm-24bit emacs -nw
}

function term_title_get_command() {
	emulate -L zsh
	local job_text job_key
	typeset -g RETURN_COMMAND
	RETURN_COMMAND=$1
	# Since ~4.3.5, patch:
	# "users/11818: allow non-numeric keys for job status parameters"
	# it is possible to use the `fg ...` or `%...` description as a key
	# in $jobtexts.
	case $1 in
		%*) job_key=$1 ;;
		fg) job_key=%+ ;;
		fg*) job_key=${(Q)${(z)1}[2,-1]} ;;
		*) job_key='' ;;
	esac
	if [[ -z $job_key ]] then
		# not a "job to foreground" command - use it as is
		return
	fi
	job_text=${jobtexts[$job_key]} 2> /dev/null
	if [[ -z $job_text ]] then
		# job lookup failed - use the original command
		return
	fi
	RETURN_COMMAND=$job_text
}

function term_title_precmd() {
	emulate -L zsh
	local cmd='zsh'
	local dir='%~'
	term_set_title '<'$USER@$HOST'>'\ $cmd:${(%)dir}
}

function term_title_preexec() {
	emulate -L zsh
	term_title_get_command $1
	local cmd=$RETURN_COMMAND
	local dir='%~'
	term_set_title '<'$USER@$HOST'>'\ $cmd:${(%)dir}
}

# Check the devices by doing v4l2-ctl --list-devices
function zoom-zoom-nice() {
    v4l2-ctl -d1 --set-ctrl zoom_absolute=158
    v4l2-ctl -d1 --set-ctrl pan_absolute=3600
    v4l2-ctl -d1 --set-ctrl tilt_absolute=7200
}

function zoom-zoom-out() {
    v4l2-ctl -d1 --set-ctrl zoom_absolute=100
}

autoload -Uz add-zsh-hook
if [[ -n "$INSIDE_EMACS" ]]; then
else
  add-zsh-hook precmd term_title_precmd
  add-zsh-hook preexec term_title_preexec
fi

# Make the title change after each command has been completed
[[ $EMACS = t ]] || precmd () { cd . } 

# Filename suffixes to ignore during completion
fignore=(.o .c~ .old .pro)

# Completion control
compctl -g '*(-/)' cd
compctl -g '*.tex *(/)' tex
compctl -g '*.tex *(/)' latex
compctl -g '*.dpf *(/)' dpfview
compctl -g '*.dpf *(/)' dpf2ps
compctl -g '*.dvi *(/)' dvitops
compctl -g '*.dvi *(/)' xdvi
compctl -g '*.lyx *(/)' lyx
compctl -g '*.mp *(/)' mp
compctl -g '*.(<1-9>|man|n)' manm
compctl -C -c -x 'C[0,*/*]' -g '*[^~](*)' + -c

# ls aliases taken from the zsh distribution
alias lsd='ls -ld *(-/DN)'
alias lsa='ls -ld .*'
alias ls='ls -F --color=auto'
# Not sure which of these is less introsive
xd() { mkdir -p $1 && cd $1 }
mcd() { mkdir -p $1 && cd $1 }
mkcd() { mkdir -p $1 && cd $1 }
ccd() { mkdir -p $1 && cd $1 }
alias less='less -R'
grel() { grep --color=always $* | less -R }
ackl() { ack --color $* | less -R }
gitgrep() { grep --color=always $* "`git ls-files`" | less -R }
qtpylab() {  /usr/local/bin/ipython qtconsole --pylab=inline & }
toxclip() { echo -n $*| perl -ne 'chomp; print' | xclip }

# Some single characters. Is this too wild?
alias -g M='|more'
alias -g H='|head'
alias -g T='|tail'


# aliases
alias vi=vim
alias nautilus="nautilus --no-desktop"
alias hostname='uname -n'
# alias mbox="frm -s"
alias xgraph='xgraph \=550x425'
alias gsn="gs -sPAPERSIZE=a4 -DNODISPLAY"
alias gs="gs -sPAPERSIZE=a4"
alias gsx="gs -dNOPLATFONTS -sDEVICE=x11alpha -sPAPERSIZE=a4"
alias oldgv='ghostview -arguments "-dNOPLATFONTS -sDEVICE=x11alpha" -geometry +100+0 -a4 -magstep -1'
alias oldgvl="ghostview -geometry +30+100 -a4 -magstep -1 -landscape"
alias pdf2ps="acroread -toPostScript"
alias xterm="xterm -fn lucidasanstypewriter-bold-14 -bg grey20 -fg green -cr green -sb -sl 1000"
alias cds=cd
alias poehebfax="poe -rmarg 50 -nohead -tmarg 50 -stdout -columns 1 -portrait -fontscale 11 -font GamCour -reverse"
alias cpan="perl -MCPAN -e shell"
alias umask-g+w='umask 002'
alias umask-default='umask 022'
alias umask-g-w='umask 022'
alias ps2pdf="ps2pdf -sPAPERSIZE=a4 "
alias wdx="echo -n 'X <= '; pwd; pwd | perl -pe 'chomp' | xclip ; pwd | perl -pe 'chomp' | xclip -selection clip"
alias aaaa='setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll us,il'
alias dvorak='xkbcomp -w0 ~/.xkbmap $DISPLAY; xset r rate 200 30'
alias dddd='xkbcomp -w0  ~/.xkbmap $DISPLAY; xset r rate 200 30'
alias sudo='sudo env PATH=$PATH'

# solaris stuff
if [[ `uname -s` == Solaris ]]; then
    alias perldl="PATH=/data/alg/local/bin:/usr/local/bin:$PATH perldl"
    alias tar=gtar
    alias memos='/opt/palm/bin/memos'
    alias vi=vim
    alias sdd-tool=/home/dov/tools/src/sdd-tool/sdd-tool
    alias curl='curl -x http://proxy:8080 -H "Proxy-authorization: Basic ZG92OmdyYXNwYXJ2"'
    alias malsync='malsync -p proxy -r 8080 -u dov -d grasparv'
    alias start_fetchmail="fetchmail"
    alias xmcd="setenv XAPPLRESDIR /data/alg/local/lib/X11/app-defaults/; /data/alg/local/bin/xmcd";
    alias dbx5=/opt/SUNWspro/bin/dbx
fi

palmemo () { install-memo /dev/ttyb -c Misc $* }

# tmux
tmux-title () { printf "\033k${1}\033\\" }

# functions
manm () { nroff -man $* | less }
mp () { export TEXFONTS=/data/alg/local/teTeX/texmf/fonts/tfm/public/cm;
        /home/dov/bin/mp $*; unset TEXFONTS }
gspage () { psselect $2 $1 > /tmp/$$.ps; gs /tmp/$$.ps; rm /tmp/$$.ps }
dvipage () { dvips $1; psselect $2 "$1".ps > /tmp/$$.ps; gs /tmp/$$.ps; rm /tmp/$$.ps }
texbook () { xdvi +9 -expert -copy -paper us -s 6 -margins 2.54cm -sidemargin 1.9cm -geometry 650x870 /home/dov/man/texman.dvi }
alias wget="wget --proxy=on"
dvd2iso () { 
    image="/isos/movies/dvd.iso";
    echo -n "Creating $image...";
    dd if=/dev/dvd of=$image;
    echo "Done..."
}

# various other settings
# Exclude the slash and equal signs

ttyctl -f

fixtty () { 
    stty ospeed 9600 parenb cstopb -clocal loblk -istrip -ixoff 
}


WORDCHARS='*?_-.[]~&;\!#$%^(){}<>'
slash_in_word=0

set-slash-in-word () {
    WORDCHARS='*?_-.[]~&;\!\#\$%^(){}<>/'
}

set-noslash-in-word () {
    WORDCHARS='*?_-.[]~&;\!\#\$%^(){}<>'
}

# These functions are mainly used interactively
copy-last-to-whitespace () {
    set-slash-in-word
    zle copy-prev-word
    set-noslash-in-word
}

back-to-whitespace () {
    set-slash-in-word 
    zle backward-word
    set-noslash-in-word
}

forward-to-whitespace () {
    set-slash-in-word 
    zle forward-word
    set-noslash-in-word
}

backward-kill-to-whitespace () {
    set-slash-in-word 
    zle backward-kill-word
    set-noslash-in-word
}

toggle-slash-in-word () {
    slash_in_word=$[1-$slash_in_word]
    if (($slash_in_word)) {
        WORDCHARS='*?_-.[]~&;\!\#\$%^(){}<>/'
    } else {
        WORDCHARS='*?_-.[]~&;\!\#\$\%^(){}<>'
    }
}

# picture rotation routines
rot270() { jpegtran -rotate 270 $1 > $1.tmp; mv $1.tmp $1 }
rot90() { jpegtran -rotate 90 $1 > $1.tmp; mv $1.tmp $1 }

# transfer routines
toxfer() { scp $* imagic.weizmann.ac.il:~dov/public_html/xfer }

# misc functions
xdump() { xxd -g 1 $* | less }

# Create widgets from functions that we want to bind
zle -N copy-last-to-whitespace
zle -N back-to-whitespace
zle -N forward-to-whitespace
zle -N backward-kill-to-whitespace

# bindkeys
bindkey -e
bindkey "^[[a" forward-word    
bindkey "^[[b" backward-word   
bindkey "^[[c" backward-kill-word
# The following should match the definition in .Xdefaults
bindkey "^[[1;2C" backward-kill-word
bindkey "^[[7;5~" backward-kill-word
bindkey "^[[f" undo
bindkey "n" history-beginning-search-forward
bindkey "p" history-beginning-search-backward
bindkey "\M-n" history-beginning-search-forward
bindkey "\M-f" history-beginning-search-backward
bindkey "\M-q" push-line
bindkey "\M-b" backward-word
bindkey "\M-f" forward-word
bindkey "\M-x" execute-named-cmd
bindkey "\t" expand-or-complete-prefix
bindkey '[i' back-to-whitespace
bindkey '[k' back-to-whitespace
bindkey '[h' forward-to-whitespace
bindkey '[j' forward-to-whitespace
bindkey "\M-" backward-kill-to-whitespace
bindkey "\M-" backward-kill-to-whitespace
bindkey 'c' copy-last-to-whitespace
bindkey '\M-c' copy-last-to-whitespace
bindkey '\M-B' back-to-whitespace
bindkey '\M-F' forward-to-whitespace
bindkey '[3D' back-to-whitespace
bindkey '[3C' forward-to-whitespace

# clipboard support
copy-region-as-kill () {
  zle kill-region
  zle yank
}

x-copy-region-as-kill () {
  copy-region-as-kill
  print -rn -- $CUTBUFFER | xsel -i
}
zle -N x-copy-region-as-kill
x-kill-end-of-line () {
  zle kill-line
  print -rn -- $CUTBUFFER | xsel -i
}
zle -N x-kill-end-of-line

x-kill-region () {
  zle kill-region
  print -rn -- $CUTBUFFER | xsel -i
}
zle -N x-kill-region
x-yank () {
  CUTBUFFER=$(xsel -o </dev/null )
  zle yank
}
zle -N x-yank
if [[ x$DISPLAY != x ]]; then
    bindkey -e 'w' x-copy-region-as-kill
    bindkey -e '\M-w' x-copy-region-as-kill
    bindkey -e '^W' x-kill-region
    bindkey -e '^Y' x-yank
    bindkey -e '^k' x-kill-end-of-line
fi

# path
path=(/usr/local/bin
      /usr/local/blender-2.80-linux-glibc217-x86_64
      /usr/java/jre1.5.0_06/bin
      /usr/X11R6/bin 
      $HOME/scripts
      $HOME/Scripts 
      $HOME/hd/scripts 
      $HOME/bin
      $HOME/hd/bin
      $HOME/go/bin
      $HOME/.cargo/bin
      /usr/X11R6/bin
      /usr/bin
      /bin
      /usr/sbin
      /sbin
      /space/Android/Sdk/platform-tools
      )

# environment variables
if [[ `uname -s` == Linux ]] {
    #setenv PERLVER `perl -MConfig -e 'print $Config{api_versionstring}'`
    setenv PERLVER "5.12.0"
    setenv PERLOS `perl -MConfig -e 'print $Config{archname}'`
    setenv PERL5LIB /usr/local/lib/perl5/${PERLVER}:/usr/local/lib/perl5/${PERLVER}/${PERLOS}:/usr/local/lib/perl5/site_perl/${PERLVER}:/usr/local/lib/perl5/site_perl/${PERLVER}/${PERLOS}:/usr/lib/perl5/vendor_perl/${PERLVER}:/usr/lib/vendor_perl/${PERLVER}/${PERLOS}:/usr/lib/vendor_perl/perl5/site_perl/${PERLVER}:/usr/lib/vendor_perl/perl5/site_perl/${PERLVER}/${PERLOS}:/nmr/dov/Projects/Lib/perl:/nmr/dov/Projects/Lib/perl/$OS/$PERLVER
    
    setenv QTDIR /usr
}
setenv XMCD_LIBDIR /usr/X11R6/lib/X11/xmcd
setenv OS linux_i386
setenv NHVL_PATH /home/dov/nhvl2/gip_db/
setenv MASEHOME /project/mase
setenv PILOTPORT /etc/udev/devices/ttyUSB1
setenv GS_LIB /usr/share/ghostscript/fonts
setenv PGPLOT_DIR /usr/local/pgplot
setenv PGPLOT_DEV      /xwindow

# Set up a public development enviroment. Works e.g. for gimp and gegl.
devl-env() {
    export PKG_CONFIG_PATH=/usr/local/devl/lib/pkgconfig 
    export LD_LIBRARY_PATH=/usr/local/devl/lib:$LD_LIBRARY_PATH 
    export PATH=/usr/local/devl/bin:$PATH 
    export CPPFLAGS="-I/usr/local/devl/include"
    export LDFLAGS="-L/usr/local/devl/lib"
    export ACLOCAL_FLAGS="-I /usr/local/devl/share/aclocal -I /usr/share/aclocal"
#    export PYTHONPATH=/usr/local/devl/lib/python2.7/site-packages:$PYTHONPATH
    export GI_TYPELIB_PATH=/usr/local/devl/lib/girepository-1.0
}

# Display an image from a webcam on the N900
vcam() {
  sudo n900-up.sh
  zap -9 -ni -9 gst-launch
  ssh groma "env DISPLAY=:0 ssh -X dov@usb gst-launch v4l2src device=/dev/video0 ! 'video/x-raw-yuv,width=320,height=240,framerate=25/1' ! videobalance brightness=0.25 ! aspectratiocrop aspect-ratio=16/9 ! videoflip method=horizontal-flip ! xvimagesink"
}

# Weizmann specific stuff
if [[ $HOST == "echo" || $HOST == "mega" ]]; then
# environment variables for zsh
  export http_proxy=http://wwwproxy.weizmann.ac.il:8080
  export PRINTER=br1a
fi

# Hadassa Group development
#export DCMDICTPATH=/usr/local/lib/dicom.dic

export GDK_USE_XFT=1
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/share/pkgconfig
export CVS_RSH=ssh
export ALGLIBS=/home/dov/orbotech/alglibs
export EDITOR=vim
#export PYTHONPATH=/usr/local/lib/python2.7/site-packages:/usr/local/lib64/python2.7/site-packages:
export PYTHONIOENCODING=utf-8
python27path () {
  export PYTHONPATH=/usr/local/lib/python2.7/site-packages:/usr/local/lib64/python2.7/site-packages:
}
python3path () {
  export PYTHONPATH=/usr/local/lib/python3/site-packages:/usr/local/lib64/python3/site-packages:/usr/local/lib/python3.11/site-packages:/usr/local/lib64/python3.11/site-packages
}
alias dnfs='sudo dnf search'
alias dnfi='sudo dnf -y install'
#export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on"
export POEFONTPATH=/home/dov/lib/psfonts
export HALCONROOT=/usr/local/halcon
unset SSH_ASKPASS
alias mntsec='sudo /sbin/modprobe cryptoloop; sudo /sbin/modprobe blowfish; sudo losetup -e blowfish /dev/loop0 /space1/secure; sudo mount -t ext2 /dev/loop0 /mnt/loop'
alias umntsec='sudo umount /dev/loop0; sudo losetup -d /dev/loop0; sudo sync'
alias android-studio='/terra/space/android-studio/bin/studio.sh'
alias ipython='env PYTHONPATH=/usr/local/lib/python3.11/site-packages:/usr/local/lib64/python3.11/site-packages:/usr/local/lib/python3/site-packages:/usr/local/lib64/python3/site-packages /usr/bin/ipython3'
#alias pip='env PYTHONPATH=/usr/local/lib/python2.7/site-packages:/usr/local/lib64/python2.7/site-packages: /usr/bin/pip'
setenv PERLVER `perl -MConfig -e 'print $Config{version}'`
setenv PERLOS `perl -MConfig -e 'print $Config{archname}'`
setenv PERL5LIB /usr/local/lib/perl5/${PERLVER}:/usr/local/lib/perl5/${PERLVER}/${PERLOS}:/usr/local/lib/perl5/site_perl/${PERLVER}:/usr/local/lib/perl5/site_perl/${PERLVER}/${PERLOS}:/usr/local.local/lib/perl5/${PERLVER}:/usr/local.local/lib/perl5/${PERLVER}/${PERLOS}:/usr/local.local/lib/perl5/site_perl/${PERLVER}:/usr/local.local/lib/perl5/site_perl/${PERLVER}/${PERLOS}:/usr/lib/perl5/vendor_perl/${PERLVER}:/usr/lib/vendor_perl/${PERLVER}/${PERLOS}:/usr/lib/vendor_perl/perl5/site_perl/${PERLVER}:/usr/lib/vendor_perl/perl5/site_perl/${PERLVER}/${PERLOS}:/nmr/dov/Projects/Lib/perl:/nmr/dov/Projects/Lib/perl/$OS/$PERLVER
export LESSCHARSET=utf-8
export MJRP=XjetApps/MetalJet/Apps/Project/qt
export PE_HOME=/home/dov/git/MetalJet
export THEONORC=/home/dov/.theanorc
export MJQT=$PE_HOME/$MJRP
export SJQT=$PE_HOME/$MJRP

# Configure with debugging
alias configuredebug='env CPPFLAGS=-DDEBUG CFLAGS="-g -O0" CXXFLAGS="-g -O0" ./configure'

# Make perl stop complaining
unset LANG
setenv LC_ALL_C

# Cuda
export CUDA_SDK_PATH=/usr/local/cuda-11.1
export CUDA_SDK_ROOT_DIR=$CUDA_SDK_PATH
export PATH=$CUDA_SDK_PATH/bin:$PATH
export CUDA_NVCC_FLAGS='-ccbin=/usr/local/gcc-8.5.0/bin/g++'
export CYCLES_CUDA_EXTRA_CFLAGS="-ccbin /usr/local/gcc-8.5.0/bin/gcc"
export PATH=${CUDA_SDK_ROOT_DIR}/bin:$PATH

# This should be the default for all cuda calculations...
export PYCUDA_DEFAULT_NVCC_FLAGS=--std=c++11

#. /home/dov/lib/zsh/mouse.zsh
#zle-toggle-mouse
#<Esc>m to toggle the mouse in emacs mode
#bindkey -M emacs '\em' zle-toggle-mouse
alias aaaa="setxkbmap en_US"
alias dvorak="xkbcomp ~/.xkbmap $DISPLAY"
alias samiam=/usr/local/samiam/runsamiam 
alias cl3="gcal -H '\e[42m\e[30m:\e[0;1m:\e[31;1m:\e[0m' -q IL ."
alias hcal="gcal -H '\e[42m\e[30m:\e[0;1m:\e[31;1m:\e[0m' -q IL -n ."
#alias krita=/usr/local/krita/bin/krita
export RSHELL_PORT=/dev/ttyUSB0
alias rshell='rshell -e vim'
#export ANDROID_EMULATOR_FORCE_32BIT=1
export ANDROID_HOME=/terra/space/Android/Sdk
export PATH=$ANDROID_HOME/platform-tools:$PATH
export GRADLE_USER_HOME=/terra/space/dov/.gradle
export QT_QPA_PLATFORMTHEME=qt5ct


# Go support
export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

# Rust/Cargo support
export PATH=$PATH:$HOME/.cargo/bin

zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

hash -d mjqt=/home/dov/git/MetalJet/XjetApps/MetalJet/Apps/Project/qt

PERL5LIB="/home/dov/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/dov/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/dov/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/dov/perl5"; export PERL_MM_OPT;

# Bsolar
export TILREPO=/home/dov/hd/bsolar/db/tilrepo/
export CLIMATO_DIR=/home/dov/hd/bsolar/ws/bsolar/db/CLIMATO/
export ETS_TOOLKIT=qt4  

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/dov/.sdkman"
[[ -s "/home/dov/.sdkman/bin/sdkman-init.sh" ]] && source "/home/dov/.sdkman/bin/sdkman-init.sh"


[ -f ~/.zsh/condaprompt.zsh ] && source ~/.zsh/condaprompt.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# My prompt for runnig anaconda
conda_setup()
{
  export PATH=/home/dov/anaconda3/bin:$PATH
  export LD_LIBRARY_PATH=/terra/space/Anaconda3/fastai/lib/:$LD_LIBRARY_PATH
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/dov/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/dov/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/dov/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/dov/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
}

# Vulkan

# Copyright (c) 2015-2019 LunarG, Inc.

# source this file into an existing shell.

VULKAN_SDK=/usr/local/vulkan/1.2.170.0/x86_64
export VULKAN_SDK
export PATH="$VULKAN_SDK/bin:$PATH"
export LD_LIBRARY_PATH="$VULKAN_SDK/lib:$LD_LIBRARY_PATH"
export VK_LAYER_PATH="$VULKAN_SDK/etc/vulkan/explicit_layer.d"

# For cuda
export LD_LIBRARY_PATH=/usr/local/cuda-11.1/lib64/:/usr/local/cuda-11.1/targets/x86_64-linux/lib/:$LD_LIBRARY_PATH

# For cross compilation
export ESPDIDF=/terra/space/pub-repos/esp/esp-idf
#export PATH=/terra/space/pub-repos/esp/crosstool-NG/builds/xtensa-esp32-elf/bin/:$PATH
export PATH="$PATH:/terra/space/pub-repos/esp/xtensa-lx106-elf/bin"

# mpremote shortcuts
alias mpr-reset='mpremote exec "import machine; machine.reset()"'
alias mpr-repl='mpremote repl'
alias mpr-ls='mpremote ls'
alias mpr-rm='mpremote rm'
mpr-run() {
    mpremote exec "import $1"
}
mpr-put() {
   if [ -z "$2" ]
   then
       mpremote cp $1 :`basename $1`
   else
       mpremote cp $1 :$2
   fi
}
mpr-get() {
   if [ -z "$2" ]
   then
       mpremote cp :`basename $1` $1
   else
       mpremote cp :`basename $1` $2
   fi
}
