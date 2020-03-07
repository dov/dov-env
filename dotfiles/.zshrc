# Oh my 
DISABLE_AUTO_UPDATE="true" 
export ZSH=/home/dov/git/dov-env/zsh/oh-my-zsh
plugins=(git)
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
    PROMPT="> "
    alias ls="ls -F --color=auto"
elif [[ $TERM == "screen-256color" ]]; then
#    chpwd () { print -Pn "\e]0;<Z> $USER@$HOST: [%~]\a" }
     chpwd () { print -Pn "\ePtmux;\e\e]0;(Z) $USER@$HOST: [%~]\a\e\\" }
    PROMPT="> "
    alias ls="ls -F --color=auto"
elif [[ -n $INSIDE_EMACS ]]; then
    unsetopt zle
    PROMPT="> "
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

: ${TERM_TITLE_SET_MULTIPLEXER:=1}

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
		printf '\033k%s\033\\' ${1//[^[:print:]]/}
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

autoload -Uz add-zsh-hook
add-zsh-hook precmd term_title_precmd
add-zsh-hook preexec term_title_preexec


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
alias visit="/usr/local.local/bin/visit"
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
alias gv='ghostview -arguments "-dNOPLATFONTS -sDEVICE=x11alpha" -geometry +100+0 -a4 -magstep -1'
alias gvl="ghostview -geometry +30+100 -a4 -magstep -1 -landscape"
alias pdf2ps="acroread -toPostScript"
alias xterm="xterm -fn lucidasanstypewriter-bold-14 -bg grey20 -fg green -cr green -sb -sl 1000"
alias cds=cd
alias poehebfax="poe -rmarg 50 -nohead -tmarg 50 -stdout -columns 1 -portrait -fontscale 11 -font GamCour -reverse"
alias cpan="perl -MCPAN -e shell"
alias pilot-mail pilot-mail /dev/pilot -f dov@imagic.weizmann.ac.il -k keep -s my-pilot-sm -p /dev/pilot
alias umask-g+w='umask 002'
alias umask-default='umask 022'
alias umask-g-w='umask 022'
alias ps2pdf="ps2pdf -sPAPERSIZE=a4 "
alias wdx="echo -n 'X <= '; pwd; pwd | perl -pe 'chomp' | xclip ; pwd | perl -pe 'chomp' | xclip -selection clip"
alias aaaa='setxkbmap us'
alias dvorak='xkbcomp -w0 ~/.xkbmap $DISPLAY; xset r rate 200 30'
alias dddd='xkbcomp -w0  ~/.xkbmap $DISPLAY; xset r rate 200 30'
alias sudo='sudo env PATH=$PATH'
alias mount-prealpha='sudo mount //prealpha/d /mnt/prealpha/d -o user=machine,passw='Amag432!',dir_mode=0777,file_mode=0777'

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
x-copy-region-as-kill () {
  zle copy-region-as-kill
  print -rn $CUTBUFFER | xsel -i
}
zle -N x-copy-region-as-kill
x-kill-end-of-line () {
  zle kill-line
  print -rn $CUTBUFFER | xsel -i
}
zle -N x-kill-end-of-line

x-kill-region () {
  zle kill-region
  print -rn $CUTBUFFER | xsel -i
}
zle -N x-kill-region
x-yank () {
  CUTBUFFER=$(xsel -o </dev/null )
  zle yank
}
zle -N x-yank
if [[ x$DISPLAY != x ]]; then
    zle -N x-yank
    bindkey -e '\eW' x-copy-region-as-kill
    bindkey -e '^W' x-kill-region
    bindkey -e '^Y' x-yank
    bindkey -e '^k' x-kill-end-of-line
fi

# path
path=(/usr/local/bin 
      /usr/java/jre1.5.0_06/bin
      /usr/X11R6/bin 
      $HOME/scripts
      $HOME/Scripts 
      $HOME/hd/scripts 
      $HOME/bin
      $HOME/hd/bin
      $HOME/go/bin
      /usr/X11R6/bin
      /usr/bin
      /bin
      /usr/sbin
      /sbin
      /space/dov/android/android-sdk-linux/platform-tools
      /home/dov/hd/bin/adb
      /usr/lib64/openmpi/bin/
      )

# environment variables
if [[ `uname -s` == Linux ]] {
    #setenv PERLVER `perl -MConfig -e 'print $Config{api_versionstring}'`
    setenv PERLVER "5.12.0"
    setenv PERLOS `perl -MConfig -e 'print $Config{archname}'`
    setenv PERL5LIB /usr/local/lib/perl5:/usr/local/lib/perl5/${PERLVER}:/usr/local/lib/perl5/${PERLVER}/${PERLOS}:/usr/local/lib/perl5/site_perl/${PERLVER}:/usr/local/lib/perl5/site_perl/${PERLVER}/${PERLOS}:/usr/lib/perl5/vendor_perl/${PERLVER}:/usr/lib/vendor_perl/${PERLVER}/${PERLOS}:/usr/lib/vendor_perl/perl5/site_perl/${PERLVER}:/usr/lib/vendor_perl/perl5/site_perl/${PERLVER}/${PERLOS}:/nmr/dov/Projects/Lib/perl:/nmr/dov/Projects/Lib/perl/$OS/$PERLVER
    
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
export PYTHONIOENCODING=utf-8

gtk-head-env() {
    export PKG_CONFIG_PATH=/opt/gtk-head/lib/pkgconfig:$PKG_CONFIG_PATH 
    export LD_LIBRARY_PATH=/opt/gtk-head/lib
    export PATH=/opt/gtk-head/bin:$PATH
    export GTK2_RC_FILES=/home/dov/.gtkrc-2.0.head
}

# Set up a public development enviroment. Works e.g. for gimp and gegl.
pub-dev-env() {
    export PKG_CONFIG_PATH=/usr/local/pub-dev/lib/pkgconfig 
    export LD_LIBRARY_PATH=/usr/local/pub-dev/lib:$LD_LIBRARY_PATH 
    export PATH=/usr/local/pub-dev/bin:$PATH 
    export CPPFLAGS="-I/usr/local/pub-dev/include"
    export LDFLAGS="-L/usr/local/pub-dev/lib"
    export ACLOCAL_FLAGS="-I /usr/local/public-dev/share/aclocal -I /usr/share/aclocal"
    export PYTHONPATH=/usr/local/public-dev/lib/python2.7/site-packages:$PYTHONPATH
    export GI_TYPELIB_PATH=/usr/local/public-dev/lib/girepository-1.0
}
gtk210-env() {
    export PKG_CONFIG_PATH=/opt/gtk-2.10/lib/pkgconfig:$PKG_CONFIG_PATH 
    export LD_LIBRARY_PATH=/opt/gtk-2.10/lib
    export PATH=/opt/gtk-2.10/bin:$PATH
}
mingw32env() {
    TARGET=mingw32
    export PREFIX="/usr/local/mingw32"
    export CC="i686-w64-mingw32-gcc -mms-bitfields"
    export CXX="i686-w64-mingw32-g++ -mms-bitfields"
    export AR=i686-w64-mingw32-ar
    export RANLIB=i686-w64-mingw32-ranlib
    export CFLAGS="-O2 -march=i586 -mms-bitfields"
    export CXXFLAGS="-O2 -march=i586 -mms-bitfields"
    export PKG_CONFIG_PATH=$PREFIX/$TARGET/lib/pkgconfig
    export PATH=$PREFIX/bin:$PREFIX/$TARGET/bin:/bin:/usr/bin
    export LD_LIBRARY_PATH=$PREFIX/$TARGET/lib
    export LDFLAGS=-L$PREFIX/$TARGET/lib
    export OBJDUMP=$PREFIX/bin/mingw32-objdump
    export HOST_CC=/usr/bin/gcc
    export OBJSUFFIX=".obj"
    export PROGSUFFIX=".exe"
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
export DCMDICTPATH=/usr/local/lib/dicom.dic

export GDK_USE_XFT=1
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/share/pkgconfig
export CVS_RSH=ssh
export ALGLIBS=/home/dov/orbotech/alglibs
export SVN_EDITOR=vim
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:/usr/local/lib64/python2.7/site-packages:/home/dov/Experiments/ExtLib
#export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on"
export POEFONTPATH=/home/dov/lib/psfonts
export HALCONROOT=/usr/local/halcon
unset SSH_ASKPASS
alias mntsec='sudo /sbin/modprobe cryptoloop; sudo /sbin/modprobe blowfish; sudo losetup -e blowfish /dev/loop0 /space1/secure; sudo mount -t ext2 /dev/loop0 /mnt/loop'
alias umntsec='sudo umount /dev/loop0; sudo losetup -d /dev/loop0; sudo sync'
alias android-studio='/space/android-studio/bin/studio.sh'
setenv PERLVER `perl -MConfig -e 'print $Config{version}'`
setenv PERLOS `perl -MConfig -e 'print $Config{archname}'`
setenv PERL5LIB /usr/local/lib/perl5/${PERLVER}:/usr/local/lib/perl5/${PERLVER}/${PERLOS}:/usr/local/lib/perl5/site_perl/${PERLVER}:/usr/local/lib/perl5/site_perl/${PERLVER}/${PERLOS}:/usr/local.local/lib/perl5/${PERLVER}:/usr/local.local/lib/perl5/${PERLVER}/${PERLOS}:/usr/local.local/lib/perl5/site_perl/${PERLVER}:/usr/local.local/lib/perl5/site_perl/${PERLVER}/${PERLOS}:/usr/lib/perl5/vendor_perl/${PERLVER}:/usr/lib/vendor_perl/${PERLVER}/${PERLOS}:/usr/lib/vendor_perl/perl5/site_perl/${PERLVER}:/usr/lib/vendor_perl/perl5/site_perl/${PERLVER}/${PERLOS}:/nmr/dov/Projects/Lib/perl:/nmr/dov/Projects/Lib/perl/$OS/$PERLVER
export LESSCHARSET=utf-8
export CUDA_SDK_PATH=/usr/local/cuda-8.0
export PATH=$CUDA_SDK_PATH/bin:$PATH
export CUDA_NVCC_FLAGS=-ccbin=/usr/local/gcc-5.4.0/bin/g++
export MJRP=XjetApps/MetalJet/Apps/Project/qt
export MJQT=/home/dov/git/SolarJet/$MJRP
# Make perl stop complaining
unset LANG
setenv LC_ALL_C

# Cuda
export CUDA_SDK_ROOT_DIR=/usr/local/cuda-7.5
export PATH=${CUDA_SDK_ROOT_DIR}/bin:$PATH
export CUDA_NVCC_FLAGS='-ccbin=/usr/local/gcc-5.4.0/bin/g++'

# This should be the default for all cuda calculations...
export PYCUDA_DEFAULT_NVCC_FLAGS=--std=c++11

#. /home/dov/lib/zsh/mouse.zsh
#zle-toggle-mouse
#<Esc>m to toggle the mouse in emacs mode
#bindkey -M emacs '\em' zle-toggle-mouse
alias aaaa="setxkbmap en_US"
alias dvorak="xkbcomp ~/.xkbmap $DISPLAY"
alias samiam=/usr/local/samiam/runsamiam 
#export ANDROID_EMULATOR_FORCE_32BIT=1
export ANDROID_HOME=/space/Android/Sdk
export PATH=$ANDROID_HOME/platform-tools:$PATH
export GRADLE_USER_HOME=/space/dov/.gradle

# xjet
export QTDIR=/usr
export QMAKE=qmake-qt5
export QTWEBENGINE_REMOTE_DEBUGGING=7979
export PE_HOME=/home/dov/git/SolarJet
export SJQT=${PE_HOME}/XjetApps/MetalJet/Apps/Project/qt/
alias svn2git="rsync -av --exclude='*.dll' --exclude='*.vcproj' --exclude='.svn' --exclude='*db' --exclude='*.obj' --include='*.cpp' --include='*.h' /mnt/fdrive/svn/Projects/* ."
alias machine64bit='rdesktop-vrdp -x l -u "xjetdom\\machine" -p 123456 -g1024x768 machine64bit'
alias printer2='rdesktop-vrdp -x l -u machine -p 123456 -g1024x768 printer2'
alias minijet2='rdesktop-vrdp -x l -u minijet6 -p 123456 -g1280x1024 172.16.10.40'
alias minijet5='rdesktop-vrdp -x l -u "xjetdom\\lab" -p 123456 -g1280x1024 minijet-5'
alias mount-minijet6='mount //minijet6/D\$ -o user='xjetdom\machine',password=123456 /mnt/minijet6'
alias electronics-lab=' rdesktop -x 0x80 -u 'XJETDOM\\\\lab' -p 123456 -g1680x1050 172.16.10.170'
alias automatica='rdesktop-vrdp -u machine -p 123456 -g1024x768 automatica'
export PRINTER=Samsung-SCX-5530FN
alias mj7='rdesktop-vrdp -x l -u 'xjetdom\\\\machine' -p 123456 -g1600x1020 172.16.10.138'
#alias hydra='rdesktop-vrdp -x l -u 'xjetdom\\\\machine' -p Amag432\! -g1600x1020 hydra'
alias hydra='xfreerdp /cert-ignore /w:1600 /h:1020 /v:hydra /u:machine /p:Amag432\! +decorations +wallpaper +clipboard'
machinerdp () {
  xfreerdp /cert-ignore /w:1600 /h:1020 /v:$1 /u:machine /p:Amag432\! +decorations +wallpaper +clipboard
}
dovrdp () {
  xfreerdp /cert-ignore /w:1600 /h:1020 /v:$1 /u:dovg /p:Toco43Nika +decorations +wallpaper +clipboard
}
machine-rdesktop () {
    rdesktop -u machine -p Amag432\! -g 1600x1020 $1
}

alias alpha='xfreerdp /cert-ignore /w:1600 /h:1020 /v:alpha /u:machine /p:Amag432\! +decorations +wallpaper +clipboard'
alias hydra-rdesktop='rdesktop-vrdp -x l -u "xjetdom\\machine" -p 123456 -g1600x1020 hydra'
alias strobe='xfreerdp /w:1600 /h:1020 /v:172.16.10.193 /u:machine /p:Amag432\! +decorations +wallpaper +clipboard'
autoload -U compinit && compinit
hash -d mjqt=/home/dov/git/SolarJet/XjetApps/MetalJet/Apps/Project/qt
alias mjqt='cd ~mjqt'
alias tmux='tmux -u'

PERL5LIB="/home/dov/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/dov/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/dov/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/dov/perl5"; export PERL_MM_OPT;

machinemount () {
  sudo mount $1 $2 -o user=machine,domain=xjetdom,password='Amag432!',vers=2.1
}

# Get around warning "Couldn't register with accessibility bus"
export NO_AT_BRIDGE=1
