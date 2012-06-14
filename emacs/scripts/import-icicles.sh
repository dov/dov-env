#!/bin/bash
# Import from wiki
#
# According to the wikipage the latest version is on the wiki. This script
# does an import into this github repo.
rm *.el
wget http://www.emacswiki.org/emacs/download/icicles.el
wget http://www.emacswiki.org/emacs/download/icicles-chg.el
wget http://www.emacswiki.org/emacs/download/icicles-cmd1.el
wget http://www.emacswiki.org/emacs/download/icicles-cmd2.el
wget http://www.emacswiki.org/emacs/download/icicles-doc1.el
wget http://www.emacswiki.org/emacs/download/icicles-doc2.el
wget http://www.emacswiki.org/emacs/download/icicles-face.el
wget http://www.emacswiki.org/emacs/download/icicles-fn.el
wget http://www.emacswiki.org/emacs/download/icicles-mac.el
wget http://www.emacswiki.org/emacs/download/icicles-mcmd.el
wget http://www.emacswiki.org/emacs/download/icicles-mode.el
wget http://www.emacswiki.org/emacs/download/icicles-opt.el
wget http://www.emacswiki.org/emacs/download/icicles-var.el
