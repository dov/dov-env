#!/bin/zsh

# TBD morph the md file a bit more if needed 

pandoc --embed-resources --standalone -t html --css /home/dov/git/dov-env/lib/dov-org.css "$@" | /home/dov/git/dov-env/scripts/premailer --stdout 2> /tmp/markdown-error.log
