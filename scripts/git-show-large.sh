#!/bin/sh

# See: https://stackoverflow.com/questions/10622179/how-to-find-identify-large-commits-in-git-history

git rev-list --objects --all |
  git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' |
  sed -n 's/^blob //p' |
  grep -vF --file=<(git ls-tree -r HEAD | awk '{print $3}') |
  sort --numeric-sort --key=2 # |
#  cut -c 1-12,41- |
#  $(command -v gnumfmt || echo numfmt) --field=2 --to=iec-i --suffix=B --padding=7 --round=nearest
