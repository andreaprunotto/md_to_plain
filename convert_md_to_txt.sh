#!/usr/bin/env bash


here=/home/prunotto/IMBI/git_projects/md_to_plain
#docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` "pandoc/latex" "$here/"testdoc.md -o "$here/"testdoc.txt
docker run --rm --volume "`pwd`:/data" --user `id -u`:`id -g` -i pandoc/latex $here/testdoc.md -o $here/testdoc.txt
