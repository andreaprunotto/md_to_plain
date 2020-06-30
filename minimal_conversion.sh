#!/usr/bin/env bash

here=/home/prunotto/IMBI/git_projects/md_to_plain
pandoc $here/testdoc.md -o $here/testdoc.txt
pandoc -r markdown -w "docx" -o $here/testdoc.docx $here/testdoc.md
