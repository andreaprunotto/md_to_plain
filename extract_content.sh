#!/usr/bin/env bash

readonly mimetype=$1
readonly filename=$2

# get basename without extension
function get_basename() {
  local s=${1##*/}

  echo "${s%.*}"
}

readonly DIR_TMP="./"
readonly fbname=$(get_basename ${filename})

case "${mimetype}" in
  "text/rtf") 
    libreoffice --headless --convert-to docx --outdir ${DIR_TMP}/ ${filename} > /dev/null
    result=$(cat /tmp/${fbname}.docx | pandoc --from=docx --to=markdown)
    rm -f ${DIR_TMP}/${fbname}.docx
    ;;
  "application/pdf") 
    pdf2docx ${filename} ${DIR_TMP}/${fbname}.docx > /dev/null
    result=$(cat /tmp/${fbname}.docx | pandoc --from=docx --to=markdown)
    rm -f ${DIR_TMP}/${fbname}.docx
    ;;
  *) 
    result="extraction for ${mimetype} is not yet implemented"
    ;;
esac

echo -e "${result}"