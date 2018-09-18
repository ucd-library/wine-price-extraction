#! /bin/bash

CDIR=$(pwd)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
cd $DIR

CMD=create


create() {
  id=$1
  page=$2
  timestamp=$(date +%s)

  template=`cat ../template.qlr`
  template=$(echo $template | sed "s/\[\[id\]\]/$id/g")
  template=$(echo $template | sed "s/\[\[page\]\]/$page/g")
  template=$(echo $template | sed "s/\[\[timestamp\]\]/$timestamp/g")
  
  echo $id
  folder="$CDIR/$id-$page"
  if [ -d "$folder" ]; then
    rm -rf $folder
  fi
  mkdir -p $folder

  cd $folder
  echo $template > $id-$page.qlr
  wget -O $id-$page.jpg https://digital.ucdavis.edu/fcrepo/rest/collection/sherry-lehmann/catalogs/$id/media/images/$id-$page
}

delete() {
  echo 'not implemented'
}

if [ $CMD = 'create' ]; then
  create $1 $2
elif [ $CMD = 'delete' ]; then
  create bar
fi
