#! /usr/bin/env bash

declare -A WP=(
  [noop]=''
  [http_ssl]="tls1.2"
  [http_print]="b"
  [shoulder]=ark:/87287
  [dams]=https://digital.ucdavis.edu
  [script_dir]=/opt/dsi/scripts
  [Rscript]='Rscript --vanilla'
);
# Allow getopt to be somewhere else
WP[getopt]=${FLAGS_GETOPT_CMD:-getopt}

function init() {
    local opts=`${WP[getopt]} -o nh --long dry-run,help -n 'wine-price' -- "$@"`
    if [ $? != 0 ] ; then echo "Bad Command Options." >&2 ; exit 1 ; fi

    eval set -- "$opts"

    while true; do
	    case $1 in
        -h | --help ) exec pod2text $0;;
        -n | --dry-run ) WP[noop]=1; shift ;;
	      -- ) shift; break;;
	      *) shift; break;
      esac
    done
}

function log() {
  (>&2 echo LOG: $@)
}

function _http () {
  local stdinp=
  local http="http --follow --check-status --ssl=${WP[http_ssl]} --print=${WP[http_print]}"
  if [[ $1 == '-n' || -n ${WP[noop]} ]]; then
    [[ $1 == '-n' ]] && shift
    if [[ ! -t 0 ]]; then
      IFS='' read -r -d '' stdinp;
      log "$http $@ <<<$(printf "%q" "$stdinp")"
    else
      log "$http $@"
    fi
  else
    if [[ ! -t 0 ]]; then
      IFS='' read -r -d '' stdinp;
      $http $@  <<<"$stdinp"
    else
      $http $@
    fi
  fi
}


function pages() {
  local i ark page s R;
  local jpg bRDS dRDS;
  s=${WP[script_dir]}
  R=${WP[Rscript]}
  for i in $@; do
    b=$(basename $i);
    ark=${b%-*[0-9]};
    jpg=$i/$b.jpg
    bRDS=$i/${b}_data1.RDS
    dRDS=$i/$b.RDS
    parsed=$i/parsed_folder.RDS
    price=$i/PRICE_NAME.csv
    [[ -d $i ]] || mkdir -p $i;
      if [[ ! -f $jpg ]]; then
        echo _http --output=$jpg ${WP[dams]}/${WP[shoulder]}/$ark/media/images/$b.jpg;
        _http --output=$jpg ${WP[dams]}/${WP[shoulder]}/$ark/media/images/$b.jpg;
      fi
    if [[ ! -f $bRDS ]]; then
      # Only fetch jpg if we need the _data1.RDS file.
      if [[ ! -f $jpg ]]; then
        _http --output=$jpg ${WP[dams]}/${WP[shoulder]}/$ark/media/images/$i.jpg;
      fi
      $R $s/run_wine_price_tables.R FILESET=$jpg DATA.OUTPUT.DIR=$i OCR.ONLY='true'
    fi
    if [[ -f $dRDS ]]; then
      echo $dRDS exists ;
    else
      $R $s/run_wine_price_tables.R FILESET=$i OUTPUT.DIR=$i DATA.INPUT.DIR=$i
    fi
    if [[ -f $parsed ]]; then
      echo $parsed exists;
    else
      $R $s/run_parse_items.R name.input.dir=$i name.output.dir=$i;
    fi
    if [[ -f $price ]]; then
      echo $price exists;
    else
      $R $s/run_wine_database_one_page.R truth.dir=/opt/dsi/Data in=$parsed
    fi
  done
}

while true; do
	case $1 in
	  -*) OPTS=($1); shift ;;
	  -- ) shift; break;;
	  *) break;
	esac
done

init $OPTS

pages "$@"

exit 0;
