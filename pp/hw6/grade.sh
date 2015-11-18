#!/bin/bash
if [ $# -lt 1 ]; then
  echo 1>&2 "$0: not enough arguments"
elif [ $1 -lt 1 ]; then
  echo 1>&2 "$0: homework starts from 1"
else
  if [[ $# -eq 2 ]]; then
    x=$2
  else
    x=$1
  fi
  ocamlc commonGrade.ml
  for ((i=$1;i<=x;i++))
  do 
    if [ ! -f ./hw*_"$i".ml ]; then
      echo "File not found!"
    else
      ocamlc hw*_"$i".ml
      ocaml commonGrade.cmo hw*_"$i".cmo hw*_"$i"_selfgrader.ml
      rm -rf hw*_"$i".cm{o,i} 
    fi
  done
fi
