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
    if [ ! -f ./hw6_"$i".ml ]; then
      echo "File not found!"
      break
    else
      ocamlc hw6_"$i".ml
      ocaml commonGrade.cmo hw6_"$i".cmo hw6_"$i"_selfgrader.ml
      rm -rf hw6_"$i".cm{o,i} 
    fi
  done
fi
