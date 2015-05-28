#!/bin/bash

INSTDIR=`mktexlsr 2>&1 | sed 's/Updating //' | grep -e -local | awk '{print $2}' | sed 's/://' | sed 'st/ls-R...tt' | awk '{printf("%s/tex/latex/",$1)}'`

if [ ! -d $INSTDIR ]
 then
  INSTDIR=`mktexlsr 2>&1 | grep -e -dist | awk '{print $2}' | sed 's/://' | awk '{printf("%s/tex/latex/",$1)}'`
  if [ ! -d $INSTDIR ]
   then
    echo "Error: Installation target directory not found."
    exit 1
  fi
fi

if [ ! -d breqn097a ]
 then
  echo "Error: breqn097a not found in this directory."
  exit 1
fi

echo "Installing breqn097a to" $INSTDIR
/bin/cp -r breqn097a $INSTDIR
texhash

kpsewhich breqn.sty
