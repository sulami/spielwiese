#!/bin/bash

## AutoBackup
# Depencies: bash tar gzip

## Modification
# Note: always specify existing folders
input="/home/sulami/sulami-spielwiese/"
output="/home/sulami/"


## Runtime
# Compress and save
echo -e "AutoBackup\nBeginning backup..."
cd $input
filename=$(date +%Y-%m-%d-%H-%M).tar.gz
tar cfz $filename *
mv $filename $output
cd $output
if [[ -e $filename ]] ; then
	echo -e "Backup '${filename}' saved"
	exit 0
else
	echo -e "Backup has NOT been saved correctly, please visit ${output} and see for yourself"
	exit 1
fi
