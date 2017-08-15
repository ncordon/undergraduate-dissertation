#!/bin/bash

MAN_FOLDER=./imbalance/man
TFG_FOLDER=./memoria/chapters
MAN_FILES="mwmote racog wracog rwo pdfos neater plotComparison"

echo "" > ${TFG_FOLDER}/manual.tex

for file in ${MAN_FILES[@]};
do
    R CMD Rdconv --type=latex ${MAN_FOLDER}/${file}.Rd >> ${TFG_FOLDER}/manual.tex
done;
