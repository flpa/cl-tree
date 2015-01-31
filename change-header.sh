#! /bin/sh
for f in `ls *.asd *.lisp`; do sed '1,/^$/d' $f > tmp; cat .fileheader tmp > $f; done
rm tmp
