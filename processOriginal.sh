#!/bin/bash


mkdir -p work/test
mkdir -p work/train
mkdir -p work/validation


for f in `ls work/original/*.txt`; do
	pr=`basename $f`

	echo "Processing $f"

	cat $f | tr "" \\n > $pr

	split -n l/10 $pr ${pr}__

	cat ${pr}__a[a-f] > work/train/$pr
	cat ${pr}__a[g-i] > work/test/$pr
	cat ${pr}__aj > work/validation/$pr

	rm ${pr}*
done


