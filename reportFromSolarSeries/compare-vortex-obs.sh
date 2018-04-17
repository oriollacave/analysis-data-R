#!/bin/bash

### EXAMPLE

#./compare-vortex-obs.sh agriplam ghi ../obs-utc/agriplam-vortex-utc.csv obs measurements ../points/agriplam/vortex_solar_series_82373-20yUTC+0GHI-agriplam-agriplam.txt serie remodserie ../points/agriplam/vortex.tmy.p50-agriplam-agriplam.csv tmy remod ../points/agriplam/vortex.tmy.p50.orig.csv tmy original
#ARGS
myArray=("$@") 
args=""
for arg in "${myArray[@]}"; do
	echo $arg
    if [ ! -z "$arg" ];then
   args=${args}" "${arg}""
   fi
done
#DO R
#R --no-save < compare-vortex-obs.R --no-save
if [ ! -d output-txt ];then mkdir output-txt;fi
if [ ! -d output-pdf ];then mkdir output-pdf;fi
if [ ! -d tmp ];then mkdir tmp;fi
echo "R --vanila --slave --quiet --no-restore --args ${args} < info4points.R "
R --vanilla --slave --quiet --no-restore  --no-save  --args ${args} < /home/oriol/SRC/validateAndgraph/info4points.R 
#CLEAN
