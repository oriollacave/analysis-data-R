#!/bin/bash
#input files to analyse
obsdir=$1
obsfile=$2
datadir=$3
datafile=$4
sitename=$5
lat=$6
lon=$7
run=$8

bindir="/home/oriol/SRC/solar-bin/"

var1=`ncdump -h $datadir/$datafile | grep float |  awk '{print $2}' | awk -F "(" '{print $1}' | grep -v lat | grep -v lon`
var2=`ncdump -h $obsdir/$obsfile | grep float | awk '{print $2}' | awk -F "(" '{print $1}' | grep -v lat | grep -v lon`
echo "DATAFILE=$datafile"
cp $obsdir/$obsfile input2.nc
#cp $datadir/$datafile input1.nc
if [ "$datafile" == "sarah.ghi.nc" ] || [ "$datafile" == "goes.ghi.nc" ] || [ "$datafile" == "genhem.ghi.nc" ] || [ "$datafile" == "gedisk.ghi.nc" ]; then
    cdo selvar,GHI_met $datadir/$datafile input1.nc
    var1="GHI_met"
fi
if [ "$datafile" == "best.ghi.nc" ] || [ "$datafile" == "remod.met.ghi.nc" ]; then
    cdo selvar,GHI $datadir/$datafile input1.nc
    var1="GHI"
fi
if [ "$datafile" == "sarah.DISC.nc" ] || [ "$datafile" == "goes.DISC.nc" ] || [ "$datafile" == "best.DISC.nc" ]; then
    cdo selvar,DNI_syn $datadir/$datafile input1.nc
    var1="DNI_syn"
fi
if [ "$datafile" == "sarah.RUIZ.nc" ] || [ "$datafile" == "goes.RUIZ.nc" ] || [ "$datafile" == "best.RUIZ.nc" ]; then
    cdo selvar,DNI_syn $datadir/$datafile input1.nc
    var1="DNI_syn"
fi
if [ "$datafile" == "remod.RUIZ.nc" ] || [ "$datafile" == "remod.DISC.nc" ] ; then
    cdo selvar,DNI_syn $datadir/$datafile input1.nc
    var1="DNI_syn"
fi
if [ "$datafile" == "merra2.ghi.nc" ] ; then
    cdo selvar,GHI_met $datadir/$datafile input1.nc
    var1="GHI_met"
fi
if [ "$datafile" == "meteosat.east.nc" ] ; then
    cdo selvar,GHI_met $datadir/$datafile input1.nc
    var1="GHI_met"
fi
if [ "$datafile" == "serie.d02.nc" ] ; then
    cdo sellevel,0 $datadir/$datafile input1.tmp.nc
    cdo selvar,SWDOWNC input1.tmp.nc input1.nc
    rm input1.tmp.nc
    var1="SWDOWNC"
fi


#var2=`echo $obsfile | awk -F . '{print toupper($2)}'`'_real'	
echo "VAR1:"$var1" VAR2:"$var2
#TO VALIDATE SOLAR SERIES aggr=sum, TO VALIDATE WIND SERIES aggr=mean
aggr=sum
#TO CALCULATE ERROR METRICS IN NATURAL UNITS, NORMALIZED OR COEFFICIENT OF VARIANCE:
norm=cv
echo "R --slave --args input1.nc input2.nc $var1 $var2 $aggr $norm < /home/oriol/SRC/solar-bin/validation.R"
R --slave --args input1.nc input2.nc $var1 $var2 $aggr $norm < /home/oriol/SRC/solar-bin/validation.R
#echo "./graph.R input2.nc $var2  input1.nc $var1 $sitename $infile $indir"
#Rscript ./graph.R input2.nc $var2  input1.nc $var1 $sitename $infile $indir

#echo "R --no-save --args input1.nc input2.nc $var1 $var1 sum cv $run sarahVSgenhem < graphs-modelvsmodel.R"
#R --no-save --args input1.nc input2.nc $var1 $var1 sum cv $sitename $run < graphs-v2.R
#mkdir -p plots/$sitename
#mv ./*.pdf plots/$sitename/

metric=`cat ValRprt2_$aggr-$norm.txt | awk '(NR==3){print $1}'`
sitename=`echo $obsdir | awk -F '/' '{print $(NF-1)}'`
echo $sitename";"$lat";"$lon";"$run";"$datafile";"$metric > kk.valreport.txt
cat ValRprt2_$aggr-$norm.txt | awk '(NR==1){print $1}' > head1.txt
cat ValRprt2_$aggr-$norm.txt | awk '(NR==2){print $1}' > head2.txt
rm -f input1.nc input2.nc ValRprt1_$aggr-$norm.txt  ValRprt2_$aggr-$norm.txt  ValRprt3_$aggr-$norm.txt
#pdfunite ${indir}/*.pdf ${indir}/${sitename}.pdfa
#rm ${indir}/*.pdf
cat head1.txt head2.txt kk.valreport.txt > heads.txt
cat kk.valreport.txt >> validation-all.txt
rm -f head1.txt head2.txt kk.valreport.txt
